{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.IO.Sync
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.IO.Sync (
  -- * Types
    EcologySyncError(..)
  -- * Functions
  , ecologySync
  , renderEcologySyncError
  ) where

import Irreverent.Ecology.IO.Common
import Irreverent.Ecology.IO.Report

import Irreverent.Ecology.API (
    CIAPI(..)
  , CIAPIs(..)
  , GitPlatformAPI(..)
  , GitPlatformAPIs(..)
  , IMAPI(..)
  )
import Irreverent.Ecology.Core.Data (
    EcologyDigests(..)
  , EcologyDigestStore(..)
  , EcologyHashMap(..)
  , EcologyParameters(..)
  , EcologyProject(..)
  , EcologyProjectCI(..)
  , EcologyProjectName(..)
  , EcologyReport(..)
  , EcologyConfigReport(..)
  , EcologyGitReport(..)
  , EcologyProjectReport(..)
  , GitRepository(..)
  , GitTemplateRepo(..)
  , NewCIInfo(..)
  , ecologyProjectDescriptionText
  , ecologyProjectNameText
  , newGitRepository
  )
import Irreverent.Ecology.Json.Data (
    ecologyDigestStoreLatestJson
  )

import Shush (
    SyncShellError(..)
  , ShellCommandArg(..)
  , ShellCommand(..)
  , renderSyncShellError
  , runShT
  , runSync
  , withCwd
  )
import Shush.Data.Environment (
    ChildEnvironment
  , (.::)
  , importEnvVar
  )

import Ultra.Control.Lens ((&), (.~))
import Ultra.Control.Monad.Bracket (MonadBracket)
import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (
    EitherT
  , pattern EitherT
  , firstEitherT
  , mapEitherT
  , runEitherT
  , unifyEitherT
  )
import qualified Ultra.Data.Aeson as A
import Ultra.Data.Foldable (filteredBy)
import qualified Ultra.Data.HashMap.Strict as H
import Ultra.Data.List (ordNub)
import qualified Ultra.Data.Text as T
import Ultra.System.Verified.IO (
    WithTempDirError(..)
  , DirVerificationFail(..)
  , existsAsDir
  , renderDirVerificationFail
  , renderWithTempDirError
  , verifiedDirPath
  , withTempDir
  )

import Control.Monad.Trans.Resource (runResourceT)

import Data.List (unzip3)
import Data.Monoid ((<>))
import NeatInterpolation (text)

import Network.AWS (
    Credentials(..)
  , Env
  , newEnv
  , runAWS
  , send
  )
import Network.AWS.Data.Body (toBody)
import Network.AWS.S3 (ServerSideEncryption(..))
import Network.AWS.S3.PutObject (
    PutObject
  , putObject
  , poServerSideEncryption
  )
import Network.AWS.S3.Types (
    BucketName(..)
  , ObjectKey(..)
  )

import Preamble hiding ((<>))

data EcologySyncError ge ce ie =
    EcologySyncReportError !(EcologyReportError ge)
  | EcologySyncGitError !ge
  | EcologySyncCIError !ce
  | EcologySyncIMError !ie
  | EcologySyncNoDirError !DirVerificationFail
  | EcologySyncShellError !SyncShellError
  | EcologySyncTempDirError !(WithTempDirError (EcologySyncError ge ce ie) ())
  | EcologySyncMissingEnvironmentVariables !(NonEmpty T.Text)
  | EcologySyncAWSParameterError
  | EcologySyncJsonError !T.Text !T.Text
  | EcologySyncHttpError !T.Text
    deriving (Show, Eq)

renderEcologySyncError
  :: (ge -> T.Text)
  -> (ce -> T.Text)
  -> (ie -> T.Text)
  -> EcologySyncError ge ce ie
  -> T.Text
renderEcologySyncError f _ _ (EcologySyncReportError e)   = renderEcologyReportError f e
renderEcologySyncError f _ _ (EcologySyncGitError e)      = f e
renderEcologySyncError _ f _ (EcologySyncCIError e)       = f e
renderEcologySyncError _ _ f (EcologySyncIMError e)       = f e
renderEcologySyncError _ _ _ (EcologySyncNoDirError e)    = renderDirVerificationFail e
renderEcologySyncError _ _ _ (EcologySyncShellError e)    = renderSyncShellError e
renderEcologySyncError f g h (EcologySyncTempDirError e)  = renderWithTempDirError (renderEcologySyncError f g h) (const "") e
renderEcologySyncError _ _ _ EcologySyncAWSParameterError = "Name or Value missing from AWS SSM response (This shouldn't happen)"
renderEcologySyncError _ _ _ (EcologySyncMissingEnvironmentVariables names) = T.bracketedList "Environment Variables missing on SSM: [" "]" ", " . toList $ names
renderEcologySyncError _ _ _ (EcologySyncJsonError desc t) = T.concat ["Json Parse Error while parsing ", desc, ": '", t, "'"]
renderEcologySyncError _ _ _ (EcologySyncHttpError e)     = T.concat ["HTTP Error: ", e]

data EcologyGitCIError = EcologyGitCIError deriving (Show, Eq)

-- BEGIN: TODO Subtasks

newprojects
  :: forall g i a b c. EcologyGitReport g i a b c
  -> [EcologyProject g i a b c]
newprojects =
  let
    filter' :: EcologyProjectReport g i a b c -> Maybe (EcologyProject g i a b c)
    filter' (EcologyProjectReport p gitRepo) = maybe (pure p) (const Nothing) gitRepo
  in filteredBy filter' . ecologyGitReportProjects

createNewProject
  :: (MonadCatch m, MonadBracket m, MonadIO m)
  => GitPlatformAPIs g b m e
  -> CIAPIs a i m ce
  -> EcologyParameters
  -> (a -> Maybe GitTemplateRepo)
  -> (a -> T.Text)
  -> EcologyProject g i a b c
  -> ExceptT (EcologySyncError e ce ie) m (GitRepository, EcologyProjectName, EcologyHashMap)
createNewProject gitAPIs ciAPIs params templates renderType p =
  let
    projectName :: T.Text
    projectName = ecologyProjectNameText . ecologyProjectName $ p
  in do
    logText [text|Creating repository for new project $projectName...|]
    gitRepo <- createRepo gitAPIs ciAPIs params templates renderType p
    logText [text|Setting up CI for new project $projectName...|]
    (name, ciHashes) <- setupNewCI ciAPIs params p
    pure (gitRepo, name, ciHashes)

defaultEnvironment :: ChildEnvironment
defaultEnvironment = importEnvVar "HOME"

gitCmd :: [ShellCommandArg] -> ShellCommand
gitCmd = ShellCommand defaultEnvironment "git"

createRepo
  :: forall a b c e g i ce ie m. (MonadBracket m, MonadCatch m, MonadIO m)
  => GitPlatformAPIs g b m e
  -> CIAPIs a i m ce
  -> EcologyParameters
  -> (a -> Maybe GitTemplateRepo)
  -> (a -> T.Text)
  -> EcologyProject g i a b c
  -> EitherT (EcologySyncError e ce ie) m GitRepository
createRepo apis ciApis params templates renderType p =
  let
    api :: GitPlatformAPI b m e
    api = selectGitAPI apis $ ecologyProjectLocation p

    ciApi :: CIAPI a m ce
    ciApi = selectCIAPI ciApis . ciType . ci $ p
  in do
    newRepo <- firstEitherT EcologySyncGitError $ createNewRepo api (newGitRepository renderType p)
    forM_ (templates . ecologyProjectType $ p) $ \template ->
      unifyEitherT EcologySyncNoDirError . existsAsDir "/tmp" $ \tmp ->
        firstEitherT EcologySyncTempDirError . withTempDir tmp "ecology-sync." $ \fp ->
          EitherT . fmap (join . first EcologySyncShellError) . runShT . withCwd (verifiedDirPath fp) . runEitherT $
            let
              newCIInfo :: NewCIInfo a
              newCIInfo = NewCIInfo
                (ecologyProjectName p)
                (ecologyProjectType p)
                (ciEnvironmentVars . ci $ p)

              cloneTemplateCommand :: ShellCommand
              cloneTemplateCommand =
                ShellCommand defaultEnvironment "git" (RawArg <$> ["clone", gitTemplateRepoURL template, "new-repo"])

              clearGitHistory :: ShellCommand
              clearGitHistory =
                ShellCommand mempty "rm" (RawArg <$> ["-rfv", ".git"])

              initialiseRepo :: ShellCommand
              initialiseRepo =
                gitCmd (RawArg <$> ["init"])

              pushNewRepoCommand :: ShellCommand
              pushNewRepoCommand =
                gitCmd (RawArg <$> ["push", gitRepoURL newRepo, "master"])

              bootstrapEnv :: ChildEnvironment
              bootstrapEnv = ("ECOLOGY_PROJECT_NAME" .:: (ecologyProjectNameText . ecologyProjectName $ p))
                <> ("ECOLOGY_PROJECT_DESCRIPTION" .:: (ecologyProjectDescriptionText . ecologyProjectDescription $ p))

              bootstrapTemplateCommand :: ShellCommand
              bootstrapTemplateCommand =
                ShellCommand bootstrapEnv "sh" (RawArg <$> [
                    "-c"
                  , "bin/bootstrap-template"
                  ])

              cleanUpTemplate :: ShellCommand
              cleanUpTemplate =
                ShellCommand mempty "rm" (RawArg <$> [
                    "-rfv"
                  , "bin/bootstrap-template"
                  , "template-files"
                  ])

              gitAddCommand :: ShellCommand
              gitAddCommand =
                gitCmd (RawArg <$> [
                    "add"
                  , "-v"
                  , "."
                  ])

              gitCommitBootstrap :: ShellCommand
              gitCommitBootstrap =
                gitCmd (RawArg <$> [
                    "commit"
                  , "-m"
                  , "Initial Commit"
                  ])

              gitCommitCI :: T.Text -> ShellCommand
              gitCommitCI msg =
                gitCmd (RawArg <$> [
                    "commit"
                  , "-m"
                  , msg
                  ])
            in do
              lift . runSync $ cloneTemplateCommand
              mapEitherT (withCwd (verifiedDirPath fp <> "/new-repo")) $ do
                lift $ traverse_ runSync [
                    bootstrapTemplateCommand
                  , cleanUpTemplate
                  , clearGitHistory
                  , initialiseRepo
                  , gitAddCommand
                  , gitCommitBootstrap
                  ]
                changes <- mapEitherT lift . firstEitherT EcologySyncCIError $ initialCIInRepoConfig ciApi (verifiedDirPath fp <> "/new-repo") params newCIInfo
                lift . forM_ changes $ \msg ->
                  traverse_ runSync [
                      gitAddCommand
                    , gitCommitCI msg
                    ]
                lift . runSync $ pushNewRepoCommand
    pure newRepo

setupNewCI
  :: forall a b c e i m ce ie g. (MonadIO m)
  => CIAPIs a i m ce
  -> EcologyParameters
  -> EcologyProject g i a b c
  -> EitherT (EcologySyncError e ce ie) m (EcologyProjectName, EcologyHashMap)
setupNewCI ciAPIs params project =
  let
    newCIInfo :: NewCIInfo a
    newCIInfo = NewCIInfo
      (ecologyProjectName project)
      (ecologyProjectType project)
      (ciEnvironmentVars . ci $ project)
  in fmap ((,) (ecologyProjectName project)) . firstEitherT EcologySyncCIError $ ciRemoteSetup
    (selectCIAPI ciAPIs . ciType . ci $ project)
    params
    newCIInfo

-- The original version that used the S3 library directly:
--retrieveDigestStore
--  :: (MonadIO m)
--  => Env
--  -> BucketName
--  -> ObjectKey
--  -> ExceptT (EcologySyncError e ce ie) m EcologyDigestStore
--retrieveDigestStore env bucket object =
--  let
--    goRequest :: GetObject
--    goRequest = getObject bucket object
--
--    getStatus' :: KA.Error -> Maybe Status
--    getStatus' = (^? KA.httpStatus)
--  in do
--    mResp <- liftIO
--      . runResourceT
--      . runAWS env
--      $ drop404 getStatus' (send goRequest)
--    case mResp of
--      Nothing   -> pure $ EcologyDigestStore H.empty
--      Just resp ->
--        let
--          stream = resp ^. gorsBody . to _streamBody
--        in do
--          chunks <- liftIO . runResourceT $ stream $$+- consume
--          firstEitherT (EcologySyncErrorJsonError "digest store" . T.pack)
--            . hoistEither
--            . fmap ecologyDigestStoreFromJson
--            . A.eitherDecodeStrict
--            $! BS.concat chunks

writeDigestStore
  :: (MonadIO m)
  => Env
  -> BucketName
  -> ObjectKey
  -> EcologyDigestStore
  -> ExceptT (EcologySyncError e ce ie) m ()
writeDigestStore env bucket key store =
  let
    body = toBody
      . A.encode
      . ecologyDigestStoreLatestJson
      $ store
    poRequest :: PutObject
    poRequest = putObject bucket key body
      & poServerSideEncryption .~ pure AES256

  in liftIO . runResourceT . runAWS env $
    -- Just overwrites what was previously there,
    -- probably want to have versioning turned on for the bucket.
    void $ send poRequest

-- END

--getConfigParams env paramPath =
--  let
--    getConfigParams'
--      :: Maybe T.Text
--      -> AWS [Parameter]
--    getConfigParams' token = do
--      awsParams <- send $
--        getParametersByPath paramPath
--          & (gpbpWithDecryption .~ (pure True))
--          . (gpbpNextToken .~ token)
--      let params = awsParams ^. gpbprsParameters
--      maybe
--        (pure params)
--        (\nextToken -> (params <>) <$> getConfigParams' (pure nextToken))
--        (awsParams ^. gpbprsNextToken)
--  in liftIO . runResourceT . runAWS env $
--    getConfigParams' Nothing

combineHashes
  :: [(EcologyProjectName, EcologyHashMap)]
  -> [(EcologyProjectName, EcologyHashMap)]
  -> EcologyDigestStore
combineHashes params ciHashes =
  let
    paramHashMap :: H.HashMap EcologyProjectName EcologyHashMap
    paramHashMap = H.fromList params

    ciHashMap :: H.HashMap EcologyProjectName EcologyHashMap
    ciHashMap = H.fromList ciHashes

    allNames :: [EcologyProjectName]
    allNames = ordNub $ ((<>) `on` fmap fst) params ciHashes
  in EcologyDigestStore
    . H.fromList
    . flip fmap allNames
    . (<*>) (,)
    $ \project ->
      (EcologyDigests `on` H.lookupDefault (EcologyHashMap H.empty) project)
        paramHashMap
        ciHashMap

ecologySync
  :: forall a b c e g i ce ie m. (Ord g, MonadBracket m, MonadCatch m, MonadIO m)
  => GitPlatformAPIs g b m e
  -> CIAPIs a i m ce
  -> IMAPI m ie
  -> T.Text
  -> T.Text
  -> T.Text
  -> (a -> Maybe GitTemplateRepo)
  -> (a -> T.Text)
  -> (i -> T.Text)
  -> [EcologyProject g i a b c]
  -> EitherT (EcologySyncError e ce ie) m [GitRepository]
--ecologySync v gitAPIs ciAuthCfg ciCfg ciAPI imCfg imAPI templates renderType projects = do
ecologySync gitAPIs ciAPIs imAPI ecologyBucket' ecologyStateObject' paramPath templates renderType renderCIType projects =
  let
    ecologyBucket :: BucketName
    ecologyBucket = BucketName ecologyBucket'

    ecologyStateObject :: ObjectKey
    ecologyStateObject = ObjectKey ecologyStateObject'
  in do
    env <- newEnv Discover
    logText "Running report..."
    report <- firstEitherT EcologySyncReportError $
      getEcologyReport Nothing gitAPIs renderCIType ecologyBucket' ecologyStateObject' paramPath projects
    let
      params = ecologyConfigParams . ecologyConfigReport $ report
      newHashes = ecologyNewParamHashes . ecologyConfigReport $ report
    logText "Creating new repositories..."
    newResults <- forM (newprojects . ecologyGitReport $ report) $
      createNewProject gitAPIs ciAPIs params templates renderType
    let
      (newRepos, newNames, newDigests) = unzip3 newResults
      newCIHashes = zip newNames newDigests
    -- Perform Updates
    -- Update CI Configuration
    for_ (ecologyParamChanges . ecologyConfigReport $ report) $ \(nm, ciType', changes) ->
       firstEitherT EcologySyncCIError $ ciParameterUpdate (selectCIAPI ciAPIs ciType') nm changes

    -- END Perform Updates
    logText "Storing the new digests"
    writeDigestStore env ecologyBucket ecologyStateObject $ combineHashes newHashes newCIHashes
    logText "Sending notifications..."
    firstEitherT EcologySyncIMError . ipSyncReport imAPI $ newRepos
    pure newRepos
    --firstEitherT EcologySyncGitCIError .
    --  syncGitCI v $ report
    -- push through any changes to github config, in some cases, CI and IM changes also need to
    -- be pushed to the git repos.. (e.g. slack token changes, travisCI/Bitbucket changes...)
    --firstEitherT EcologySyncGitError .
    --  forM_ (gitProjects v report) $ uncurry (updateRepo v)
    -- TODO: Do a pass and render the reports for new projects?

