{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.IO.Report
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.IO.Report (
  -- * Types
    EcologyReportError(..)
  -- * Functions
  , getEcologyReport
  , gitReport
  , renderEcologyReportError
  ) where

import Irreverent.Ecology.IO.Common

import Irreverent.Ecology.API.Git
import Irreverent.Ecology.Core.Data

import Irreverent.Ecology.Json.Data (
    ecologyDigestStoreFromJson
  )

import Ultra.Control.Applicative (checkPredicate)
import Ultra.Control.Lens ((&), (^.), (.~))
import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (
    EitherT
  , bimapEitherT
  , hoistEither
  , left
  )
import qualified Ultra.Data.Aeson as A
import Ultra.Data.Foldable (filteredBy)
import qualified Ultra.Data.HashMap.Strict as H
import qualified Ultra.Data.HashSet as S
import Ultra.Data.List.NonEmpty (groupBy2)
import Ultra.Network.HTTP.Client (
    HttpException(..)
  , HttpExceptionContent(..)
  , HttpClientResponse(..)
  , Status(..)
  , drop404
  , httpClientResponse
  , responseBody
  , responseStatus
  )
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T

import Network.AWS (
    AWS
  , Credentials(..)
  , Env
  , envAuth
  , envRegion
  , newEnv
  , runAWS
  , send
  )
import Network.AWS.Presign (presignURL)
import Network.AWS.S3.GetObject (
    GetObject
  , getObject
  )
import Network.AWS.S3.Types (
    BucketName(..)
  , ObjectKey(..)
  )
import Network.AWS.SSM.GetParametersByPath (
    getParametersByPath
  , gpbpRecursive
  , gpbpWithDecryption
  , gpbpNextToken
  , gpbprsParameters
  , gpbprsNextToken
  )
import Network.AWS.SSM.Types (
    Parameter
  , pName
  , pValue
  )
import qualified Network.AWS.Types as KA
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S

import Control.Monad.Trans.Resource (runResourceT)

import Crypto.Hash (Digest, SHA3_512, hash)

import Data.Bool (not)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (find, sort)
import Data.Time.Clock (getCurrentTime)
import Data.Validation (Validation(..), toEither)

import Preamble

data EcologyReportError e =
    EcologyReportGitError !e
  | EcologyReportMissingEnvironmentVariables !(NonEmpty T.Text)
  | EcologyReportAWSParameterError
  | EcologyReportJsonError !T.Text !T.Text
  | EcologyReportHttpError !T.Text
    deriving (Show, Eq)

renderEcologyReportError
  :: (e -> T.Text)
  -> EcologyReportError e
  -> T.Text
renderEcologyReportError f (EcologyReportGitError e) = f e
renderEcologyReportError _ (EcologyReportMissingEnvironmentVariables names) = T.bracketedList "Environment Variables missing on SSM: [" "]" ", " . toList $ names
renderEcologyReportError _ EcologyReportAWSParameterError = "Name or Value missing from AWS SSM response (This shouldn't happen)"
renderEcologyReportError _ (EcologyReportJsonError desc t) = T.concat ["Json Parse Error while parsing ", desc, ": '", t, "'"]
renderEcologyReportError _ (EcologyReportHttpError e)     = T.concat ["HTTP Error: ", e]

fetchOrgRepos
  :: forall a b g m e. (Monad m)
  => GitPlatformAPIs g a b m e
  -> [g]
  -> EitherT (EcologyReportError e) m [(g, NonEmpty GitRepository)]
fetchOrgRepos apis platforms =
  fmap (filteredBy (traverse nonEmpty)) . forM platforms $ \platform ->
    let
      api :: GitPlatformAPI a b m e
      api = selectGitAPI apis platform
    in bimapEitherT EcologyReportGitError ((,) platform) $ getOrgRepos api

getObjectText
  :: (MonadIO m)
  => Env
  -> BucketName
  -> ObjectKey
  -> EitherT (EcologyReportError e) m (Maybe T.Text)
getObjectText env bucket object =
  let
    goRequest :: GetObject
    goRequest = getObject bucket object

    getStatus :: HttpException -> Maybe Status
    getStatus (HttpExceptionRequest _ (StatusCodeException resp _)) = pure $ responseStatus resp
    getStatus _                            = Nothing
  in do
    now <- liftIO getCurrentTime
    url <- liftIO $ presignURL (env ^. envAuth) (env ^. envRegion) now (KA.Seconds 5) goRequest
    session <- liftIO S.newSession
    let opts = W.defaults & (W.redirects .~ 10) . (W.checkResponse .~ Nothing)
    mResp <- liftIO . drop404 getStatus . fmap httpClientResponse $
      S.getWith opts session (T.unpack . T.decodeUtf8 $ url)
    forM mResp $ \case
      HttpNotOk r -> left . EcologyReportHttpError . T.pack . show . fmap BSL.toStrict $ r
      HttpOk r -> pure . T.decodeUtf8 . BSL.toStrict . responseBody $ r

hashValues :: T.Text -> T.Text
hashValues =
  let
    hash' :: BS.ByteString -> Digest SHA3_512
    hash' = hash
  in T.pack . show . hash' . T.encodeUtf8

hashesFromValues
  :: H.HashMap T.Text T.Text
  -> EcologyHashMap
hashesFromValues = EcologyHashMap . fmap hashValues

computeParamValues
  :: EcologyParameters
  -> EcologyProject g i a b c
  -> Validation (NonEmpty T.Text) (H.HashMap T.Text T.Text)
computeParamValues (EcologyParameters params) project =
  let
    derefEnvValue :: EnvironmentVariableValue -> Validation (NonEmpty T.Text) T.Text
    derefEnvValue (EnvironmentVariableValue val) = pure val
    derefEnvValue (EnvironmentVariableReferenceValue ref) =
      maybe (Failure . pure $ ref) pure . flip H.lookup params $ ref
  in
    fmap H.fromList . for (ciEnvironmentVars . ci $ project) $ \case
      EcologyEnvironmentPair (EnvironmentVariableName name) value ->
        fmap ((,) name) . derefEnvValue $ value

-- |
-- Retrieves the Digest from S3 and parses it from JSON.
--
-- NOTE: This doesn't directly use amazonka-s3's getObject
-- message due to "ConnectionClosed" HTTP Client errors
-- occuring on our system.
-- It sounds like an http-client problem with the Managers,
-- but doesnt sem to happen with anything except getting an
-- S3 object, that we have seen so far.
-- We workaround it by presigning the url and using http client directly,
-- and using Wreq and Sessions.
retrieveDigestStore
  :: (MonadIO m)
  => Env
  -> BucketName
  -> ObjectKey
  -> ExceptT (EcologyReportError e) m EcologyDigestStore
retrieveDigestStore env bucket object =
  getObjectText env bucket object >>= \case
    Nothing   -> pure $ EcologyDigestStore H.empty
    Just json -> hoistEither
      . bimap (EcologyReportJsonError "digest store" . T.pack) ecologyDigestStoreFromJson
      . A.eitherDecodeStrict
      . T.encodeUtf8
      $ json

getConfigParams
  :: forall m. (MonadIO m)
  => Env
  -> T.Text
  -> m [Parameter]
getConfigParams env paramPath =
  let
    getConfigParams'
      :: Maybe T.Text
      -> AWS [Parameter]
    getConfigParams' token = do
      awsParams <- send $
        getParametersByPath paramPath
          & (gpbpWithDecryption .~ pure True)
          . (gpbpRecursive .~ pure True)
          . (gpbpNextToken .~ token)
      let params = awsParams ^. gpbprsParameters
      maybe
        (pure params)
        (\nextToken -> (params <>) <$> getConfigParams' (pure nextToken))
        (awsParams ^. gpbprsNextToken)
  in liftIO . runResourceT . runAWS env $
    getConfigParams' Nothing

awsToEcologyParameters
  :: Int
  -> [Parameter]
  -> Either (EcologyReportError e) EcologyParameters
awsToEcologyParameters prefixLength awsParams = do
  tups <- forM awsParams (\p ->
    maybe
      (Left EcologyReportAWSParameterError)
      pure
      ((,) <$> (T.drop prefixLength <$> p ^. pName) <*> (p ^. pValue)))
  pure . EcologyParameters . H.fromList $ tups

envChange
  :: H.HashMapChange T.Text T.Text T.Text
  -> EcologyEnvironmentVariableUpdate
envChange (H.HashMapKeyUpdate varName oldHash newHash newVal) =
  EcologyEnvironmentVariableUpdate (EnvironmentVariableName varName) oldHash newHash newVal
envChange (H.HashMapKeyNew varName hash' val) =
  EcologyEnvironmentVariableNew (EnvironmentVariableName varName) hash' val
envChange (H.HashMapKeyDelete varName)  =
  EcologyEnvironmentVariableDelete . EnvironmentVariableName $ varName

getEcologyReport
  :: forall a b c e g i m. (Monad m, MonadCatch m, MonadIO m, Ord g)
  => Maybe Env
  -> GitPlatformAPIs g a b m e
  -> (i -> T.Text)
  -> T.Text
  -> T.Text
  -> T.Text
  -> [EcologyProject g i a b c]
  -> EitherT (EcologyReportError e) m (EcologyReport g i a b c)
getEcologyReport mEnv apis renderCIType ecologyBucket' ecologyStateObject' paramPath projects =
  let
    ecologyBucket :: BucketName
    ecologyBucket = BucketName ecologyBucket'

    ecologyStateObject :: ObjectKey
    ecologyStateObject = ObjectKey ecologyStateObject'

    paramPrefix :: T.Text
    paramPrefix = paramPath <> "/parameters/"

    partitionConfigChanges
      :: (EcologyProjectName, z)
      -> ([(EcologyProjectName, i, z)], [(EcologyProjectName, T.Text, z)])
      -> ([(EcologyProjectName, i, z)], [(EcologyProjectName, T.Text, z)])
    partitionConfigChanges (nm, x) (actionable, unactionable) =
      case find ((nm ==) . ecologyProjectName) projects of
        Nothing    -> (actionable, (nm, "Do not know CI platform where config resides (To be implemented)", x) : unactionable)
        Just proj  -> ((nm, ciType . ci $ proj, x) : actionable, unactionable)
  in do
    logText "Running report on Git Repos..."
    gitReport' <- gitReport apis projects
    env <- maybe (newEnv Discover) pure mEnv
    logText . T.concat $ [
        "Fetching the Ecology Parameters under '"
      , paramPrefix
      , "'"
      ]
    awsParams <- getConfigParams env paramPrefix
    params <- hoistEither $ awsToEcologyParameters (T.length paramPrefix) awsParams
    logText "Parameters received."
    logText "Retrieving stored hashes..."
    digests <- retrieveDigestStore env ecologyBucket ecologyStateObject
    logText "Computing new hashes..."
    newValues <- hoistEither
      . toEither
      . first EcologyReportMissingEnvironmentVariables
      . for projects
      $ \project ->
        (,) (ecologyProjectName project) <$> computeParamValues params project
    let
      newHashes = (fmap . second) hashesFromValues newValues
      ciEnvDiff =
          (fmap . second . fmap) envChange
        . H.hashMapDiff2 hashValues (H.toList . fmap (hashes . ciParamMap) . digestStore $ digests)
        $ newValues

      (actionableCIEnvDiff, unactionableCIEnvDiff) = foldr partitionConfigChanges ([], []) ciEnvDiff
      report =
        EcologyReport gitReport' (EcologyConfigReport params digests newHashes actionableCIEnvDiff unactionableCIEnvDiff)
    traverse_ logText $ renderEcologyReport renderCIType report
    pure report

-- |
-- TODO: Test that projects that exist are in the `ecologyGitReportProjects`
-- list and projects that are found that are not in the list of ecology projects are in the
-- `ecologyGitReportUnmanaged` list
gitReport
  :: forall a b c e g i m. (Monad m, Ord g)
  => GitPlatformAPIs g a b m e
  -> [EcologyProject g i a b c]
  -> EitherT (EcologyReportError e) m (EcologyGitReport g i a b c)
gitReport apis projects =
  let
    distinctPlatforms :: [g]
    distinctPlatforms =
      fmap fst . groupBy2 (flip (,) ()) . sort . fmap ecologyProjectLocation $ projects

    projectNames :: S.HashSet T.Text
    projectNames = S.fromList (ecologyProjectNameText . ecologyProjectName <$> projects)

    isUnmanaged :: (g, NonEmpty GitRepository) -> Maybe (g, NonEmpty GitRepository)
    isUnmanaged (platform, rs) =
        fmap ((,) platform)
      . nonEmpty
      . filteredBy (checkPredicate (\r -> not (gitRepoName r `S.member` projectNames)))
      $ rs
  in do
    repos <- fetchOrgRepos apis distinctPlatforms
    let reports = ecologyReportFor repos <$> projects
    let theUnmanaged = filteredBy isUnmanaged repos
    pure $ EcologyGitReport reports theUnmanaged

ecologyReportFor
  :: (Eq g)
  => [(g, NonEmpty GitRepository)]
  -> EcologyProject g i a b c
  -> EcologyProjectReport g i a b c
ecologyReportFor repos project = EcologyProjectReport project $ do
  gitPlatformRepos <- snd <$> find ((== ecologyProjectLocation project) . fst) repos
  find ((== (ecologyProjectNameText . ecologyProjectName $ project)) . gitRepoName) . toList $ gitPlatformRepos
