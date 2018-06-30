{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.Core.Data
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.Core.Data (
  -- * Types
    EcologyDigests(..)
  , EcologyDigestStore(..)
  , EcologyEnvironmentVariableUpdate(..)
  , EcologyReport(..)
  , EcologyConfigReport(..)
  , EcologyGitReport(..)
  , EcologyHashMap(..)
  , EcologyParameters(..)
  , EcologyPrivacy(..)
  , EcologyProjectReport(..)
  , EcologyPermission(..)
  , EcologyProject(..)
  , EcologyProjectCI(..)
  , EcologyProjectName(..)
  , EcologyProjectDescription(..)
  , EcologyTag(..)
  , EcologyEnvironmentPair(..)
  , EnvironmentVariableName(..)
  , EnvironmentVariableValue(..)
  , GitTemplateRepo(..)
  , GitRepository(..)
  , NewCIInfo(..)
  , NewGitRepository(..)
  , BranchRestriction(..)
  , PushRestrictions(..)
  , RequiredStatusChecks(..)
  , TeamPermission(..)
  , TeamName(..)
  , Username(..)
  -- * Functions
  , ecologyPermissionText
  , ecologyPrivacyText
  , newGitRepository
  , permissionValues
  , privacyValues
  , renderEcologyReport
  ) where

import Ultra.Data.Foldable (filteredBy)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.HashMap.Strict as H

import Data.Hashable (Hashable(..))
import Data.List (reverse)

import Preamble

data GitTemplateRepo = GitTemplateRepo {
    gitTemplateRepoURL  :: !T.Text
  , gitTemplatePrivacy  :: !EcologyPrivacy
  } deriving (Show, Eq)

data GitRepository = GitRepository {
    gitRepoName :: !T.Text
  , gitRepoURL  :: !T.Text
  } deriving (Show, Eq)

-- Information that will be passed from
-- Ecology to the API to create a New Git Repository
-- Its currently missing team permissions
data NewGitRepository b = NewGitRepository {
    newRepoName         :: !EcologyProjectName
  , newRepoDescription  :: !EcologyProjectDescription
  , newRepoLanguage     :: !T.Text
  , newRepoPrivacy      :: !EcologyPrivacy
  , newRepoCategory     :: !(Maybe b)
  , newRepoTags         :: ![EcologyTag]
  } deriving (Show, Eq)

-- BEGIN: CI stuff

data NewCIInfo a = NewCIInfo {
    nciProjectName :: !EcologyProjectName
  , nciProjectType :: !a
  , nciEnvironment :: ![EcologyEnvironmentPair]
  } deriving (Show, Eq)

-- END

data EcologyReport g i a b c = EcologyReport {
    ecologyGitReport :: !(EcologyGitReport g i a b c)
  , ecologyConfigReport :: !(EcologyConfigReport i)
  } deriving (Show, Eq)

tell' :: a -> State [a] ()
tell' x = gets (x :) >>= put

renderEnvironmentVariableChanges
  :: (Foldable t)
  => EcologyProjectName
  -> T.Text
  -> t EcologyEnvironmentVariableUpdate
  -> State [T.Text] ()
renderEnvironmentVariableChanges proj info changes = do
  tell' $ "  " <> ecologyProjectNameText proj <> "(" <> info <> ")" <> ":"
  let (news, updates, deletes) = partitionEnvironmentVariableChanges . toList $ changes
  tell' "    New CI Environment Variables:"
  for_ news $ \(nm, newHash) ->
    tell' . T.concat $ ["      - ", envVarName nm, " (hash: ", newHash, ")"]
  tell' "    Updated CI Environment Variables:"
  for_ updates $ \(nm, oldHash, newHash) ->
    tell' . T.concat $ ["      - ", envVarName nm, " (hash: ", oldHash, " -> ", newHash, " )"]
  tell' "    Deleted CI Environment Variables (Ecology cannot at the moment handle any of the listed deletions that resulted from deleting an entire project, any such thing must be cleaned up by hand):"
  for_ deletes $ \nm ->
    tell' . T.concat $ ["      - ", envVarName nm]

renderEcologyReport
  :: (i -> T.Text)
  -> EcologyReport g i a b c
  -> [T.Text]
renderEcologyReport renderCIType report =
  let
    newFilter
      :: EcologyProjectReport g i a b c
      -> Maybe (EcologyProject g i a b c)
    newFilter (EcologyProjectReport project gitRepo) =
      maybe (pure project) (const Nothing) gitRepo

  -- Use State briefly here to compose the "report" in a slightly declarative style...
  in reverse . snd . flip runState [] $ do
    tell' "New projects:"
    for_ (filteredBy newFilter . ecologyGitReportProjects . ecologyGitReport $ report) $ \proj ->
      tell' . T.concat $ [
          "  "
        , ecologyProjectNameText . ecologyProjectName $ proj
        , ": "
        , ecologyProjectDescriptionText . ecologyProjectDescription $ proj
        , T.bracketedList " (By: " ")" "," . fmap usernameText . experts $ proj
        ]
    tell' "CI Config Changes:"
    for_ (ecologyParamChanges . ecologyConfigReport $ report) $ \(proj, plat, changes) ->
      renderEnvironmentVariableChanges proj (renderCIType plat) changes
    for_ (nonEmpty . ecologyUnactionableParamChanges . ecologyConfigReport $ report) $ \unactionableChanges -> do
      tell' "Unactionable CI Config Changes:"
      for_ unactionableChanges $ \(proj, reason, changes) ->
        renderEnvironmentVariableChanges proj reason changes

data EcologyEnvironmentVariableUpdate =
    EcologyEnvironmentVariableUpdate !EnvironmentVariableName !T.Text !T.Text !T.Text -- ^ Old hash, new hash, new value
  | EcologyEnvironmentVariableNew !EnvironmentVariableName !T.Text !T.Text -- ^ Hash, Value
  | EcologyEnvironmentVariableDelete !EnvironmentVariableName -- ^ for environment variables that no longer exist
    deriving (Show, Eq)

-- |
-- This is for the report generated for user output
-- hence it will not contain potentially sensitive output like the
-- actual values of the environment variables.
partitionEnvironmentVariableChanges
  :: (Foldable t)
  => t EcologyEnvironmentVariableUpdate
  -> ([(EnvironmentVariableName, T.Text)], [(EnvironmentVariableName, T.Text, T.Text)], [EnvironmentVariableName])
partitionEnvironmentVariableChanges =
  let
    partitionEnvironmentVariableChanges' change (news, updates, deletes) =
      case change of
        EcologyEnvironmentVariableUpdate nm oldHash newHash _ -> (news, (nm, oldHash, newHash) : updates, deletes)
        EcologyEnvironmentVariableNew nm newHash _ -> ((nm, newHash) : news, updates, deletes)
        EcologyEnvironmentVariableDelete varName -> (news, updates, varName : deletes)
  in foldr partitionEnvironmentVariableChanges' ([], [], [])

data EcologyConfigReport i = EcologyConfigReport {
    ecologyConfigParams             :: !EcologyParameters
  , ecologyOldHashes                :: !EcologyDigestStore
  , ecologyNewParamHashes           :: ![(EcologyProjectName, EcologyHashMap)] -- ^ New Hashes, only for the parameters, doesn't include the new CI hashes though.
  , ecologyParamChanges             :: ![(EcologyProjectName, i, NonEmpty EcologyEnvironmentVariableUpdate)]
  , ecologyUnactionableParamChanges :: ![(EcologyProjectName, T.Text, NonEmpty EcologyEnvironmentVariableUpdate)]
  } deriving (Show, Eq)

data EcologyProjectReport g i a b c = EcologyProjectReport {
    ecologyProjectReportProject     :: !(EcologyProject g i a b c)
  , ecologyProjectReportGitRepo     :: !(Maybe GitRepository)
  } deriving (Show, Eq)

data EcologyGitReport g i a b c = EcologyGitReport {
    ecologyGitReportProjects  :: ![EcologyProjectReport g i a b c]
  , ecologyGitReportUnmanaged :: ![(g, NonEmpty GitRepository)]
  } deriving (Show, Eq)

newtype EcologyProjectName = EcologyProjectName {
    ecologyProjectNameText :: T.Text
  } deriving (Hashable, Show, Eq, Ord)

newtype EcologyProjectDescription = EcologyProjectDescription {
    ecologyProjectDescriptionText :: T.Text
  } deriving (Show, Eq)

newtype EcologyParameters = EcologyParameters {
    paramMap :: H.HashMap T.Text T.Text
  } deriving (Show, Eq)

newtype EcologyDigestStore = EcologyDigestStore {
    digestStore :: H.HashMap EcologyProjectName EcologyDigests
  } deriving (Show, Eq)

newtype EcologyHashMap = EcologyHashMap {
    hashes :: H.HashMap T.Text T.Text
  } deriving (Show, Eq)

data EcologyDigests = EcologyDigests {
    ciParamMap :: EcologyHashMap  -- CI Parameters, used by 'users'
  , ciOtherMap :: EcologyHashMap  -- Other stateful hashes decided by the CI API implementations
  } deriving (Show, Eq)

newtype EcologyTag = EcologyTag {
    ecologyTagText :: T.Text
  } deriving (Show, Eq)


newtype TeamName = TeamName {
    teamNameText :: T.Text
  } deriving (Show, Eq)

data EcologyPermission =
    EcologyPermissionPush
  | EcologyPermissionPull
  | EcologyPermissionAdmin
    deriving (Show, Eq)

permissionValues :: NonEmpty EcologyPermission
permissionValues = [
    EcologyPermissionPush
  , EcologyPermissionPull
  , EcologyPermissionAdmin
  ]

ecologyPermissionText
  :: EcologyPermission
  -> T.Text
ecologyPermissionText EcologyPermissionPush  = "push"
ecologyPermissionText EcologyPermissionPull  = "pull"
ecologyPermissionText EcologyPermissionAdmin = "admin"

data TeamPermission = TeamPermission {
    teamPName       :: !TeamName
  , teamPermission  :: !(Maybe EcologyPermission)
  } deriving (Show, Eq)

newtype EnvironmentVariableName = EnvironmentVariableName {
    envVarName :: T.Text
  } deriving (Show, Eq)

data EnvironmentVariableValue =
    EnvironmentVariableValue !T.Text
  | EnvironmentVariableReferenceValue !T.Text -- * References a parameter in the AWS parameter store
    deriving (Show, Eq)

data EcologyEnvironmentPair =
  EcologyEnvironmentPair !EnvironmentVariableName !EnvironmentVariableValue
  deriving (Show, Eq)

data EcologyProjectCI i = EcologyProjectCI {
    ciType            :: !i
  , ciEnvironmentVars :: ![EcologyEnvironmentPair]
  } deriving (Show, Eq)

data RequiredStatusChecks = RequiredStatusChecks {
    enforcementLevel  :: !EnforcementLevel
  , isStrict          :: !Bool
  , context           :: ![T.Text]
  } deriving (Show, Eq)

data EnforcementLevel =
    Everyone
  | NotAdmins
    deriving (Show, Eq)

newtype Username = Username {
    usernameText :: T.Text
  } deriving (Show, Eq)

data PushRestrictions =
  PushRestrictions ![Username] ![TeamName]
  deriving (Show, Eq)

data BranchRestriction = BranchRestriction {
    requiredStatusChecks  :: !(Maybe RequiredStatusChecks)
  , pushRestrictions      :: !(Maybe PushRestrictions)
  } deriving (Show, Eq)

data EcologyPrivacy =
    EcologyPublic
  | EcologyPrivate
    deriving (Show, Eq)

ecologyPrivacyText
  :: EcologyPrivacy
  -> T.Text
ecologyPrivacyText EcologyPublic  = "public"
ecologyPrivacyText EcologyPrivate = "private"

privacyValues :: NonEmpty EcologyPrivacy
privacyValues = [EcologyPublic, EcologyPrivate]

newGitRepository :: (a -> T.Text) -> EcologyProject g i a b c -> NewGitRepository b
newGitRepository renderType (EcologyProject name' _ desc typ category _ _ _ priv tags' _) = NewGitRepository
  name'
  desc
  (renderType typ)
  priv
  category
  tags'

data EcologyProject g i a b c = EcologyProject {
    ecologyProjectName          :: !EcologyProjectName
  , ecologyProjectLocation      :: !g
  , ecologyProjectDescription   :: !EcologyProjectDescription
  , ecologyProjectType          :: !a
  , ecologyProjectCategory      :: !(Maybe b)
  , ecologyProjectStatus        :: !c
  , teams                       :: ![TeamPermission]
  , ci                          :: !(EcologyProjectCI i)
  , ecologyPrivacy              :: !EcologyPrivacy
  , tags                        :: ![EcologyTag]
  , experts                     :: ![Username]
  } deriving (Show, Eq)
