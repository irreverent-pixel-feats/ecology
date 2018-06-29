{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.Json.Data
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.Json.Data (
  -- * Types
    EcologyJsonParameterCodec(..)
  , EcologyJsonParameterCodecs(..)
  , EcologyDigestStoreJson(..)
  -- * Functions
  , ecologyDigestStoreFromJson
  , ecologyDigestStoreLatestJson
  , ecologyEnvVarValueToJSONV1
  , ecologyEnvVarValueFromJsonV1
  , ecologyEnvironmentPairToJSONV1
  , ecologyEnvironmentPairFromJsonV1
  , ecologyProjectCIToJSONV1
  , ecologyProjectCIFromJsonV1
  , ecologyPermissionToJSONV1
  , ecologyPermissionFromJsonV1
  , ecologyPrivacyToJSONV1
  , ecologyPrivacyFromJsonV1
  , ecologyTeamPermissionToJSONV1
  , ecologyTeamPermissionFromJsonV1
  , ecologyProjectToJsonV1
  , ecologyProjectFromJsonV1
  ) where

import Irreverent.Ecology.Core.Data

import Ultra.Data.Aeson (
    ToJSON(..)
  , FromJSON(..)
  , Object
  , Parser
  , Value
  , (.:)
  , (.=)
  , jsonDispatchOnText
  , jsonTextEnum
  , object
  , parseJSON
  , toJSON
  )
import qualified Ultra.Data.HashMap.Strict as H
import qualified Ultra.Data.Text as T

import Preamble

-- |
-- No JSON codecs have been provided for this
-- as codec fors the parameterised types have to
-- be provided by the user to provide a full versioned
-- JSON document.
-- (ToJSON g, ...) contraints wont suffice here
-- because the codecs need to be aware of the
-- version of the overall document, which Aeson
-- codecs are unaware of.
-- The functions that (de/en)code this type are pretty unwieldy
-- but the increased boilerplate is intended to result in a more coherently versioned
-- JSON parser.
data EcologyProjectJson g i a b c =
  EcologyProjectJsonV1 !(EcologyProject g i a b c)
  deriving (Show, Eq)

data EcologyJsonParameterCodec a = EcologyJsonParameterCodec {
    ecologyToJson     :: a -> Value
  , ecologyParseJson  :: Value -> Parser a
  }

data EcologyJsonParameterCodecs g i a b c = EcologyJsonParameterCodecs {
    ecologyLocationCodec :: !(EcologyJsonParameterCodec g)
  , ecologyCITypeCodec   :: !(EcologyJsonParameterCodec i)
  , ecologyTypeCodec     :: !(EcologyJsonParameterCodec a)
  , ecologyCategoryCodec :: !(EcologyJsonParameterCodec b)
  , ecologyStatusCodec   :: !(EcologyJsonParameterCodec c)
  }

ecologyEnvVarValueToJSONV1
  :: EnvironmentVariableValue
  -> Value
ecologyEnvVarValueToJSONV1 (EnvironmentVariableValue v) = object ["type" .= t "value", "value" .= v]
ecologyEnvVarValueToJSONV1 (EnvironmentVariableReferenceValue x) = object ["type" .= t "aws_param_store_ref", "parameter" .= x]

ecologyEnvVarValueFromJsonV1
  :: Object
  -> Parser EnvironmentVariableValue
ecologyEnvVarValueFromJsonV1 o =
  o .: "type" >>= jsonDispatchOnText [
      ("value", EnvironmentVariableValue <$> o .: "value")
    , ("aws_param_store_ref", EnvironmentVariableReferenceValue <$> o .: "parameter")
    ]

ecologyEnvironmentPairToJSONV1
  :: EcologyEnvironmentPair
  -> Value
ecologyEnvironmentPairToJSONV1 (EcologyEnvironmentPair (EnvironmentVariableName name) val) =
  object [
    "name"  .= name
  , "value" .= ecologyEnvVarValueToJSONV1 val
  ]

ecologyEnvironmentPairFromJsonV1
  :: Object
  -> Parser EcologyEnvironmentPair
ecologyEnvironmentPairFromJsonV1 o = EcologyEnvironmentPair
  <$> (EnvironmentVariableName <$> (o .: "name"))
  <*> (o .: "value" >>= ecologyEnvVarValueFromJsonV1)

ecologyProjectCIToJSONV1
  :: EcologyJsonParameterCodec i
  -> EcologyProjectCI i
  -> Value
ecologyProjectCIToJSONV1 (EcologyJsonParameterCodec encode' _) ci' =
  object [
    "type" .= (encode' . ciType $ ci')
  , "environment" .= (fmap ecologyEnvironmentPairToJSONV1 . ciEnvironmentVars) ci'
  ]

ecologyProjectCIFromJsonV1
  :: EcologyJsonParameterCodec i
  -> Object
  -> Parser (EcologyProjectCI i)
ecologyProjectCIFromJsonV1 codec o = EcologyProjectCI
  <$> (o .: "type" >>= ecologyParseJson codec)
  <*> (o .: "environment" >>= traverse ecologyEnvironmentPairFromJsonV1)

ecologyPermissionToJSONV1
  :: EcologyPermission
  -> Value
ecologyPermissionToJSONV1 = toJSON . ecologyPermissionText

ecologyPermissionFromJsonV1
  :: Value
  -> Parser EcologyPermission
ecologyPermissionFromJsonV1 =
  jsonTextEnum . fmap (ecologyPermissionText >>= (,)) $ permissionValues

ecologyPrivacyToJSONV1
  :: EcologyPrivacy
  -> Value
ecologyPrivacyToJSONV1 = toJSON . ecologyPrivacyText

ecologyPrivacyFromJsonV1
  :: Value
  -> Parser EcologyPrivacy
ecologyPrivacyFromJsonV1 =
  jsonTextEnum . fmap (ecologyPrivacyText >>= (,)) $ privacyValues

ecologyTeamPermissionToJSONV1
  :: TeamPermission
  -> Value
ecologyTeamPermissionToJSONV1 (TeamPermission (TeamName name) perm) =
  object [
    "name"        .= name
  , "permissions" .= (ecologyPermissionToJSONV1 <$> perm)
  ]

ecologyTeamPermissionFromJsonV1
  :: Object
  -> Parser TeamPermission
ecologyTeamPermissionFromJsonV1 o = TeamPermission
  <$> (TeamName <$> (o .: "name"))
  <*> (o .: "permissions" >>= traverse ecologyPermissionFromJsonV1)

ecologyProjectToJsonV1
  :: EcologyJsonParameterCodecs g i a b c -- * v1 codecs
  -> EcologyProject g i a b c
  -> Value
ecologyProjectToJsonV1 codecs (EcologyProject name loc desc typ cat status teams' ci' prv tags' experts') =
  object [
    "name"        .= ecologyProjectNameText name
  , "location"    .= (ecologyToJson . ecologyLocationCodec $ codecs) loc
  , "description" .= ecologyProjectDescriptionText desc
  , "status"      .= (ecologyToJson . ecologyStatusCodec $ codecs) status
  , "class"       .= (ecologyToJson . ecologyTypeCodec $ codecs) typ
  , "category"    .= ((ecologyToJson . ecologyCategoryCodec $ codecs) <$> cat)
  , "teams"       .= (ecologyTeamPermissionToJSONV1 <$> teams')
  , "ci"          .= ecologyProjectCIToJSONV1 (ecologyCITypeCodec codecs) ci'
  , "privacy"     .= ecologyPrivacyToJSONV1 prv
  , "tags"        .= (ecologyTagText <$> tags')
  , "experts"     .= (usernameText <$> experts')
  ]

ecologyProjectFromJsonV1
  :: EcologyJsonParameterCodecs g i a b c
  -> Object
  -> Parser (EcologyProject g i a b c)
ecologyProjectFromJsonV1 codecs o = EcologyProject
  <$> (EcologyProjectName <$> (o .: "name"))
  <*> ((o .: "location") >>= (ecologyParseJson . ecologyLocationCodec $ codecs))
  <*> (EcologyProjectDescription <$> o .: "description")
  <*> (o .: "class" >>= (ecologyParseJson . ecologyTypeCodec $ codecs))
  <*> (o .: "category" >>= traverse (ecologyParseJson . ecologyCategoryCodec $ codecs))
  <*> (o .: "status" >>= (ecologyParseJson . ecologyStatusCodec $ codecs))
  <*> (o .: "teams" >>= traverse ecologyTeamPermissionFromJsonV1)
  <*> (o .: "ci" >>= ecologyProjectCIFromJsonV1 (ecologyCITypeCodec codecs))
  <*> (o .: "privacy" >>= ecologyPrivacyFromJsonV1)
  <*> (o .: "tags" >>= traverse (fmap EcologyTag . parseJSON))
  <*> (o .: "experts" >>= traverse (fmap Username . parseJSON))

data EcologyDigestStoreJson =
  EcologyDigestStoreJsonV1 !EcologyDigestStore
  deriving (Show, Eq)

ecologyDigestStoreLatestJson
  :: EcologyDigestStore
  -> EcologyDigestStoreJson
ecologyDigestStoreLatestJson = EcologyDigestStoreJsonV1

ecologyDigestStoreFromJson
  :: EcologyDigestStoreJson
  -> EcologyDigestStore
ecologyDigestStoreFromJson (EcologyDigestStoreJsonV1 x) = x

instance ToJSON EcologyDigestStoreJson where
--toJSON :: a -> Value
  toJSON (EcologyDigestStoreJsonV1 x) = ecologyDigestStoreV1 x

instance FromJSON EcologyDigestStoreJson where
--parseJSON :: Value -> Parser EcologyDigestStoreJson
  parseJSON v = do
    o <- parseJSON v
    o .: "version" >>=
      jsonDispatchOnText [
        ("1", EcologyDigestStoreJsonV1 <$> ecologyDigestStoreParseJsonV1 o)
      ]

ecologyDigestStoreV1
  :: EcologyDigestStore
  -> Value
ecologyDigestStoreV1 s =
  let
    digests = toJSON . flip fmap (H.toList . digestStore $ s) $ \(pn, digests') ->
      object [
        "project" .= ecologyProjectNameText pn
      , "digests" .= ecologyDigestsJsonV1 digests'
      ]
  in object [
      "version" .= t "1"
    , "project_digests" .= digests
    ]

ecologyDigestsJsonV1 :: EcologyDigests -> Value
ecologyDigestsJsonV1 (EcologyDigests ciParams ciOther) = object [
    "ci_params" .= ecologyHashMapJsonV1 ciParams
  , "ci_other" .= ecologyHashMapJsonV1 ciOther
  ]


ecologyHashMapJsonV1
  :: EcologyHashMap
  -> Value
ecologyHashMapJsonV1 (EcologyHashMap ds) =
  toJSON . flip fmap (H.toList ds) $ \(paramName, digest) ->
    object [
      "parameter" .= paramName
    , "digest" .= digest
    ]

ecologyHashMapParseJsonV1
  :: Value
  -> Parser EcologyHashMap
ecologyHashMapParseJsonV1 v = do
  jsonObjects <- parseJSON v
  kvs <- forM jsonObjects $ \o ->
    (,) <$> o .: "parameter" <*> o .: "digest"
  pure . EcologyHashMap . H.fromList $ kvs

ecologyDigestsParseJsonV1
  :: Object
  -> Parser EcologyDigests
ecologyDigestsParseJsonV1 o =
  EcologyDigests
    <$> (o .: "ci_params" >>= ecologyHashMapParseJsonV1)
    <*> (o .: "ci_other" >>= ecologyHashMapParseJsonV1)

ecologyDigestStoreParseJsonV1
  :: Object
  -> Parser EcologyDigestStore
ecologyDigestStoreParseJsonV1 o = do
  projectDigests <- o .: "project_digests"
  kvs <- forM projectDigests $ \o' ->
    (,)
      <$> (EcologyProjectName <$> o' .: "project")
      <*> (o' .: "digests" >>= ecologyDigestsParseJsonV1)
  pure . EcologyDigestStore . H.fromList $ kvs

t :: T.Text -> T.Text
t = id
