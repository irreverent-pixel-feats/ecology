{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Ecology.Core.Data
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Irreverent.Ecology.Core.Data (
  -- * Generators
    projects
  , projectCIs
  , envVarNames
  , envVarValues
  , envVarPairs
  , projectNames
  , projectDescriptions
  , teamPermissions
  , teamNames
  , ecologyPermissions
  , ecologyPrivacies
  , ecologyTags
  , usernames
  ) where

import Irreverent.Ecology.Core.Data

import Lab.Core.Gen (
    alphaChars
  , alphaNumChars
  , boundedListOf
  , boundedTextOf
  , boundedTextOf1
  , maybeOf
  )
import Lab.Core.QuickCheck (Gen, elements, oneof)

import Preamble

projects
  :: Gen g
  -> Gen i
  -> Gen a
  -> Gen b
  -> Gen c
  -> Gen (EcologyProject g i a b c)
projects locations ciTypes types cats statuses = EcologyProject
  <$> projectNames
  <*> locations
  <*> projectDescriptions
  <*> types
  <*> maybeOf cats
  <*> statuses
  <*> boundedListOf 5 teamPermissions
  <*> projectCIs ciTypes
  <*> ecologyPrivacies
  <*> boundedListOf 10 ecologyTags
  <*> boundedListOf 10 usernames

projectCIs :: Gen i -> Gen (EcologyProjectCI i)
projectCIs is = EcologyProjectCI
  <$> is
  <*> boundedListOf 10 envVarPairs

envVarNames :: Gen EnvironmentVariableName
envVarNames = EnvironmentVariableName <$> boundedTextOf1 15 "_ABCDEFGHIJKLMNOPQRSTUVWXYZ"

envVarValues :: Gen EnvironmentVariableValue
envVarValues = oneof [
    EnvironmentVariableValue <$> boundedTextOf1 30 alphaNumChars
  , EnvironmentVariableReferenceValue <$> boundedTextOf1 20 alphaNumChars
  ]

envVarPairs :: Gen EcologyEnvironmentPair
envVarPairs = EcologyEnvironmentPair
  <$> envVarNames
  <*> envVarValues

projectNames :: Gen EcologyProjectName
projectNames = EcologyProjectName <$> boundedTextOf1 10 alphaChars

projectDescriptions :: Gen EcologyProjectDescription
projectDescriptions =
  EcologyProjectDescription <$> boundedTextOf 50 (' ' : alphaChars)

teamPermissions :: Gen TeamPermission
teamPermissions = TeamPermission
  <$> teamNames
  <*> maybeOf ecologyPermissions

teamNames :: Gen TeamName
teamNames = TeamName <$> boundedTextOf1 10 alphaNumChars

ecologyPermissions :: Gen EcologyPermission
ecologyPermissions = elements permissionValues

ecologyPrivacies :: Gen EcologyPrivacy
ecologyPrivacies = elements privacyValues

ecologyTags :: Gen EcologyTag
ecologyTags = EcologyTag <$> boundedTextOf1 10 alphaNumChars

usernames :: Gen Username
usernames = Username <$> boundedTextOf1 10 alphaNumChars
