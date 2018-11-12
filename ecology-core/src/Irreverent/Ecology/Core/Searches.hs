{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.Core.Searches
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.Core.Searches (
  -- * Functions
    searchParam
  ) where

import Irreverent.Ecology.Core.Data

import Ultra.Data.Foldable (filteredBy)
import qualified Ultra.Data.Text as T

import Preamble

searchParam
  :: T.Text
  -> [EcologyProject g i a b c]
  -> [(EcologyProjectName, NonEmpty EnvironmentVariableName)]
searchParam paramPath =
  let
    checkEnvironmentPair :: EcologyEnvironmentPair -> Maybe EnvironmentVariableName
    checkEnvironmentPair (EcologyEnvironmentPair _ (EnvironmentVariableValue _)) = Nothing
    checkEnvironmentPair (EcologyEnvironmentPair n (EnvironmentVariableReferenceValue p)) =
      if p == paramPath then pure n else Nothing

    searchProject'
      :: EcologyProject g i a b c
      -> Maybe (EcologyProjectName, NonEmpty EnvironmentVariableName)
    searchProject' proj = fmap ((,) . ecologyProjectName $ proj)
      . nonEmpty
      . filteredBy checkEnvironmentPair
      . ciEnvironmentVars
      . ci
      $ proj
  in filteredBy searchProject'
