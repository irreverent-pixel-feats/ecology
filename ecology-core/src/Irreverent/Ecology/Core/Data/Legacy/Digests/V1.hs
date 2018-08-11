{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.Core.Data.Legacy.Digests.V1
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.Core.Data.Legacy.Digests.V1 (
  -- * Types
    EcologyDigestStore(..)
  , EcologyDigests(..)
  ) where

import Irreverent.Ecology.Core.Data (
    EcologyHashMap
  , EcologyProjectName
  )

import qualified Ultra.Data.HashMap.Strict as H

import Preamble

newtype EcologyDigestStore = EcologyDigestStore {
    digestStore :: H.HashMap EcologyProjectName EcologyDigests
  } deriving (Show, Eq)

data EcologyDigests = EcologyDigests {
    ciParamMap :: EcologyHashMap  -- CI Parameters, used by 'users'
  , ciOtherMap :: EcologyHashMap  -- Other stateful hashes decided by the CI API implementations
  } deriving (Show, Eq)

