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
  -- * Functions
  , ecologyDigestStoreV1toV2
  , ecologyDigestStoreV2toV1
  , ecologyDigestsV1toV2
  ) where

import Irreverent.Ecology.Core.Data (
    EcologyHashMap
  , EcologyProjectName
  )

import qualified Irreverent.Ecology.Core.Data as V2 (
    EcologyDigests2(..)
  , EcologyDigestStore2(..)
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

ecologyDigestStoreV1toV2
  :: EcologyDigestStore
  -> V2.EcologyDigestStore2
ecologyDigestStoreV1toV2 (EcologyDigestStore m) =
  V2.EcologyDigestStore2 (ecologyDigestsV1toV2 <$> m)

ecologyDigestStoreV2toV1
  :: V2.EcologyDigestStore2
  -> EcologyDigestStore
ecologyDigestStoreV2toV1 (V2.EcologyDigestStore2 m) =
  EcologyDigestStore (ecologyDigestsV2toV1 <$> m)

ecologyDigestsV1toV2
  :: EcologyDigests
  -> V2.EcologyDigests2
ecologyDigestsV1toV2 (EcologyDigests x y)
  = V2.EcologyDigests2 x y

ecologyDigestsV2toV1
  :: V2.EcologyDigests2
  -> EcologyDigests
ecologyDigestsV2toV1 (V2.EcologyDigests2 x y)
  = EcologyDigests x y
