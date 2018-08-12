{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Ecology.Core.Data.Legacy.Digests.V1
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Irreverent.Ecology.Core.Data.Legacy.Digests.V1 (
  -- * Generators
    v1EcologyDigests
  , v1EcologyDigestStores
  ) where

import Test.Irreverent.Ecology.Core.Data

import Irreverent.Ecology.Core.Data.Legacy.Digests.V1

import Lab.Core.Gen (
    boundedListOf
  )
import Lab.Core.QuickCheck (Gen)

import qualified Ultra.Data.HashMap.Strict as H

import Preamble

v1EcologyDigestStores :: Gen EcologyDigestStore
v1EcologyDigestStores = EcologyDigestStore . H.fromList <$>
  boundedListOf 50 ((,) <$> projectNames <*> v1EcologyDigests)

v1EcologyDigests :: Gen EcologyDigests
v1EcologyDigests = EcologyDigests
  <$> ecologyHashMaps
  <*> ecologyHashMaps

