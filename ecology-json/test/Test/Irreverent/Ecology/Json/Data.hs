{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Ecology.Json.Data
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Irreverent.Ecology.Json.Data where

import Test.Irreverent.Ecology.Core.Data

import Irreverent.Ecology.Core
import Irreverent.Ecology.Json.Data

import Lab.Core.Control.RoundTrip (roundTripProp)
import Lab.Core.QuickCheck (Property, forAll)
import Lab.Core.QuickCheck.TH (quickCheckAll)

import qualified Ultra.Data.Aeson as A

import Preamble

emptyCodec :: EcologyJsonParameterCodec ()
emptyCodec = EcologyJsonParameterCodec A.toJSON A.parseJSON

emptyCodecs :: EcologyJsonParameterCodecs () () () () ()
emptyCodecs = EcologyJsonParameterCodecs
  emptyCodec
  emptyCodec
  emptyCodec
  emptyCodec
  emptyCodec

prop_projectJsonV1 :: Property
prop_projectJsonV1 = forAll (projects (pure ()) (pure ()) (pure ()) (pure ()) (pure ())) $
  roundTripProp (ecologyProjectToJsonV1 emptyCodecs) (A.parseEither (A.parseJSON >=> ecologyProjectFromJsonV1 emptyCodecs))

return []
tests :: IO Bool
tests = $quickCheckAll

