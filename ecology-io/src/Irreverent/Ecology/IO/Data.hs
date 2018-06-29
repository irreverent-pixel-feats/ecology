{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.IO.Data
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.IO.Data (
  -- * Types
    VerifiedProject(..)
  ) where

import Irreverent.Ecology.Core.Data
import Irreverent.Ecology.API.Git

-- TODO: Do I still need this?
data VerifiedProject g i m ge a b c = VerifiedProject {
    vpGitAPI  :: GitPlatformAPI b m ge
  , vpProject :: EcologyProject g i a b c
  }
