{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.API.CI
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.API.CI (
  -- * Types
    CIAPI(..)
  , CIAPIs(..)
  ) where

import Irreverent.Ecology.Core

import Ultra.Control.Monad.Trans.Either (EitherT)
import qualified Ultra.Data.Text as T

import Preamble

newtype CIAPIs a i m e = CIAPIs {
    selectCIAPI :: i -> CIAPI a m e
  }

data CIAPI a m e = CIAPI {
    initialCIInRepoConfig :: !(T.Text -> EcologyParameters -> NewCIInfo a -> EitherT e m (Maybe T.Text)) -- ^ Some CI services need in repo configuration, this hook provides the opportunity to do that, return a text commit log message if you make changes you wish to have commited to the repo
  , ciRemoteSetup :: !(EcologyParameters -> NewCIInfo a -> EitherT e m EcologyHashMap) -- ^ Gives you an opportunity to make any remote calls to the CI service to activate it for the repo, set up build configurations etc... Will be called after any in repo config has been made
  , ciParameterUpdate :: !(EcologyProjectName -> NonEmpty EcologyEnvironmentVariableUpdate -> EitherT e m ()) -- ^ Once a project is created, CI config in the form of environment variables can change
  }
