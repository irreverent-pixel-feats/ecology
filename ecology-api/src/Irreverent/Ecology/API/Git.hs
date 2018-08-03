{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.API.Git
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.API.Git (
  -- * Types
    GitPlatformAPI(..)
  , GitPlatformAPIs(..)
  ) where

import Irreverent.Ecology.Core.Data (GitRepository, NewGitRepository)

import Ultra.Control.Monad.Trans.Either (EitherT)
import qualified Ultra.Data.Text as T

newtype GitPlatformAPIs g a b m e = GitPlatformAPIs {
    selectGitAPI :: g -> GitPlatformAPI a b m e
  }

data GitPlatformAPI a b m e = GitPlatformAPI {
    gpAuthToken   :: !T.Text
  , getOrgRepos   :: EitherT e m [GitRepository]
  , createNewRepo :: NewGitRepository a b -> EitherT e m GitRepository
  }
