{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.API.IM
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.API.IM (
  -- * Types
    IMAPI(..)
  , IMAPIs(..)
  ) where

import Irreverent.Ecology.Core (
    GitRepository
  )

import Ultra.Control.Monad.Trans.Either (EitherT)

newtype IMAPIs i m e = IMAPIs {
    selectIMAPI :: i -> IMAPI m e
  }

data IMAPI m e = IMAPI {
    ipSyncReport :: [GitRepository] -> EitherT e m ()
  }


