{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.API.Hooks
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.API.Hooks (
  -- * Types
    CustomHooks(..)
  -- * Values
  , emptyHooks
  ) where

import Irreverent.Ecology.Core.Data (EcologyProject)

import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT)

import Preamble

newtype CustomHooks g i a b c m he = CustomHooks {
    chPostSyncHook :: EcologyProject g i a b c -> EitherT he m ()
  }

instance (Functor m) => Functor (CustomHooks g i a b c m) where
--fmap :: (a -> b) -> f a -> f b
  fmap f (CustomHooks postSync) =
    CustomHooks (firstEitherT f . postSync)

emptyHooks :: (Monad m) => CustomHooks g i a b c m he
emptyHooks = CustomHooks (const $ pure ())
