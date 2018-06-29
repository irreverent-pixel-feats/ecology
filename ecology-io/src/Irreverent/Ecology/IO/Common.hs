{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.IO.Common
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.IO.Common (
  -- * Functions
    logText
  ) where

import qualified Data.ByteString as BS
import qualified Ultra.Data.Text.Encoding as T
import qualified Ultra.Data.Text as T

import System.IO (stdout)

import Preamble

logText
  :: (MonadIO m)
  => T.Text
  -> m ()
logText = liftIO
  . BS.hPut stdout
  . T.encodeUtf8
  . flip T.snoc '\n'
