{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Irreverent.Ecology.Cli.Main (ecoMain) where

import BuildInfo_ecology_cli

import Ultra.Cli
import qualified Ultra.Data.Text as T
import Ultra.Options.Applicative
  ( Parser
  , command'
  , commandParser
  , parseAndRun
  )

import Data.Monoid ((<>))
import System.IO (putStrLn)

import Preamble hiding ( (<>) )

versionString :: String
versionString = "eco: " <> buildInfoVersion

data Command = Version | ExampleCommand deriving (Show, Eq)

foldCommand :: a -> a -> Command -> a
foldCommand v e c = case c of
    Version         -> v
    ExampleCommand  -> e

commandParser' :: Parser Command
commandParser' = commandParser Version [
        command' "example" "Example Description" (pure ExampleCommand)
    ]

runCommand :: Command -> IO ()
runCommand =
    let
        renderError :: () -> T.Text
        renderError _ = "Some Error"
    in renderErrorAndDie renderError . foldCommand (lift printVersion) (pure ())

printVersion :: IO ()
printVersion = putStrLn versionString

ecoMain :: IO ()
ecoMain = parseAndRun "ecology" "Manages git projects" commandParser' runCommand
