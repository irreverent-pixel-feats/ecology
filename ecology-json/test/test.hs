module Main where

import qualified Test.Irreverent.Ecology.Json.Data

import Lab.Core.Main

import Preamble

main :: IO ()
main = labMain [
    Test.Irreverent.Ecology.Json.Data.tests
  ]
