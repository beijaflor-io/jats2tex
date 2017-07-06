{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Monoid
import           System.Environment
import           Text.XML.Light

import qualified Text.JaTex.CleanUp as CleanUp
import qualified Text.JaTex.Upgrade as Upgrade

helpText :: String
helpText =
  unlines
    [ "jats2tex-remove-c-data@" <> Upgrade.versionNumber Upgrade.currentVersion
    , ""
    , "  Clean-up JATS-XML files, format CData HTML/XML blocks"
    , "  Translate HTML entities and latin-1 to UTF-8"
    , ""
    , "Usage:"
    , ""
    , "  jats2tex-remove-c-data <input.xml>"
    , ""
    ]

help :: IO ()
help = putStrLn helpText

main :: IO ()
main =
  getArgs >>= \case
    ("--help":_) -> help
    ("-h":_) -> help
    [] -> help
    (targetFp:_) -> do
      putStrLn =<< CleanUp.cleanUpXMLFile Nothing targetFp
