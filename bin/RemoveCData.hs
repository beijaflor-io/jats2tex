{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text
import           System.Environment
import           Text.XML.Light

import           Text.JaTex.Parser
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
      inputFile <- parseJATS <$> readJatsFile targetFp
      putStrLn (concatMap showContent (concatMap cleanUp inputFile))
