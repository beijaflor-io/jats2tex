{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.JaTex.Cmd.Main
  where

import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           System.IO

import           Options.Applicative
import           Text.JaTex
import           Text.JaTex.Parser
import qualified Text.JaTex.Upgrade  as Upgrade


data Options
  = RunUpgrade
  | RunVersion
  | Options { optsOutputFile   :: Maybe FilePath
            , optsTemplateFile :: Maybe String
            , optsInputFile    :: FilePath
            }

options :: Parser Options
options =
  subparser
    (metavar "version" <>
     command
       "version"
       (info (pure RunVersion) (fullDesc <> progDesc "Print the version"))) <|>
  subparser
    (metavar "upgrade" <>
     command
       "upgrade"
       (info (pure RunUpgrade) (fullDesc <> progDesc "Upgrade jats2tex"))) <|>
  Options <$>
  optional
    (strOption
       (long "output" <> metavar "OUTPUT_FILE" <> help "LaTeX Output File")) <*>
  optional
    (strOption
       (long "template" <> metavar "TEMPLATE_FILE" <> help "Template File")) <*>
  argument str (metavar "INPUT_FILE" <> help "XML Input File")

optionsPI :: ParserInfo Options
optionsPI =
  info
    (options <**> helper)
    (fullDesc <> progDesc "Convert JATS-XML INPUT_FILE to LaTeX OUTPUT_FILE"
    <> header "jats2tex - Customizable JATS to LaTeX Conversion")

run :: FilePath -> Handle -> Text -> IO ()
run inputFile outputHandle templateFile = do
  -- inputFileC <- Text.unpack <$> readJatsFile inputFile
  contents <- readJats inputFile
  Text.hPutStr outputHandle (jatsXmlToLaTeXText inputFile contents)
  hFlush outputHandle

defaultMain :: IO ()
defaultMain = do
  opts <- execParser optionsPI
  case opts of
    Options {..} -> do
      outputFile <-
        case optsOutputFile of
          Nothing -> return stdout
          Just f  -> openFile f WriteMode
      templateFile <-
        case optsTemplateFile of
          Nothing -> return ""
          Just f  -> Text.readFile f
      run optsInputFile outputFile templateFile
    RunUpgrade -> Upgrade.runUpgrade
    RunVersion -> Upgrade.putVersionInfo
