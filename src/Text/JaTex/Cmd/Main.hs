{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.JaTex.Cmd.Main
  where

import           Data.Maybe
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
            , optsColumnWidth  :: Maybe Int
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
       (short 'o' <> long "output" <> metavar "OUTPUT_FILE" <>
        help "LaTeX Output File")) <*>
  optional
    (strOption
       (short 't' <> long "template" <> metavar "TEMPLATE_FILE" <>
        help "YAML/JSON Template File")) <*>
  optional
    (option auto
      (short 'w' <> long "max-width" <> metavar "MAX_COLUMN_WIDTH" <>
       help "Maximum Column Width 80 by default, set to 0 to disable")) <*>
  argument str (metavar "INPUT_FILE" <> help "XML Input File")

optionsPI :: ParserInfo Options
optionsPI =
  info
    (options <**> helper)
    (fullDesc <> progDesc "Convert JATS-XML INPUT_FILE to LaTeX OUTPUT_FILE"
    <> header "jats2tex - Customizable JATS to LaTeX Conversion")

run :: FilePath -> Handle -> Template -> Int -> IO ()
run inputFile outputHandle templateFile maxWidth = do
  -- inputFileC <- Text.unpack <$> readJatsFile inputFile
  contents <- readJats inputFile
  result <- jatsXmlToLaTeXText def { joInputFilePath = inputFile
                                   , joTemplate = templateFile
                                   , joMaxWidth = maxWidth
                                   , joInputDocument = contents
                                   }
  Text.hPutStr outputHandle result
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
          Nothing -> return defaultTemplate
          Just f  -> parseTemplateFile f
      run optsInputFile outputFile templateFile (fromMaybe 80 optsColumnWidth)
    RunUpgrade -> Upgrade.runUpgrade
    RunVersion -> Upgrade.putVersionInfo
