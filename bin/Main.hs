{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Data.Monoid
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text (hPutStr)
import           System.IO

import           Options.Applicative
import           Text.JaTex
-- import           Text.JaTex.JatsElements
import           Text.JaTex.Parser
import qualified Text.JaTex.Upgrade  as Upgrade


data Options
  = RunUpgrade
  | RunVersion
  | Options { optsOutputFile   :: Maybe FilePath
            , optsTemplateFile :: Maybe String
            , optsInputFile    :: FilePath}

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

main :: IO ()
main = do
  opts <- execParser optionsPI
  case opts of
    Options {..} -> do
      outputFile <-
        case optsOutputFile of
          Nothing -> return stdout
          Just f  -> openFile f WriteMode
      run optsInputFile outputFile
    RunUpgrade -> Upgrade.runUpgrade
    RunVersion -> Upgrade.putVersionInfo

run :: FilePath -> Handle -> IO ()
run inputFile outputHandle = do
  -- inputFileC <- Text.unpack <$> readJatsFile inputFile
  contents <- readJats inputFile
  Text.hPutStr outputHandle (jatsXmlToLaTeXText inputFile contents)
  hFlush outputHandle
  -- let d@(HaXml.Document _ _ root _) = HaXml.xmlParse inputFile inputFileC
  -- let (r, _) = PolyParse.runParser elementArticle -- HaXml.emptySTs
  --                       [HaXml.CElem root HaXml.noPos]
  -- print r
