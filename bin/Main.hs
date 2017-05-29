{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.IO        as Text (hPutStr)
import           System.Environment  (getArgs)
import           System.IO

import           Options.Applicative
import           Text.JaTex

data Options = Options { optsOutputFile   :: Maybe FilePath
                       , optsTemplateFile :: Maybe String
                       , optsInputFile    :: FilePath
                       }

options :: Parser Options
options =
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
    Options{..} <- execParser optionsPI
    outputFile <- case optsOutputFile of
        Nothing -> return stdout
        Just f  -> openFile f WriteMode
    run optsInputFile outputFile

run :: FilePath -> Handle -> IO ()
run inputFile outputHandle = do
    -- hPutStrLn stderr ("Using " <> inputFile <> " writting " <> show outputHandle)
    contents <- readJats inputFile
    Text.hPutStr outputHandle (jatsXmlToLaTeXText contents)
