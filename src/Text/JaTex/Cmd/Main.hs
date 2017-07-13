{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.JaTex.Cmd.Main
  where

import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.IO         as Text
import           GHC.IO.Encoding
import           System.FilePath
import           System.IO

import           Options.Applicative
#ifdef mingw32_HOST_OS
import           System.Win32.Console
#endif
import           Text.JaTex
import qualified Text.JaTex.Upgrade   as Upgrade


data Options
  = RunUpgrade
  | RunVersion
  | Options { optsOutputFile    :: Maybe FilePath
            , optsTemplateFile  :: Maybe String
            , optsColumnWidth   :: Maybe Int
            , optsWarnings      :: Bool
            , optsInputEncoding :: Maybe String
            , optsInputFile     :: FilePath
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
    (option
       auto
       (short 'w' <> long "max-width" <> metavar "MAX_COLUMN_WIDTH" <>
        help "Maximum Column Width 80 by default, set to 0 to disable")) <*>
  flag False True (short 'W' <> long "warnings" <> help "Enable warnings") <*>
  optional
    (strOption
       (short 'e' <> long "input-encoding" <> metavar "INPUT_ENCODING" <>
        help (unlines [ "The input file/handle encoding (defaults to latin-1)"
                      , "Output and FFI with Lua is always with UTF-8"
                      ]))) <*>
  argument str (metavar "INPUT_FILE" <> help "XML Input File")

optionsPI :: ParserInfo Options
optionsPI =
  info
    (options <**> helper)
    (fullDesc <> progDesc "Convert JATS-XML INPUT_FILE to LaTeX OUTPUT_FILE"
    <> header "jats2tex - Customizable JATS to LaTeX Conversion")

run :: Options -> IO ()
run Options{..} = do
  templateFile <-
    case optsTemplateFile of
      Nothing -> return defaultTemplate
      Just f -> do
        t <- parseTemplateFile f
        return (t, f)

  contents <- readJats optsInputEncoding optsInputFile
  result <- jatsXmlToLaTeXText def { joInputFilePath = optsInputFile
                                   , joTemplate = templateFile
                                   , joMaxWidth = fromMaybe 80 optsColumnWidth
                                   , joInputDocument = contents
                                   , joWarnings = optsWarnings
                                   }

  let outputFile = fromMaybe (dropExtension optsInputFile <> ".tex") optsOutputFile
  Text.writeFile outputFile result
  putStrLn $ "Wrote: " <> outputFile
run RunUpgrade = Upgrade.runUpgrade
run RunVersion = Upgrade.putVersionInfo

makeSafe :: Handle -> IO ()
makeSafe h = hSetEncoding h utf8

defaultMain :: IO ()
defaultMain = do
  setLocaleEncoding utf8
#ifdef mingw32_HOST_OS
  setConsoleCP 65001
#endif

  opts <- execParser optionsPI
  run opts
