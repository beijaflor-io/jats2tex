{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Monoid
import qualified Data.Text.IO       as Text (hPutStr)
import           System.Environment (getArgs)
import           System.IO

import           Text.JaTex

main :: IO ()
main = getArgs >>= \case
    [inputFile, outputFile] -> run inputFile =<< openFile outputFile WriteMode
    [inputFile] -> run inputFile stdout
    _ -> error "Usage: jatex <input-file> <output-file>"

run :: FilePath -> Handle -> IO ()
run inputFile outputHandle = do
    hPutStrLn stderr ("Using " <> inputFile <> " writting " <> show outputHandle)
    contents <- readJats inputFile
    Text.hPutStr outputHandle (jatsXmlToLaTeXText contents)
