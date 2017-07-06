{-# LANGUAGE RecordWildCards #-}
module Text.JaTex.CleanUp
    ( Text.JaTex.CleanUp.readFile
    , cleanUp
    , cleanUpBS
    , cleanUpXML
    , cleanUpXMLFile
    , module Text.XML.Light
    )
  where

import qualified Data.ByteString       as ByteString
import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text.ICU.Convert as ICU
import           Text.XML.Light

import           JATSXML.HTMLEntities

readFile :: Maybe String -> FilePath -> IO Text
readFile encoding inputFile = do
  inp <- ByteString.readFile inputFile
  cleanUpBS encoding inp

cleanUpBS :: Maybe String -> ByteString.ByteString -> IO Text
cleanUpBS encoding inputBS = do
  converter <- ICU.open (fromMaybe "latin-1" encoding) Nothing
  return (ICU.toUnicode converter inputBS)

cleanUpXML :: Maybe String -> ByteString.ByteString -> IO String
cleanUpXML encoding input =
  concatMap showContent . concatMap cleanUp . parseXML <$>
  cleanUpBS encoding input

cleanUpXMLFile :: Maybe String -> FilePath -> IO String
cleanUpXMLFile mencoding targetFp = do
  inputFile <- parseXML <$> Text.JaTex.CleanUp.readFile mencoding targetFp
  return (concatMap showContent (concatMap cleanUp inputFile))

cleanUp :: Content -> [Content]
cleanUp t@(Text (CData CDataText _ _)) = [t]
cleanUp (Text (CData _ str ml)) = concatMap (cleanUp . helper) $ parseXML str
  where
    helper (Elem el) =
      Elem el {elLine = Just $ fromMaybe 0 (elLine el) + fromMaybe 0 ml}
    helper c = c
cleanUp (CRef ref) =
  [Text (CData CDataText (fromMaybe ref (crefToString ref)) Nothing)]
cleanUp (Elem e@Element {..})
  | not (null elContent) =
    [Elem (e {elContent = concatMap cleanUp elContent})]
cleanUp c = [c]
