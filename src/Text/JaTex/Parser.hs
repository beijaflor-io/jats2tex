{-# LANGUAGE RecordWildCards #-}
module Text.JaTex.Parser
  where

import qualified Data.ByteString       as ByteString
import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text.ICU.Convert as ICU
import           Text.XML.HXT.Core
import           Text.XML.Light

import           JATSXML.HTMLEntities
-- import           Text.XML.HaXml

type JATSDoc = XmlTrees

readJatsFile :: Maybe String -> FilePath -> IO Text
readJatsFile encoding inputFile = do
  inp <- ByteString.readFile inputFile
  decodeLatin inp
  where
    decodeLatin i = do
      converter <- ICU.open (fromMaybe "latin-1" encoding) Nothing
      return (ICU.toUnicode converter i)

readJats :: Maybe String -> FilePath -> IO [XmlTree]
readJats _ fp = parseJATS =<< readFile fp

parseJATS :: String -> IO [XmlTree]
parseJATS = runX . readString [ withValidate no
                              , withInputEncoding utf8
                              ]

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
