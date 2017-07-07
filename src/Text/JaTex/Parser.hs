module Text.JaTex.Parser
  where

import qualified Data.Text          as Text
import qualified Text.JaTex.CleanUp as CleanUp
import           Text.XML.HXT.Core

type JATSDoc = XmlTrees

readJats :: Maybe String -> FilePath -> IO [XmlTree]
readJats mencoding@(Just _) fp = do
    input <- CleanUp.readFile mencoding fp
    parseJATS (Text.unpack input)
readJats Nothing fp = do
    input <- readFile fp
    parseJATS input

parseJATS :: String -> IO [XmlTree]
parseJATS = runX . readString [ withValidate no
                              -- , withInputEncoding utf8
                              , withSubstDTDEntities no
                              , withSubstHTMLEntities yes
                              ]
