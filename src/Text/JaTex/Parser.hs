module Text.JaTex.Parser
  where

import qualified Text.JaTex.CleanUp as CleanUp
import           Text.XML.HXT.Core

type JATSDoc = XmlTrees

readJats :: Maybe String -> FilePath -> IO [XmlTree]
readJats encoding fp = do
    input <- CleanUp.cleanUpXMLFile encoding fp
    parseJATS input

parseJATS :: String -> IO [XmlTree]
parseJATS = runX . readString [ withValidate no
                              , withInputEncoding utf8
                              ]
