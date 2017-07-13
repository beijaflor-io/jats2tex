module Text.JaTex.Parser
  where

import qualified Text.JaTex.CleanUp as CleanUp
import           Text.XML.HXT.Core
import           Text.XML.HXT.Expat

type JATSDoc = XmlTrees

readJats :: Maybe String -> FilePath -> IO [XmlTree]
readJats mencoding@(Just _) fp = do
    input <- CleanUp.cleanUpXMLFile mencoding fp
    parseJATS input
readJats Nothing fp = runX $ readDocument hxtOptions fp

hxtOptions :: [SysConfig]
hxtOptions =
  [ withValidate no
  , withExpat yes
  -- , withInputEncoding utf8
  , withSubstDTDEntities no
  , withSubstHTMLEntities yes
  ]

parseJATS :: String -> IO [XmlTree]
parseJATS = runX . readString hxtOptions
