module Text.JaTex.Parser
  where

import qualified Data.ByteString       as ByteString
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.ICU.Convert as ICU
import           Text.XML.Light

type JATSDoc = [Content]

readJats :: FilePath -> IO JATSDoc
readJats inputFile = do
  inp <- ByteString.readFile inputFile
  -- print ICU.converterNames
  inp' <- decodeLatin inp
  -- print (map fromXMLNode (parseJATS inp') :: [Maybe JATSElement])
  return $ parseJATS inp'
  where
    decodeLatin i = do
      converter <- ICU.open "latin-1" Nothing
      return (ICU.toUnicode converter i)

parseJATS :: Text -> JATSDoc
parseJATS = parseXML . Text.unpack
