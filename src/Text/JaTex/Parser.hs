module Text.JaTex.Parser
  where

import qualified Data.ByteString       as ByteString
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.ICU.Convert as ICU
import           Text.XML.Light

-- import           Text.XML.HaXml

type JATSDoc = [Content]

readJatsFile :: FilePath -> IO Text
readJatsFile inputFile = do
  inp <- ByteString.readFile inputFile
  decodeLatin inp
  where
    decodeLatin i = do
      converter <- ICU.open "latin-1" Nothing
      return (ICU.toUnicode converter i)

readJats :: FilePath -> IO JATSDoc
readJats fp = parseJATS <$> readJatsFile fp

parseJATS :: Text -> JATSDoc
parseJATS = parseXML . Text.unpack
