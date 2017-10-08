{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE LambdaCase            #-}
-- | Exposes parsing interface for reading template files
module Text.JaTex.ConcreteTemplateWrapper
  where


import           Control.Monad.Writer
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as ByteStringS
import qualified Data.Yaml                           as Yaml
import           Text.JaTex.Template.Types


parseConcreteTemplateByteString :: FilePath -> Data.ByteString.ByteString -> IO ConcreteTemplateWrapper
parseConcreteTemplateByteString _ s = do
  let v = Yaml.decodeEither s
  case v of
    Left err -> error $ "Couldn't parse " <> (show err)
    Right i  -> return i

parseConcreteTemplateFile :: FilePath -> IO ConcreteTemplateWrapper
parseConcreteTemplateFile fp = parseConcreteTemplateByteString fp =<< ByteStringS.readFile fp

parseConcreteTemplateWrapperFile :: FilePath -> IO (ConcreteTemplateWrapper)
parseConcreteTemplateWrapperFile fp = Yaml.decodeFileEither fp >>= \case
  Left err -> error (show err)
  Right v -> return v
