{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Conduit
import qualified Data.ByteString.Char8 as ByteString
import           Data.FileEmbed
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Data.Text.ICU.Convert as ICU
import           Import
import           System.IO.Unsafe
import           Text.JaTex
import qualified Text.JaTex.CleanUp    as CleanUp
import           Yesod.Core.Types      (FileInfo (..))

placeholder :: Text
placeholder = unsafePerformIO $ do
    converter <- ICU.open "latin-1" Nothing
    return $ ICU.toUnicode converter $(embedFile "../examples/S0250-54602016000400001.xml")
{-# NOINLINE placeholder #-}

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo        :: FileInfo
    , fileDescription :: Text
    }

curlExample :: Text
curlExample = "curl " <> host <> " -F \"body=@./`echo ./something.xml`\""
  where
    host = "https://jats2tex.beijaflor.io"

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        setTitle "jats2tex - Convert JATS-XML to TeX"
        $(widgetFile "homepage")

postHomeR :: Handler TypedContent
postHomeR = do
  mfile <- runInputPost $ iopt fileField "body"
  (_filepath, body) <-
    case mfile of
      Just fi@FileInfo {fileName} -> do
        body <- sourceToList $ fileSource fi =$= decodeUtf8C :: Handler [Text]
        return (fileName, Text.unlines body)
      Nothing -> do
        body <- runInputPost $ ireq textField "text"
        return ("<none>", body)
  mstemplate <- runInputPost $ iopt textField "template"
  template <-
    case mstemplate of
      Just stemplate -> do
        template <- liftIO $ parseTemplate "<none>" (Text.encodeUtf8 stemplate)
        return (template, "<none>")
      Nothing -> return defaultTemplate
  !etex <-
    liftIO $ do
      jats <-
        parseJATS =<< CleanUp.cleanUpXML (Just "utf-8") (Text.encodeUtf8 body)
      jatsXmlToLaTeXText
        def
        { joTemplate = template
        , joInputFilePath = "none"
        , joInputDocument = jats
        }
  selectRep $ do
    provideRep $ return etex
    provideRep $ return $ object ["result" .= etex]

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
