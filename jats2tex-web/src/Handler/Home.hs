{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Conduit
import           Data.FileEmbed
import qualified Data.Text             as Text
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Text.IO          as Text
import           Import
import           System.IO.Unsafe
import           Text.JaTex
import           Text.Julius           (RawJS (..))
import           Yesod.Core.Types      (FileInfo (..))
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)

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
getHomeR = do
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        formId <- newIdent
        setTitle "jats2tex - Convert JATS-XML to TeX"
        $(widgetFile "homepage")

postHomeR :: Handler Text
postHomeR = do
  -- mfile <- lookupPostParam "body"
  mfile <- runInputPost $ iopt fileField "body"
  (filepath, body) <-
    case mfile of
      Just fi@FileInfo {fileName} -> do
        body <- sourceToList $ fileSource fi =$= decodeUtf8C :: Handler [Text]
        return (fileName, Text.unlines body)
      Nothing -> do
        body <- runInputPost $ ireq textField "text"
        return ("<none>", body)
  !etex <-
    liftIO $
    jatsXmlToLaTeXText
      def {joInputFilePath = "none", joInputDocument = parseJATS body}
  return etex

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
