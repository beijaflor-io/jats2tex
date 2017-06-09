{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.JaTex ( readJats
                  , parseJATS
                  , jatsXmlToLaTeXText
                  ) where

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Writer
import qualified Data.ByteString        as ByteString
import           Data.List
import qualified Data.Text              as Text
import qualified Data.Text.ICU.Convert  as ICU
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax
import           Text.XML.Light

import           Text.JaTex.Parser
import qualified Text.JaTex.Upgrade     as Upgrade

type JaTeXT m r = LaTeXT m r
type JaTeX m = JaTeXT m ()

runJaTeX
  :: Monad m
  => JaTeXT m a -> m (a, LaTeX)
runJaTeX = runLaTeXT

jatsXmlToLaTeXText :: JATSDoc -> Text
jatsXmlToLaTeXText cs =
  let (_, t) = runIdentity $ runLaTeXT $ jatsXmlToLaTeX cs
  in render t

jatsXmlToLaTeX
  :: Monad m
  => JATSDoc -> LaTeXT m ()
jatsXmlToLaTeX d = do
  comment
    (Text.pack ("jats2tex@" <> Upgrade.versionNumber Upgrade.currentVersion))
  mapM_ convertNode d

convertNode
  :: Monad m
  => Content -> LaTeXT m ()
convertNode (Elem e) = do
  comment "recursive elem"
  convertElem e
convertNode (Text (CData CDataText str _)) = do
  comment "cdata (text-like)"
  fromString str
convertNode (Text (CData _ str _)) = do
  comment "cdata (not-text-like)"
  let cs = parseXML str
  mapM_ convertNode cs
convertNode (CRef _) = do
  comment "cref"
  return ()

convertElem
  :: Monad m
  => Element -> LaTeXT m ()
convertElem _el@Element {..} = do
  commentEl
  run
  where
    lookupAttr' k =
      attrVal <$> find (\Attr {attrKey} -> showQName attrKey == k) elAttribs
    n = qName elName
    convertChildren = mapM_ convertNode elContent
    commentEl =
      comment (Text.pack ("<" <> n <> "> (" <> maybe "" show elLine <> ")"))
    run
      | n == "article" = do
        documentclass [] article
        convertChildren
      | n == "contrib-group" -- && lookupAttr' "contrib-type" == Just "author" =
       = do
        author $ forM_ (intersperse (comm0 "and") (map convertNode elContent)) id
      | n == "contrib" = convertChildren
      | n == "name" = convertChildren
      | n == "surname" = do
        convertChildren
        fromString ","
      | n == "given-names" = convertChildren
      | n == "article-title" = do
        let lang = forM_ (lookupAttr' "xml:lang") fromString
        comm2 "article-title" lang convertChildren
        title convertChildren
      | n == "b" || n == "bold" = textbf convertChildren
      | n == "p" = paragraph convertChildren
      | n == "break" = newline
      | n == "code" || n == "codebold" = texttt convertChildren
      | n == "?xml" = return ()
      | "?" `isPrefixOf` n = return ()
      | otherwise =
        case elContent of
          [] ->
            textell $
            TeXComm
              (n <> concatMap (\Attr {attrKey} -> showQName attrKey) elAttribs)
              []
          _ -> begin (Text.pack n) convertChildren

comm2
  :: LaTeXC l
  => String -> l -> l -> l
comm2 str = liftL2 $ \l1 l2 -> TeXComm str [FixArg l1, FixArg l2]

begin
  :: Monad m
  => Text -> LaTeXT m () -> LaTeXT m ()
begin n c =
  between c (raw ("\\begin{" <> n <> "}")) (raw ("\\end{" <> n <> "}"))
