{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.JaTex where

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

import           JATSXML.Class
import           JATSXML.Types

type JATSDoc = [Content]

type JaTeXT m r = LaTeXT m r

type JaTeX m = JaTeXT m ()

runJaTeX
  :: Monad m
  => JaTeXT m a -> m (a, LaTeX)
runJaTeX = runLaTeXT

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

jatsXmlToLaTeXText :: JATSDoc -> Text
jatsXmlToLaTeXText cs =
  let (_, t) = runIdentity $ runLaTeXT $ jatsXmlToLaTeX cs
  in render t

jatsXmlToLaTeX
  :: Monad m
  => JATSDoc -> LaTeXT m ()
jatsXmlToLaTeX = mapM_ convertNode

convertNode
  :: Monad m
  => Content -> LaTeXT m ()
convertNode (Elem e) = convertElem e
convertNode (Text (CData CDataText str _)) = fromString str
convertNode (Text (CData _ str _)) =
  let cs = parseXML str
  in mapM_ convertNode cs
convertNode (CRef _) = return ()

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
    commentEl = return () -- comment (Text.pack (n <> ": " <> show elLine))
    run
      | n == "article" = do
        documentclass [] article
        convertChildren
      | True =
        case elContent of
          [] ->
            textell $
            TeXComm
              (n <> concatMap (\Attr {attrKey} -> showQName attrKey) elAttribs)
              []
          _ -> begin (Text.pack n) convertChildren
      | n == "body"
                -- comment "<body>"
       = document convertChildren
      | n == "front"
                -- comment (fromString (show el))
       = begin "front" convertChildren
      | n == "articlemeta"
                -- comment (fromString (show el))
       = begin "articlemeta" convertChildren
      | n == "titlegroup"
                -- comment (fromString (show el))
       = begin "titlegroup" convertChildren
            -- Output Nodes
      | n == "article-title" = do
        let lang = forM_ (lookupAttr' "xml:lang") fromString
        comm2 "articletitle" lang convertChildren
            -- Inline Formatting
      | n == "b" || n == "bold" = textbf convertChildren
      | n == "p" = paragraph convertChildren
      | n == "break" = newline
      | n == "code" || n == "codebold" = texttt convertChildren
      | otherwise = convertChildren

comm2
  :: LaTeXC l
  => String -> l -> l -> l
comm2 str = liftL2 $ \l1 l2 -> TeXComm str [FixArg l1, FixArg l2]

begin
  :: Monad m
  => Text -> LaTeXT m () -> LaTeXT m ()
begin n c =
  between c (raw ("\\begin{" <> n <> "}")) (raw ("\\end{" <> n <> "}"))
{-
convertNode :: Content -> [Block]
convertNode node = case node of
    Elem e -> jatsXmlToBlocks e
    Text (CData CDataText str _) -> [Plain [Str str]]
    Text (CData kind str _) ->
        let cs = parseXML str
        in concatMap convertNode cs
    CRef cref -> []

convertInline :: Content -> Inline
convertInline node = case node of
    Elem (Element{..}) -> case qName elName of
        "bold" ->
            (Strong $ map convertInline elContent)
        "b" ->
            (Strong $ map convertInline elContent)
        "font" ->
            (Span nullAttr $ map convertInline elContent)
        _ -> Span nullAttr (map convertInline elContent)
    Text (CData CDataText str _) -> Str str
    Text (CData kind str l) ->
        let cs = parseXML str
        in Span nullAttr (map convertInline cs)
    CRef cref -> Str (fromMaybe ("&" ++ cref ++ ";") (crefToString cref))

jatsXmlToBlocks :: Element -> [Block]
jatsXmlToBlocks Element{..}
    = case qName elName of
    "p" ->
        [ Para (map convertInline elContent)
        ]

    "abstract" ->
        [ RawBlock
             "latex"
             ("\\begin{abstract}" ++
                (writeLaTeX def (Pandoc (Meta Map.empty) (concatMap convertNode elContent))) ++
                "\\end{abstract}"
             )
         ]

    "title" ->
        [ RawBlock
             "latex"
             ( "\\renewcommand{\\abstractname{" ++ (writeLaTeX def (Pandoc (Meta Map.empty) (concatMap convertNode elContent))) ++ "}}"
             )
         ]

    "body" ->
        [ Div
             ( ""
             , []
             , [] -- map (\Attr{..} -> (show attrKey, attrVal)) elAttribs
             )
             (concatMap convertNode elContent)
         ]

    _ ->
        [ Div
             ( ""
             , []
             , [] -- map (\Attr{..} -> (show attrKey, attrVal)) elAttribs
             )
             (concatMap convertNode elContent)
         ]
 -}
