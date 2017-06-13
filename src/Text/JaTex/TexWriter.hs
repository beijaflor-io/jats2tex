{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Text.JaTex.TexWriter
  where

import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char              (isSpace)
import           Data.List
import           Data.Monoid
import qualified Data.Text              as Text
import           Debug.Trace
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax
import           Text.XML.Light

import           Text.JaTex.Parser
import qualified Text.JaTex.Upgrade     as Upgrade

type MonadTex m = MonadTexState m
type MonadTexState m = MonadState TexState m
type TexM = StateT TexState Identity

data TexState = TexState { tsFileName :: FilePath
                         , tsHead     :: LaTeXT Identity ()
                         , tsBody     :: LaTeXT Identity ()
                         }

data TexWriterCmd = AddHead (LaTeXT Identity ())
                  | AddBody (LaTeXT Identity ())

emptyState = TexState { tsBody = mempty
                      , tsHead = mempty
                      , tsFileName = ""
                      }

isHead (AddHead {}) = True
isHead _            = False

isBody (AddBody {}) = True
isBody _            = False

runLaTeX :: LaTeXT Identity () -> LaTeX
runLaTeX = snd . runIdentity . runLaTeXT

runTexWriter :: TexState -> TexM a -> (TexState, LaTeX)
runTexWriter st w =
    let (_, newState) = runState w st
        hCmds = tsHead newState
        bCmds = tsBody newState
        (_, r) = runIdentity $ runLaTeXT (hCmds <> bCmds)
    in (newState, r)

convert :: FilePath -> JATSDoc -> LaTeX
convert fp i = let (_, t) = (runTexWriter emptyState { tsFileName = fp
                                                     } (jatsXmlToLaTeX i))
               in t

jatsXmlToLaTeX
  :: MonadTex m
  => JATSDoc -> m ()
jatsXmlToLaTeX d = do
  add $ comment
    (Text.pack ("jats2tex@" <> Upgrade.versionNumber Upgrade.currentVersion))
  mapM_ convertNode d

convertNode
  :: MonadTex m
  => Content -> m ()
convertNode (Elem e) | traceShow "elem" True= do
  convertElem e
convertNode (Text (CData CDataText str _))
    | str == "" || dropWhile isSpace str == ""
      && traceShow "empty cdata" True
    =
      return ()
convertNode (Text (CData CDataText str _))
    | traceShow "cdata" True = do
  add $ fromString str
convertNode (Text (CData _ str ml))
    | traceShow "xml-cdata" True = do
  let cs = map (\e -> case e of
                       Elem e -> Elem e { elLine = ml
                                        }
                       _ -> e) (parseXML str)
  mapM_ convertNode cs
convertNode (CRef _) | traceShow "ref" True = return ()

addHead m = modify (\ts -> ts { tsHead = tsHead ts <> m
                              })

add m = modify (\ts -> ts { tsBody = tsBody ts <> m
                          })

convertElem
  :: MonadTex m
  => Element -> m ()
convertElem el@Element {..}
  | traceShow ("Entering", n) True = do
    commentEl
    run
  -- commentEndEl
  where
    lookupAttr' k =
      attrVal <$> find (\Attr {attrKey} -> showQName attrKey == k) elAttribs
    n = qName elName
    commentEl = do
      add $
        (comment
           (Text.pack
              (" <" <> n <> " " <> humanAttrs <> "> (" <> maybe "" show elLine <>
               ")")))
    humanAttrs =
      unwords $
      map
        (\(Attr attrKey attrValue) -> showQName attrKey <> "=" <> show attrValue)
        elAttribs
    -- commentEndEl =
    --   add $
    --   (comment (Text.pack ("</" <> n <> "> (" <> maybe "" show elLine <> "))")))
    run
      :: MonadTex m
      => m ()
    run
      | n == "article" = do
        add $ documentclass [] article
        add $ do
          fromString "\n"
          comment " jats2tex requirements"
          usepackage [] "keycommand"
          textell $ TeXRaw $ Text.unlines
            [ "% patch by Joseph Wright (\"bug in the definition of \\ifcommandkey (2010/04/27 v3.1415)\"),"
            , "% https://tex.stackexchange.com/a/35794/"
            , "\\begingroup"
            , "  \\makeatletter"
            , "  \\catcode`\\/=8 %"
            , "  \\@firstofone"
            , "    {"
            , "      \\endgroup"
            , "      \\renewcommand{\\ifcommandkey}[1]{%"
            , "        \\csname @\\expandafter \\expandafter \\expandafter"
            , "        \\expandafter \\expandafter \\expandafter \\expandafter"
            , "        \\kcmd@nbk \\commandkey {#1}//{first}{second}//oftwo\\endcsname"
            , "      }"
            , "    }"
            , "%=======================%"
            , "\\newkeycommand+[\\|]{\\transparentimage}[opacity][origkeys][1]"
            , "{%"
            , "  \\begingroup"
            , "  \\ifcommandkey{opacity}{|\\transparent|{\\commandkey{opacity}}}{}"
            , "    |\\includegraphics|[\\commandkey{origkeys}]{#1}"
            , "  \\endgroup%"
            , "}"
            ]
          fromString "\n"
        (head, inline) <- convertInlineChildren el
        add (begin "document" (head >> inline))
      | n == "article-title"
        -- let lang = forM_ (lookupAttr' "xml:lang") fromString
       = do
        (h, inline) <- convertInlineChildren el
        addHead $ title (h <> inline)
      | n == "contrib-group" -- && lookupAttr' "contrib-type" == Just "author" =
       = do
        inline <- mapM (\e -> convertInlineNode e) elContent
        addHead $
          author $ forM_ (intercalate [comm0 "and"] (map (: []) inline)) id
      | n == "body" = do convertChildren el
      | n == "font" = do
          (h, i) <- convertInlineChildren el
          let prelude = case lookupAttr' "size" of
                  Just fz -> comm1 "fontsize" (fromString (fz <> "pt"))
                  _       -> return ()
          add $ textell (TeXBraces (runLaTeX (prelude <> h <> i)))
      | n == "contrib" = convertChildren el
      | n == "back" = return ()
      | n == "abstract" = do
        inline <- convertInlineNode (head elContent)
        addHead $ comm1 "abstract" inline
      | n == "name" = convertChildren el
      -- | n == "img" =
      --   add $
      --   textell $ TeXComm "img" (map (OptArg . fromString) (words humanAttrs))
      | n == "surname" = do
        convertChildren el
        add $ fromString ","
      | n == "given-names" = convertChildren el
      | n == "kwd" = do
        (_, inline) <- convertInlineChildren el
        add $ textit inline
      | n == "b" || n == "bold" = do
        (h, inline) <- convertInlineChildren el
        add h
        add $ textbf inline
      | n == "p" = do
        (h, inline) <- convertInlineChildren el
        add $ paragraph (h <> inline)
      | n == "break" = add $ newline
      | n == "code" || n == "codebold" = do
        (h, inline) <- convertInlineChildren el
        add $ h <> texttt inline
      | n == "?xml" = return ()
      | "?" `isPrefixOf` n = return ()
      | otherwise =
        case elContent of
          [] -> return ()
            -- add
            --   (textell
            --      (TeXComm
            --         (protectString n)
            --         ((map (OptArg . fromString . removeSpecial) $
            --          words humanAttrs))))
          _ -> do
            convertChildren el
            -- inline <- convertInlineChildren el
            -- add $ begin (Text.pack n) inline

removeSpecial = map (\c -> if c == ':'
                        then '-'
                        else c)

convertInlineNode :: MonadTex m => Content -> m (LaTeXT Identity ())
convertInlineNode c = do
    st <- get
    let (newState, _) = runTexWriter (st { tsHead = mempty
                                         , tsBody = mempty
                                         }) (convertNode c)
    return (tsBody newState)

convertInlineChildren :: MonadTex m => Element -> m (LaTeXT Identity (), LaTeXT Identity ())
convertInlineChildren el = do
    st <- get
    let (newState, _) = runTexWriter (st { tsHead = mempty
                                         , tsBody = mempty
                                         }) (convertChildren el)
    return (tsHead newState, tsBody newState)

convertChildren :: MonadTex m => Element -> m ()
convertChildren Element{elContent} = mapM_ convertNode elContent

comm2
  :: LaTeXC l
  => String -> l -> l -> l
comm2 str = liftL2 $ \l1 l2 -> TeXComm str [FixArg l1, FixArg l2]

begin
  :: Monad m
  => Text -> LaTeXT m () -> LaTeXT m ()
begin n c = between c (raw ("\\begin{" <> n <> "}")) (raw ("\\end{" <> n <> "}"))
