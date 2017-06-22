{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.JaTex.Template.TemplateInterp.Helpers
    ( intercalate
    , elements
    , alignToRagged
    , module Data.Maybe
    , module Text.JaTex.Template.Types
    , module Control.Monad.Identity
    , module Control.Monad.State
    , module Text.LaTeX
    )
  where

import           Prelude

import           Control.Monad.Identity    (Identity, runIdentity)
import           Control.Monad.State       (StateT (..))
import qualified Data.List
import           Data.Maybe
import           Text.LaTeX
import           Text.XML.Light

import           Text.JaTex.Template.Types
import           Text.JaTex.TexWriter

intercalate :: Monad m => LaTeXT Identity () -> [LaTeXT Identity ()] -> m (LaTeXT Identity ())
intercalate i t =
    return $ foldl (<>) mempty (Data.List.intercalate [i] (map (: []) t))

elements :: MonadTex m => TemplateContext -> m [LaTeXT Identity ()]
elements tc = do
    let el = tcElement tc
    r <- mapM convertInlineElem (elChildren el)
    let heads = concatMap fst r :: [LaTeXT Identity ()]
        bodies = concatMap snd r
    liftIO (print (elChildren el))
    liftIO (print (map (render . snd . runIdentity . runLaTeXT) (heads <> bodies)))
    return (filter ((/= mempty) . snd . runIdentity . runLaTeXT) (heads <> bodies))

-- test = do
--   r <- intercalate (raw "and") [ textbf "1"
--                              , textbf "2"
--                              ]
--   print (runIdentity . runLaTeXT $ r)

alignToRagged
  :: MonadTex m
  => TemplateContext -> m (LaTeXT Identity ())
alignToRagged tc = do
  let el = tcElement tc
      align = lookupAttr (QName "align" Nothing Nothing) (elAttribs el)
  (h, inline) <- convertInlineChildren el
  return $ do
    let ma :: Maybe Text
        ma =
          case fromMaybe "justify" align of
            "center" -> Just "Center"
            "left"   -> Just "FlushLeft"
            "right"  -> Just "FlushRight"
            _        -> Nothing
    case ma of
      Just _  -> sequence_ (h <> inline)
      Nothing -> sequence_ (h <> inline)
