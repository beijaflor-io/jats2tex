{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Text.JaTex.Template.TemplateInterp.Helpers
    ( intercalate
    , elements
    , alignToRagged
    , module Data.Maybe
    , module Data.Text
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
import qualified Data.Text
import           Text.LaTeX
-- import           Text.XML.Light

import           Text.JaTex.Template.Types
import           Text.JaTex.TexWriter

type TemplateHelper m = MonadTex m => m (LaTeXT Identity ())

intercalate :: MonadTex m => LaTeXT Identity () -> [LaTeXT Identity ()] -> TemplateHelper m
intercalate i t =
    return $ foldl (<>) mempty (Data.List.intercalate [i] (map (: []) t))

elements :: MonadTex m => TemplateContext -> m [LaTeXT Identity ()]
elements tc = do
  let el = tcElement tc
  r <- mapM convertInlineElem (elChildren el)
  let heads = concatMap fst r :: [LaTeXT Identity ()]
      bodies = concatMap snd r
  let ts = heads <> bodies
      latexs = map (render . snd . runIdentity . runLaTeXT) ts
      els = filter ((/= mempty) . Data.Text.strip . fst) (zip latexs ts)
  return (map snd els)

alignToRagged
  :: MonadTex m
  => TemplateContext -> TemplateHelper m
alignToRagged tc = do
  let el = tcElement tc
      align = lookupAttr "align" (elAttribs el)
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
      Just a -> begin a (sequence_ (fromString "\n":h <> inline))
      _      -> sequence_ (h <> inline)
