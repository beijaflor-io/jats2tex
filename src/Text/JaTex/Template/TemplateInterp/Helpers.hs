{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.JaTex.Template.TemplateInterp.Helpers
    ( intercalate
    , elements
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
import           Debug.Trace
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
    return $ heads <> bodies

-- test = do
--   r <- intercalate (raw "and") [ textbf "1"
--                              , textbf "2"
--                              ]
--   print (runIdentity . runLaTeXT $ r)

-- alignToRaggedEnv = undefined

