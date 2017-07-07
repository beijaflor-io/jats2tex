{-# LANGUAGE OverloadedStrings #-}
module Text.JaTex.Template.Requirements where

import           Text.LaTeX

requirements :: Monad m => LaTeXT m ()
requirements = do
  usepackage ["document"] "ragged2e"
  fromString "\n"
  usepackage [] "amssymb"
