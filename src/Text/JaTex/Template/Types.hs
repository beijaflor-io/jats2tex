{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.JaTex.Template.Types where

import           Control.Monad.Identity
import           Data.Maybe
import           Data.Text              (Text)
import           Data.Yaml
import qualified Data.Yaml              as Yaml
import           Text.LaTeX
import           Text.XML.Light

data ConcreteTemplateNode = ConcreteTemplateNode
  { templateSelector :: Text
  , templateHead     :: Text
  , templateContent  :: Text
  } deriving (Show)

instance Yaml.FromJSON ConcreteTemplateNode where
  parseJSON (String s) = return $ ConcreteTemplateNode "" "" s
  parseJSON (Object o) = verboseForm
    where
      verboseForm =
        ConcreteTemplateNode "" <$> (fromMaybe "" <$> o .:? "head") <*>
        (fromMaybe "" <$> o .:? "content")
  parseJSON _ = fail "Template inválido"

data TemplateNode = TemplateNode
  { templatePredicate :: !NodeSelector
  , templateLaTeXHead :: !LaTeX
  , templateLaTeX     :: !LaTeX
  }

data TemplateContext = TemplateContext
  { tcHeads   :: [LaTeXT Identity ()]
  , tcBodies  :: [LaTeXT Identity ()]
  , tcElement :: Element
  }

tcChildren :: TemplateContext -> [LaTeXT Identity ()]
tcChildren e = tcHeads e <> tcBodies e

type NodeSelector = String
