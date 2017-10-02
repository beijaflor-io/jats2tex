{-# LANGUAGE OverloadedStrings #-}
module Text.JaTex.TemplateWrapper
  where

import           Data.Aeson
import           Data.Text                 (Text)
import qualified Data.Yaml                 as Yaml
import           Text.JaTex.Template.Types (ConcreteTemplate)

data TemplateWrapper = TemplateWrapper
  { templateWrapperVersion :: Int
  , templateWrapperExtends :: (Maybe Text)
  , templateWrapperRules   :: ConcreteTemplate
  }

instance Yaml.FromJSON TemplateWrapper where
  parseJSON (Object o) =
    TemplateWrapper <$>
    o .: "version" <*>
    o .:? "extends" <*>
    o .: "rules"
  parseJSON _ = fail "Invalid Template"
