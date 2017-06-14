{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.JaTex.Template
  where

import           Data.Aeson             (Value (..))
import           Data.Either
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Text.LaTeX
import           Text.LaTeX.Base.Parser
import           Text.XML.Light

data ConcreteTemplateNode = ConcreteTemplateNode
  { templateSelector :: Text
  , templateContent  :: Text
  }

data TemplateNode = TemplateNode
  { templatePredicate :: Element -> Bool
  , templateApply     :: Element -> LaTeX
  }

parseTemplate :: ConcreteTemplateNode -> Either ParseError TemplateNode
parseTemplate ConcreteTemplateNode {..} = case latexContent of
    Right l -> Right TemplateNode
               { templatePredicate = \e -> showQName (elName e) == Text.unpack templateSelector
               , templateApply = const l
              }
    Left e -> Left e
  where
    latexContent = parseLaTeX templateContent

parseCTemplateFromJson :: Value -> Either [Text] [ConcreteTemplateNode]
parseCTemplateFromJson (Object o) =
  mergeEithers $ HashMap.foldrWithKey parsePair [] o
  where
    mergeEithers :: [Either a b] -> Either [a] [b]
    mergeEithers [] = Right []
    mergeEithers (Left e:es) = Left (e : lefts es)
    mergeEithers (Right e:es) =
      case mergeEithers es of
        lfs@(Left _) -> lfs
        (Right rs)   -> Right (e : rs)
    parsePair k (String v) m =
      Right ConcreteTemplateNode {templateSelector = k, templateContent = v} :
      m
    parsePair k _ m = Left ("Tipo inválido para chave `" <> k <> "`") : m
parseCTemplateFromJson _ = Left ["Template inválido, o formato esperado é `seletor: 'template'`"]
