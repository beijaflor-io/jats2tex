{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.JaTex.Template.Types where

import           Control.Monad.Catch
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.HashMap.Strict    (HashMap)
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable
import           Data.Yaml
import qualified Data.Yaml              as Yaml
import           Text.LaTeX
import           Text.XML.Light

type ExprType m = MonadTex m =>
                    TemplateContext -> [LaTeXT Identity ()] -> (Text -> m (LaTeXT Identity ())) -> m (LaTeXT Identity ())

type TemplateInterp = [TemplateInterpNode]
data TemplateInterpNode = TemplateVar Text
                        | TemplateExpr Text
                        | TemplatePlain Text
  deriving(Show)

type PreparedTemplate m = [PreparedTemplateNode m]
data PreparedTemplateNode m
  = PreparedTemplateVar Text
  | PreparedTemplateExpr (ExprType m)
  | PreparedTemplatePlain Text


data ConcreteTemplateNode = ConcreteTemplateNode
  { templateSelector :: Text
  , templateHead     :: Text
  , templateContent  :: Text
  } deriving (Show)

instance Yaml.FromJSON ConcreteTemplateNode where
  parseJSON v =
    case v of
      String s -> return $ ConcreteTemplateNode "" "" s
      Object o -> verboseForm o
      _        -> fail "Template inválido"
    where
      trimTrailingNewline "" = ""
      trimTrailingNewline i =
        if Text.last i == '\n'
          then Text.init i
          else i
      verboseForm o =
        ConcreteTemplateNode "" <$>
        (trimTrailingNewline . fromMaybe "" <$> o .:? "head") <*>
        (trimTrailingNewline . fromMaybe "" <$> o .:? "content")

data TemplateContext = TemplateContext
  { tcHeads   :: [LaTeXT Identity ()]
  , tcBodies  :: [LaTeXT Identity ()]
  , tcElement :: Element
  , tcState   :: TexState
  }

data TemplateNode m = TemplateNode
  { templatePredicate :: !NodeSelector
  , templateLaTeXHead :: PreparedTemplate m
  , templateLaTeX     :: PreparedTemplate m
  }

newtype Template =
  Template [(ConcreteTemplateNode, TemplateNode (StateT TexState IO))]

type MonadTex m = (MonadCatch m, MonadState TexState m, MonadIO m, MonadMask m)
type TexM = StateT TexState Identity


data TexState = TexState
  { tsFileName :: FilePath
  , tsDebug    :: Bool
  , tsTemplate :: (Template, FilePath)
  , tsMetadata :: HashMap Text Text
  , tsHeadRev  :: [LaTeXT Identity ()]
  , tsBodyRev  :: [LaTeXT Identity ()]
  } deriving (Typeable)


tcChildren :: TemplateContext -> [LaTeXT Identity ()]
tcChildren e = tcHeads e <> tcBodies e

type NodeSelector = String
