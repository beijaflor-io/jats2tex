{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.JaTex.Template.Types where

import           Control.Applicative
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
import qualified Scripting.Lua          as Lua
import           Text.LaTeX
-- import           Text.XML.Light
import           Text.XML.HXT.Core

type ExprType m = MonadTex m =>
                    TemplateContext -> [LaTeXT Identity ()] -> (Text -> m (LaTeXT Identity ())) -> m (LaTeXT Identity ())

type LuaExprType m = MonadTex m => TemplateContext -> ([LaTeXT Identity ()], [LaTeXT Identity ()]) -> m (LaTeXT Identity ())

type TemplateInterp = [TemplateInterpNode]
data TemplateInterpNode = TemplateVar Text
                        | TemplateExpr Text
                        | TemplateLua Text
                        | TemplatePlain Text
  deriving(Show)

type PreparedTemplate m = [PreparedTemplateNode m]
data PreparedTemplateNode m
  = PreparedTemplateVar Text
  | PreparedTemplateExpr (ExprType m)
  | PreparedTemplateLua (LuaExprType m)
  | PreparedTemplatePlain Text

type ConcreteTemplate = [ConcreteTemplateNode]

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
      _        -> fail "Invalid Template"
    where
      trimTrailingNewline "" = ""
      trimTrailingNewline i =
        if Text.last i == '\n'
          then Text.init i
          else i
      verboseForm o =
        ConcreteTemplateNode "" <$>
        (trimTrailingNewline . fromMaybe "" <$> o .:? "head") <*>
        (trimTrailingNewline . fromMaybe "" <$> (o .:? "body" <|> o .:? "content"))

data TemplateContext = TemplateContext
  { tcElement  :: XmlTree
  , tcState    :: TexState
  , tcLuaState :: Lua.LuaState
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
  , tsWarnings :: Bool
  } deriving (Typeable)

type NodeSelector = String
