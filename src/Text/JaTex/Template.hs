{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.JaTex.Template
  where

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Data.Aeson                         (Result (..), Value (..),
                                                     fromJSON)
import qualified Data.ByteString
import           Data.Either
import qualified Data.HashMap.Strict                as HashMap
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.IO                       as Text
import           Data.Yaml                          ()
import qualified Data.Yaml                          as Yaml
import qualified Language.Haskell.Interpreter       as Hint
import           System.Exit
import           System.IO
import           System.IO.Unsafe
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import qualified Text.LaTeX.Base.Parser             as LaTeX
import           Text.LaTeX.Base.Syntax
import           Text.Megaparsec
import           Text.XML.Light

import           Text.JaTex.Template.TemplateInterp
import           Text.JaTex.Template.Types
import           Text.JaTex.Util

