{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.JaTex ( readJats
                  , parseJATS
                  , jatsXmlToLaTeXText
                  ) where

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Writer
import qualified Data.ByteString         as ByteString
import           Data.List
import qualified Data.Text               as Text
import qualified Data.Text.ICU.Convert   as ICU
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax
import           Text.XML.Light

import           Text.JaTex.JatsElements
import           Text.JaTex.Parser
import           Text.JaTex.TexWriter
import qualified Text.JaTex.Upgrade      as Upgrade

jatsXmlToLaTeXText :: FilePath -> JATSDoc -> Text
jatsXmlToLaTeXText fp cs =
  let t = convert fp cs
  in render t
