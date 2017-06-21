module Text.JaTex ( readJats
                  , parseJATS
                  , defaultTemplate
                  , Template
                  , parseTemplateFile
                  , jatsXmlToLaTeXText
                  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Typeable
import           Text.LaTeX

import           Text.JaTex.Parser
import           Text.JaTex.Template
import           Text.JaTex.Template.Types
import           Text.JaTex.TexWriter

jatsXmlToLaTeXText :: (Typeable m, MonadIO m, MonadMask m) => FilePath -> Template -> JATSDoc -> m Text
jatsXmlToLaTeXText fp tmp cs = do
  t <- convert fp tmp cs
  return $ render t
