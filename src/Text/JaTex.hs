module Text.JaTex ( readJats
                  , parseJATS
                  , jatsXmlToLaTeXText
                  ) where

import           Control.Monad.IO.Class
import           Text.LaTeX

import           Text.JaTex.Parser
import           Text.JaTex.Template
import           Text.JaTex.TexWriter

jatsXmlToLaTeXText :: MonadIO m => FilePath -> Template -> JATSDoc -> m Text
jatsXmlToLaTeXText fp tmp cs = do
  t <- convert fp tmp cs
  return $ render t
