module Text.JaTex ( readJats
                  , parseJATS
                  , jatsXmlToLaTeXText
                  ) where

import           Text.LaTeX

import           Text.JaTex.Parser
import           Text.JaTex.TexWriter

jatsXmlToLaTeXText :: FilePath -> JATSDoc -> Text
jatsXmlToLaTeXText fp cs =
  let t = convert fp cs
  in render t
