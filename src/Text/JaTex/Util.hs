module Text.JaTex.Util where

import           Control.Monad.Identity
import           Text.LaTeX

runLaTeX :: LaTeXT Identity a -> LaTeX
runLaTeX = snd . runIdentity . runLaTeXT
