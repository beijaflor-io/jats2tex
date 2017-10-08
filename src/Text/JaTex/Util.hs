module Text.JaTex.Util where

import           Control.Monad.Identity
import           Data.Either

import           Text.LaTeX

runLaTeX :: LaTeXT Identity a -> LaTeX
runLaTeX = snd . runIdentity . runLaTeXT

-- | Aggregates list of 'Rights' and 'Lefts' onto a 'Right' and 'Left' of lists
-- of values
mergeEithers :: [Either a b] -> Either [a] [b]
mergeEithers [] = Right []
mergeEithers (Left e:es) = Left (e : lefts es)
mergeEithers (Right e:es) =
  case mergeEithers es of
    lfs@(Left _) -> lfs
    (Right rs)   -> Right (e : rs)
