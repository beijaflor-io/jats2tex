module Text.JaTex.TexWriter
  where

import           Control.Monad.Writer

type TexWriterT m l a = WriterT m l a

-- addHead = tell [ AddHead
--                ]
-- addBody = undefined
