{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Text.JaTex.Template
  where

import Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad.State
import           TH.RelativePaths

import Text.JaTex.Template.Types

parseTemplateNode :: ConcreteTemplateNode -> IO (Either Text (TemplateNode (StateT TexState IO)))
parseTemplateNode ConcreteTemplateNode {..} = do
  preparedH <- liftIO $ prepareInterp templateHead
  preparedL <- liftIO $ prepareInterp templateContent
  return $
    Right
      TemplateNode
      { templatePredicate = Text.unpack templateSelector
      , templateLaTeXHead = preparedH
      , templateLaTeX = preparedL
      }

defaultTemplateContents :: ByteString
defaultTemplateContents = $(bsToExp =<< qReadFileBS "./default.yaml")

defaultTemplate :: (Template, FilePath)
defaultTemplate = unsafePerformIO $ do
    let s = defaultTemplateContents
        fp = "default.yaml"
    t <- parseTemplate fp s
    return (t, fp)
{-# NOINLINE defaultTemplate #-}
