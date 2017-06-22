{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.JaTex ( readJats
                  , parseJATS
                  , defaultTemplate
                  , Default(..)
                  , JaTexOptions(..)
                  , Template
                  , parseTemplateFile
                  , jatsXmlToLaTeXText
                  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Monoid
import qualified Data.Text                 as Text
import           Text.LaTeX

import           Text.JaTex.Parser
import           Text.JaTex.Template.Types
import           Text.JaTex.TexWriter

-- https://gist.github.com/yiannist/4546899
wrap :: Int -> Text -> [Text]
wrap maxWidth text | maxWidth <= 0 = error "wrap: maxWidth too small"
                   | otherwise = Prelude.reverse (lastLine : accLines)
  where (accLines, lastLine) = Prelude.foldl handleWord ([], "") (Text.words text)
        handleWord :: ([Text], Text) -> Text -> ([Text], Text)
        handleWord (acc, line) word
            | Text.length lineWithWord <= maxWidth
                = (acc, lineWithWord) -- 'word' fits fine; append to 'line' and continue.
            | Text.length word > maxWidth = -- 'word' doesn't fit anyway; split awkwardly.
                let (rest:intermediateLines) = Prelude.reverse $ Text.chunksOf maxWidth (line `addWord` word)
                in  (intermediateLines <> acc, rest)
            | otherwise
                = (line : acc, word) -- 'line' is full; start with 'word' and continue.
          where lineWithWord = line `addWord` word
        addWord :: Text -> Text -> Text
        addWord "" word   = word
        addWord line word = line <> " " <>  word

wrapLines :: Int -> Text -> [Text]
wrapLines w | w <= 0 = Text.lines
wrapLines w = concatMap (wrap w) . Text.lines

data JaTexOptions = JaTexOptions { joInputFilePath :: FilePath
                                 , joTemplate      :: (Template, FilePath)
                                 , joMaxWidth      :: Int
                                 , joInputDocument :: JATSDoc
                                 }

instance Default JaTexOptions where
    def = JaTexOptions { joInputFilePath = "<unknown>"
                       , joTemplate = defaultTemplate
                       , joMaxWidth = 80
                       , joInputDocument = mempty
                       }

trimConsecutiveEmptyLines :: [Text] -> Text
trimConsecutiveEmptyLines = Text.unlines . fst . foldr helper ([], True)
  where
    helper "" (acc, lastLineWasNewline) =
      ( if lastLineWasNewline
          then acc
          else "" : acc
      , True)
    helper t (acc, _) = (t : acc, False)

jatsXmlToLaTeXText :: (MonadIO m, MonadMask m) => JaTexOptions -> m Text
jatsXmlToLaTeXText JaTexOptions{..} = do
  t <- convert joInputFilePath joTemplate joInputDocument
  return (trimConsecutiveEmptyLines (wrapLines joMaxWidth (render t)))
