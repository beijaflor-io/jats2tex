{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.JaTex.Template.TemplateInterp
  where

import           Control.Monad.Identity
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer     as Lexer
import           Text.Megaparsec.Text

import           Text.JaTex.Template.Types

parseInterp
  :: String -> Text -> Either (ParseError Char Dec) TemplateInterp
parseInterp fp s = runIdentity $ runParserT interpParser fp s

interpParser :: Parser TemplateInterp
interpParser =
  manyTill (
  choice
    [ label "expr" (try interpExprParser)
    , label "lua" (try luaParser)
    , label "var" (try interpVarParser)
    , label "plain" interpPlainParser
    ]) eof

whitespaceConsumer :: Parser ()
whitespaceConsumer = return ()

symbol :: String -> Parser String
symbol = Lexer.symbol whitespaceConsumer

interpExprParser :: Parser TemplateInterpNode
interpExprParser =
  TemplateExpr . Text.pack <$> do
    _ <- symbol "@@("
    someTill anyChar (symbol ")@@")

interpVarParser :: Parser TemplateInterpNode
interpVarParser =
  TemplateVar . Text.pack <$> do
    _ <- symbol "@@"
    someTill letterChar (lookAhead (notFollowedBy letterChar))

luaParser :: Parser TemplateInterpNode
luaParser =
  TemplateLua . Text.pack <$> do
    _ <- symbol "@@lua("
    someTill anyChar (symbol ")@@")

interpPlainParser :: Parser TemplateInterpNode
interpPlainParser =
  TemplatePlain . Text.pack <$>
  manyTill anyChar (lookAhead (void (symbol "@@") <|> eof))

-- testvar :: IO ()
-- testvar = do
--   let input = Text.unlines [ "@@here"
--                            -- , "here"
--                            ]
--   parseTest interpParser input

-- testreal :: IO ()
-- testreal = do
--   let input = Text.unlines [ "\\textbf{@@children}"
--                            , "@@("
--                            , "  interpolate \"stuff\" 3"
--                            , ")@@"
--                            ]
--   parseTest interpParser input

-- testvar2 :: IO ()
-- testvar2 = do
--   let input = Text.unlines [ "something @@here"
--                            -- , "here"
--                            ]
--   let result = runParserT interpParser "<none>" input
--   print result

-- testvar3 :: IO ()
-- testvar3 = do
--   let input = Text.unlines [ "@@here  something"
--                            ]
--   let result = runParserT interpParser "<none>" input
--   print result

-- testexpr :: IO ()
-- testexpr = do
--   let input = Text.unlines [ "@@(1 + 2 + 3)@@"
--                            , "something @@("
--                            , "123)@@"
--                            ]
--   let result = runParserT interpParser "<none>" input
--   print result

-- test :: IO ()
-- test = do
--   let input = Text.unlines [ "something @@here"
--                            , "something @@(1 + 2 + 3)@@"
--                            ]
--   let result = runParserT interpParser "<none>" input
--   print result
