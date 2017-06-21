{-# LANGUAGE OverloadedStrings #-}
module Text.JaTex.Template.TemplateInterp
  where

import           Control.Monad.Identity
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Language.Haskell.Interpreter     as Hint
import           Text.LaTeX                       (LaTeXT, render)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer            as Lexer
import           Text.Megaparsec.Text

import           Text.JaTex.Template.Requirements
import           Text.JaTex.Template.Types
import           Text.JaTex.Util

type TemplateInterp = [TemplateInterpNode]
data TemplateInterpNode = TemplateVar Text
                        | TemplateExpr Text
                        | TemplatePlain Text
  deriving(Show)

evalNode
  :: Hint.MonadInterpreter m
  => TemplateContext -> TemplateInterpNode -> m Text
evalNode _ (TemplatePlain t) = return t
evalNode e (TemplateVar "heads") =
  return $ render (runLaTeX (sequence_ (tcHeads e)))
evalNode e (TemplateVar "bodies") =
  return $ render (runLaTeX (sequence_ (tcBodies e)))
evalNode e (TemplateVar "children") =
  return $ render (runLaTeX (sequence_ (tcChildren e)))
evalNode _ (TemplateVar "requirements") =
  return $ render (runLaTeX requirements)
evalNode _ (TemplateVar _) = return ""
evalNode e (TemplateExpr s) = do
  Hint.reset
  runner
    -- Hint.interpret
    --   (
    --   )
     <-
    Hint.interpret
      ("\\children -> do " <> Text.unpack s)
      (Hint.as :: [LaTeXT Identity ()] -> LaTeXT Identity ())
  return (render (runLaTeX (runner (tcChildren e))))

parseInterp
  :: String -> Text -> Either (ParseError Char Dec) TemplateInterp
parseInterp fp s = runIdentity $ runParserT interpParser fp s

interpParser :: Parser TemplateInterp
interpParser =
  manyTill (
  choice
    [ label "expr" (try interpExprParser)
    , label "var" (try interpVarParser)
    , label "plain" (try interpPlainParser)
    , eol >> return (TemplatePlain "\n")
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
--                            -- , "here"
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
