{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.JaTex.Template
  where

import           Control.Monad
import           Data.Aeson             (Value (..))
import qualified Data.ByteString
import           Data.Either
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import qualified Data.Yaml              as Yaml
import           System.Exit
import           System.IO
import           System.IO.Unsafe
import           Text.LaTeX
import           Text.LaTeX.Base.Parser
import           Text.XML.Light

data ConcreteTemplateNode = ConcreteTemplateNode
  { templateSelector :: Text
  , templateContent  :: Text
  } deriving (Show)

data TemplateNode = TemplateNode
  { templatePredicate :: Element -> Bool
  , templateApply     :: Element -> LaTeX
  }

parseTemplateNode :: ConcreteTemplateNode -> Either ParseError TemplateNode
parseTemplateNode ConcreteTemplateNode {..} = case latexContent of
    Right l -> Right TemplateNode
               { templatePredicate = \e -> showQName (elName e) == Text.unpack templateSelector
               , templateApply = const l
              }
    Left e -> Left e
  where
    latexContent = parseLaTeX templateContent

mergeEithers :: [Either a b] -> Either [a] [b]
mergeEithers [] = Right []
mergeEithers (Left e:es) = Left (e : lefts es)
mergeEithers (Right e:es) =
  case mergeEithers es of
    lfs@(Left _) -> lfs
    (Right rs)   -> Right (e : rs)

parseCTemplateFromJson :: Value -> Either [Text] [ConcreteTemplateNode]
parseCTemplateFromJson (Object o) =
  mergeEithers $ HashMap.foldrWithKey parsePair [] o
  where
    parsePair k (String v) m =
      Right ConcreteTemplateNode {templateSelector = k, templateContent = v} :
      m
    parsePair k _ m = Left ("Tipo inválido para chave `" <> k <> "`") : m
parseCTemplateFromJson _ = Left ["Template inválido, o formato esperado é `seletor: 'template'`"]

newtype Template =
  Template [(ConcreteTemplateNode, TemplateNode)]

instance Show Template where
    show (Template cs) = "Template " ++ show (map fst cs)

parseTemplateFile :: FilePath -> IO Template
parseTemplateFile fp = do
  v <- Yaml.decodeFile fp
  v' <-
    case v of
      Nothing -> error "Couldn't parse fp"
      Just i  -> return i
  case parseCTemplateFromJson v' of
    Left errs -> do
      forM_ errs $ \err -> Text.hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right cs ->
      case mergeEithers $ map parseTemplateNode cs of
        Left errs -> do
          forM_ errs $ \err -> Text.hPutStrLn stderr (Text.pack (show err))
          exitWith (ExitFailure 1)
        Right ns -> return $ Template $ zip cs ns

parseTemplate :: Data.ByteString.ByteString -> IO Template
parseTemplate s = do
  let mv = Yaml.decode s
  v <- case mv of
      Nothing -> error "Couldn't parse fp"
      Just i  -> return i
  case parseCTemplateFromJson v of
    Left errs -> do
      forM_ errs $ \err -> Text.hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right cs ->
      case mergeEithers $ map parseTemplateNode cs of
        Left errs -> do
          forM_ errs $ \err -> Text.hPutStrLn stderr (Text.pack (show err))
          exitWith (ExitFailure 1)
        Right ns -> return $ Template $ zip cs ns

defaultTemplate :: Template
defaultTemplate = unsafePerformIO $ parseTemplateFile "./default.yaml"
{-# NOINLINE defaultTemplate #-}

runTemplate :: Template -> Element -> Maybe ((ConcreteTemplateNode, TemplateNode), LaTeX)
runTemplate (Template []) _ = Nothing
runTemplate (Template (p@(_, TemplateNode{..}):ps)) el =
    if templatePredicate el
       then Just (p, templateApply el)
       else runTemplate (Template ps) el
