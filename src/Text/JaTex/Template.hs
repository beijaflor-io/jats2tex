{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.JaTex.Template
  where

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Data.Aeson                         (Result (..), Value (..),
                                                     fromJSON)
import qualified Data.ByteString
import           Data.Either
import qualified Data.HashMap.Strict                as HashMap
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.IO                       as Text
import           Data.Yaml                          ()
import qualified Data.Yaml                          as Yaml
import qualified Language.Haskell.Interpreter       as Hint
import           System.Exit
import           System.IO
import           System.IO.Unsafe
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import qualified Text.LaTeX.Base.Parser             as LaTeX
import           Text.LaTeX.Base.Syntax
import           Text.Megaparsec
import           Text.XML.Light

import           Text.JaTex.Template.TemplateInterp
import           Text.JaTex.Template.Types
import           Text.JaTex.Util

import           Debug.Trace

templateApply
  :: Hint.MonadInterpreter m
  => TemplateNode
  -> TemplateContext
  -> m (LaTeXT Identity (), LaTeXT Identity ())
templateApply TemplateNode {templateLaTeX, templateLaTeXHead} tc
  | traceShow "templateApply" True =
    (,) <$> applyTemplateToEl templateLaTeXHead tc <*>
    applyTemplateToEl templateLaTeX tc

runPredicate :: NodeSelector -> NodeSelector -> Bool
runPredicate s t
  | traceShow ("runPredicate") True = t == s

parseTemplateNode :: ConcreteTemplateNode -> Either Text TemplateNode
parseTemplateNode ConcreteTemplateNode {..} =
  case latexContent of
    Right l ->
      case templateHead of
        "" ->
          Right
            TemplateNode
            { templatePredicate = Text.unpack templateSelector
            , templateLaTeXHead = mempty
            , templateLaTeX = l
            }
        _ ->
          case LaTeX.parseLaTeX templateHead of
            Right h ->
              Right
                TemplateNode
                { templatePredicate = Text.unpack templateSelector
                , templateLaTeXHead = h
                , templateLaTeX = l
                }
            Left _ -> Left "Failed to parse template head"
    Left _ -> Left "Failed to parse template content"
  where
    latexContent = LaTeX.parseLaTeX templateContent

applyTemplateToEl :: Hint.MonadInterpreter m => LaTeX -> TemplateContext -> m (LaTeXT Identity ())
applyTemplateToEl l e =
  case traceShow ("applyTemplateToEl", render l) l of
    TeXLineBreak m b -> return $ fromLaTeX $ TeXLineBreak m b
    TeXBraces i -> braces <$> recur i
    TeXMath mt i -> liftL (TeXMath mt) <$> recur i
    TeXEnv ev as i -> do
      ev' <- runReplacementsS ev
      as' <- recurArgs as
      i' <- recur i
      return $ liftL (TeXEnv ev' as') i'
    TeXCommS c -> do
      c' <- runReplacementsS c
      return $ textell (TeXCommS c')
    TeXComm c as -> do
      c' <- runReplacementsS c
      as' <- recurArgs as
      return $ fromLaTeX (TeXComm c' as')
    TeXRaw lr -> do
      lr' <- runReplacements lr
      return $ fromLaTeX $ TeXRaw lr'
    TeXComment text -> do
      text' <- runReplacements text
      return $ comment text'
    TeXSeq l1 l2 -> do
      l1' <- recur l1
      l2' <- recur l2
      return $ l1' <> l2'
    TeXEmpty -> return $ fromLaTeX TeXEmpty
  where
    recur child = applyTemplateToEl child e
    recurArgs
      :: Hint.MonadInterpreter m
      => [TeXArg] -> m [TeXArg]
    recurArgs = mapM recurArg
    recurArg
      :: Hint.MonadInterpreter m
      => TeXArg -> m TeXArg
    recurArg arg =
      case arg of
        FixArg a   -> FixArg <$> inner a
        OptArg a   -> OptArg <$> inner a
        MOptArg as -> MOptArg <$> mapM inner as
        SymArg a   -> SymArg <$> inner a
        MSymArg as -> MSymArg <$> mapM inner as
        ParArg a   -> ParArg <$> inner a
        MParArg as -> MParArg <$> mapM inner as
    inner i = do
      o <- recur i
      return $ runLaTeX o
    runReplacementsS s = Text.unpack <$> runReplacements (Text.pack s)
    runReplacements i =
      case parseMaybe interpParser i of
        Nothing -> return ""
        Just interp -> do
          rs <- mapM (evalNode e) interp
          return (Text.concat rs)

mergeEithers :: [Either a b] -> Either [a] [b]
mergeEithers [] = Right []
mergeEithers (Left e:es) = Left (e : lefts es)
mergeEithers (Right e:es) =
  case mergeEithers es of
    lfs@(Left _) -> lfs
    (Right rs)   -> Right (e : rs)

isTruthy :: Value -> Bool
isTruthy (Bool b)   = b
isTruthy (Number n) = n /= 0
isTruthy (Object o) = o /= mempty
isTruthy (String s) = s /= mempty
isTruthy Null       = False
isTruthy (Array _)  = True

parseCTemplateFromJson :: Value -> Either [Text] [ConcreteTemplateNode]
parseCTemplateFromJson (Object o) =
  mergeEithers $ HashMap.foldrWithKey parsePair [] o
  where
    parsePair k v m =
      let mctn = fromJSON v :: Result ConcreteTemplateNode
      in case mctn of
           Error e     -> Left (Text.pack e) : m
           Success ctn -> Right ctn {templateSelector = k} : m
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
          forM_ errs $ \err -> Text.hPutStrLn stderr err
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
          forM_ errs $ \err -> Text.hPutStrLn stderr err
          exitWith (ExitFailure 1)
        Right ns -> return $ Template $ zip cs ns

defaultTemplate :: Template
defaultTemplate = unsafePerformIO $ parseTemplateFile "./default.yaml"
{-# NOINLINE defaultTemplate #-}

findTemplate :: Template -> TemplateContext -> Maybe (ConcreteTemplateNode, TemplateNode)
findTemplate ts el = run ts el
  where
    run (Template []) _ = Nothing
    run (Template (p@(_, t@TemplateNode{..}):ps)) el =
        if runPredicate templatePredicate targetName
        then Just p
        else run (Template ps) el
    targetName = showQName (elName (tcElement el))

runTemplate
  :: (MonadIO m, Hint.MonadInterpreter m)
  => Template
  -> TemplateContext
  -> m (Maybe ((ConcreteTemplateNode, TemplateNode), (LaTeXT Identity (), LaTeXT Identity ())))
runTemplate ts el =
  case findTemplate ts el of
    Nothing -> return Nothing
    Just p@(_, t) -> do
      liftIO $ hPutStrLn stderr "Found template, applying..."
      r <- templateApply t el
      return $ Just (p, r)
