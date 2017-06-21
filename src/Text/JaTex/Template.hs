{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.JaTex.Template
  where

import           Control.Monad
import           Control.Monad.Identity
import           Data.Aeson             (Result (..), Value (..), fromJSON)
import qualified Data.ByteString
import           Data.Either
import qualified Data.HashMap.Strict    as HashMap
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Data.Yaml
import qualified Data.Yaml              as Yaml
import           System.Exit
import           System.IO
import           System.IO.Unsafe
import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax
import           Text.XML.Light

data ConcreteTemplateNode = ConcreteTemplateNode
  { templateSelector :: Text
  , templateHead     :: Text
  , templateContent  :: Text
  } deriving (Show)

instance Yaml.FromJSON ConcreteTemplateNode where
  parseJSON (String s) = return $ ConcreteTemplateNode "" "" s
  parseJSON (Object o) = verboseForm
    where
      verboseForm =
        ConcreteTemplateNode "" <$> (fromMaybe "" <$> o .:? "head") <*>
        (fromMaybe "" <$> o .:? "content")
  parseJSON _ = fail "Template inválido"

data TemplateNode = TemplateNode
  { templatePredicate :: NodeSelector
  , templateLaTeXHead :: LaTeX
  , templateLaTeX     :: LaTeX
  }

data TemplateContext = TemplateContext
  { tcHeads   :: LaTeXT Identity ()
  , tcBodies  :: LaTeXT Identity ()
  , tcElement :: Element
  }

type NodeSelector = Text

templateApply :: TemplateNode -> TemplateContext -> (LaTeXT Identity (), LaTeXT Identity ())
templateApply TemplateNode{templateLaTeX, templateLaTeXHead} tc =
    ( applyTemplateToEl templateLaTeXHead tc
    , applyTemplateToEl templateLaTeX tc
    )

runPredicate :: NodeSelector -> TemplateContext -> Bool
runPredicate s TemplateContext{tcElement = e} = showQName (elName e) == Text.unpack s

parseTemplateNode :: ConcreteTemplateNode -> Either ParseError TemplateNode
parseTemplateNode ConcreteTemplateNode {..} =
  case latexContent of
    Right l ->
      case templateHead of
        "" ->
          Right
            TemplateNode
            { templatePredicate = templateSelector
            , templateLaTeXHead = mempty
            , templateLaTeX = l
            }
        _ ->
          case parseLaTeX templateHead of
            Right h ->
              Right
                TemplateNode
                { templatePredicate = templateSelector
                , templateLaTeXHead = h
                , templateLaTeX = l
                }
            Left e -> Left e
    Left e -> Left e
  where
    latexContent = parseLaTeX templateContent

applyTemplateToEl :: LaTeX -> TemplateContext -> LaTeXT Identity ()
applyTemplateToEl l e =
  case l of
    TeXLineBreak m b -> fromLaTeX $ TeXLineBreak m b
    TeXBraces i -> braces (recur i)
    TeXMath mt i -> liftL (TeXMath mt) (recur i)
    TeXEnv ev as i ->
      liftL (TeXEnv (runReplacementsS ev) (recurArgs as)) (recur i)
    TeXCommS c -> textell $ TeXCommS (runReplacementsS c)
    TeXComm c as ->
      fromLaTeX $
      TeXComm (Text.unpack (runReplacements (Text.pack c))) (recurArgs as)
    TeXRaw lr -> fromLaTeX $ TeXRaw (runReplacements lr)
    TeXComment text -> comment (runReplacements text)
    TeXSeq l1 l2 -> recur l1 <> recur l2
    TeXEmpty -> fromLaTeX TeXEmpty
  where
    recur child = applyTemplateToEl child e
    recurArgs = map recurArg
    recurArg arg =
      case arg of
        FixArg a   -> FixArg (inner a)
        OptArg a   -> OptArg (inner a)
        MOptArg as -> MOptArg (map inner as)
        SymArg a   -> SymArg (inner a)
        MSymArg as -> MSymArg (map inner as)
        ParArg a   -> ParArg (inner a)
        MParArg as -> MParArg (map inner as)
    runLaTeX = snd . runIdentity . runLaTeXT
    inner = runLaTeX . recur
    runReplacementsS = Text.unpack . runReplacements . Text.pack
    runReplacements :: Text -> Text
    runReplacements i = foldr (\f m -> f m) i [ replaceChildren
                                              , replaceHeads
                                              , replaceBodies
                                              , replaceRequirements
                                              ]
      where
        replaceHeads = Text.replace "@@heads" (render (runLaTeX (tcHeads e)))
        replaceBodies = Text.replace "@@bodies" (render (runLaTeX (tcBodies e)))
        replaceChildren = Text.replace "@@children" (render (runLaTeX (tcChildren e)))
        replaceRequirements = Text.replace "@@requirements" (render (runLaTeX requirements))

requirements :: LaTeXT Identity ()
requirements = do
  usepackage ["document"] "ragged2e"
  fromString "\n"
  usepackage ["utf8x"] "inputenc"


tcChildren :: TemplateContext -> LaTeXT Identity ()
tcChildren e = tcHeads e <> tcBodies e

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

runTemplate :: Template -> TemplateContext -> Maybe ((ConcreteTemplateNode, TemplateNode), (LaTeXT Identity (), LaTeXT Identity ()))
runTemplate (Template []) _ = Nothing
runTemplate (Template (p@(_, t@TemplateNode{..}):ps)) el =
    if runPredicate templatePredicate el
       then Just (p, templateApply t el)
       else runTemplate (Template ps) el
