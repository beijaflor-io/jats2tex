#!/usr/bin/env stack
-- stack runghc --package case-conversion --package mtl --package MissingH
import Control.Monad
import Data.Char
import Text.CaseConversion
import Control.Monad.Writer
import Data.String.Utils

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize "" = ""

camelCase e = (toCase Camel (fromCase Spinal e))

main = do
    elements <- lines <$> readFile "./list"
    putStrLn "{-# LANGUAGE DeriveFunctor   #-}"
    putStrLn "{-# LANGUAGE RecordWildCards #-}"
    putStrLn "module JATSXML.Types where"
    putStrLn ""
    putStrLn "import Data.Maybe"
    putStrLn "import Text.XML.Light"
    putStrLn "import JATSXML.Class"

    let (dataTypeNames, out) = runWriter $ forM elements $ \element -> do
        let element' = replace ":" "-" element
            fieldPrefix = camelCase ("jatsxml-" ++ element')
            dataTypeName = capitalize fieldPrefix
        tell "\n"
        tell
            $ "data " ++ dataTypeName ++ " = " ++ dataTypeName ++ "\n" ++
              "    { " ++ fieldPrefix ++ "Element :: Element\n" ++
              "    , " ++ fieldPrefix ++ "Children :: [JATSElement]\n" ++
              "    }\n"
        tell "\n"
        tell
            $ "instance FromXMLNode " ++ dataTypeName ++ " where\n" ++
              "    fromXMLNode e@(Element{..}) | elName == \"" ++ element ++ "\" = Just $\n" ++
              "         " ++ dataTypeName ++ " e (catMaybes fromXMLNode elContent)\n" ++
              "    fromXMLNode _ = Nothing\n"
        return dataTypeName

    putStr out

    let (_, (f:outDataWrapper)) = runWriter $ forM_ dataTypeNames $ \dataTypeName -> do
        let n = "Node" ++ dataTypeName
        tell $ [n ++ " " ++ dataTypeName]
        return dataTypeName

    putStrLn "data JATSElement ="
    putStrLn $ "    " ++ f
    forM_ outDataWrapper $ \d ->
        putStrLn $ "  | " ++ d
    putStrLn ""
    putStrLn "instance FromXMLNode JATSElement where"
    putStrLn "    fromXMLNode x = case fromXMLNode n of\n"
