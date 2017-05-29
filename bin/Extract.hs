#!/usr/bin/env stack
-- stack runghc --resolver lts-8.3 --package case-conversion --package mtl --package MissingH --package xml --package tagsoup --package scalpel
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Control.Monad.Writer
import           Data.Char
import           Data.String.Utils
import           Text.CaseConversion
import           Text.HTML.Scalpel
-- import           Text.XML.Light hiding (Element)

import Debug.Trace

data ElName = ElName String | ElRef String
type ElAttributes = [String]

data Element =
  Element { elName :: ElName
          , elAttributes :: ElAttributes
          , elChildren :: [Element]
          }

element = do
    p <- position
    n <- attr "name" "xsd:element"
    r <- attr "ref" "xsd:element"
    attrs <- chroots "xsd:attribute" attribute
    attrs <- chroots "xsd:annotation" annotation

    return (p, Element {
      elName = if n /= "" then ElName n else ElRef r
      , elAttributes
    })

elementsScraper = do
  as <- chroots "xsd:element" element
  return $ (map (\e -> ("element", e)) as)

main = do
  Just elements <-
    flip scrapeStringLike elementsScraper <$>
    readFile "./schemas/JATS-journalpublishing1-elements.xsd"
  forM_ elements $ \(t, el) -> putStrLn $ show t ++ " " ++ show el
