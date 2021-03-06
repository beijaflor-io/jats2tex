{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.JaTexSpec where

import Data.String.Here
import qualified Data.Text as Text

import Test.Hspec

import Text.JaTex

spec :: Spec
spec = do
  describe "readJats" $ do
    it "works" $ do
      doc <- parseJATS [here|<strong>Hello</strong>|]
      templ <- parseTemplate "<noname>" [here|strong: "\\textbf{@@children}"|]
      output <-
        jatsXmlToLaTeXText
          def {joInputDocument = doc, joTemplate = (templ, "<noname>")}
      output `shouldBe` "% Generated by jats2tex@0.11.1.0\n\\textbf{Hello}\n"
    it "handles nested XPaths" $ do
      doc <-
        parseJATS
          [here|
            <sec>
              <p>Something</p>
              <other><p>Hidden</p></other>
            </sec>
          |]
      templ <-
        parseTemplate
          "<noname>"
          [here|
            sec: |
              @@lua(return find("/p"))@@
          |]
      output <-
        jatsXmlToLaTeXText
          def {joInputDocument = doc, joTemplate = (templ, "<noname>")}
      output `shouldBe` "% Generated by jats2tex@0.11.1.0\nSomething\n"
