{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Text.JaTexSpec where

import           Data.String.Here
import qualified Data.Text        as Text

import           Test.Hspec

import           Text.JaTex

spec :: Spec
spec = do
    describe "readJats" $ do
        it "works" $ do
            doc <- parseJATS [here|
                                  <strong>Hello</strong>
                                  |]
            templ <- parseTemplate "<noname>" [here|
                                                   strong: "\\textbf{@@children}"
                                                   |]
            output <- jatsXmlToLaTeXText def { joInputDocument = doc
                                             , joTemplate = (templ, "<noname>")
                                             }
            output `shouldBe` "\\textbf{Hello}"
