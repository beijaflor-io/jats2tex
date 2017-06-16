{-# LANGUAGE QuasiQuotes #-}
module Text.JaTexSpec where

import           Data.String.Here
import qualified Data.Text        as Text

import           Test.Hspec

import           Text.JaTex

parseJATS' = parseJATS . Text.pack

spec :: Spec
spec = do
    describe "readJats" $ do
        it "works" pending

    describe "jatsXmlToLaTeXText" $ do
        it "doesn't encode xml version encoding and namespaces" $ do
            let inp = parseJATS' [here|
<?xml version="1.0" encoding="ISO-8859-1"?><article xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <front>
  </front>
</article>
                                      |]
            jatsXmlToLaTeXText "" inp `shouldBe`
                Text.pack ([here|\documentclass{article}
  \begin{front}
  \end{front}
                           |] ++ "\n")

        it "works" $ do
            let inp = parseJATS' [here|
<article xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <front>
    <article-meta>
      <title-group>
        <article-title xml:lang="pt"><![CDATA[A influência do genótipo da ECA sobre a aptidão cardiovascular de jovens do sexo masculino moderadamente ativos]]></article-title>
        <article-title xml:lang="en"><![CDATA[The influence of ACE genotype on cardiovascular fitness of moderately active young men]]></article-title>
      </title-group>
    </article-meta>
  </front>
</article>
                      |]
            jatsXmlToLaTeXText "" inp `shouldBe`
                Text.pack ([here|\documentclass{article}
  \begin{front}
    \begin{article-meta}
      \begin{title-group}
        \article-title{pt}{A influência do genótipo da ECA sobre a aptidão cardiovascular de jovens do sexo masculino moderadamente ativos}
        \article-title{en}{The influence of ACE genotype on cardiovascular fitness of moderately active young men}
      \end{title-group}
    \end{article-meta}
  \end{front}
                           |] ++ "\n")

        it "translates basic formatting" $ do
            let inp = parseJATS' [here|<bold>Bold</bold> text here<break />
New-line and <code>Some code</code>|]
            jatsXmlToLaTeXText "" inp `shouldBe`
                Text.pack [here|\textbf{Bold} text here\newline{}
New-line and \texttt{Some code}

|]


    describe "jatsXmlToLaTeX" $ do
        it "works" pending

    describe "convertNode" $ do
        it "works" pending

    describe "convertInline" $ do
        it "works" pending

    describe "jatsXmlToBlocks" $ do
        it "works" pending
