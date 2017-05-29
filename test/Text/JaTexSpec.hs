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
            jatsXmlToLaTeXText inp `shouldBe`
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
            jatsXmlToLaTeXText inp `shouldBe`
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
            jatsXmlToLaTeXText inp `shouldBe`
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

            -- <contrib-group>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[Almeida]]></surname>
            --             <given-names><![CDATA[Jeeser Alves]]></given-names>
            --         </name>
            --     </contrib>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[Boullosa]]></surname>
            --             <given-names><![CDATA[Daniel Alexandre]]></given-names>
            --         </name>
            --     </contrib>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[Pardono]]></surname>
            --             <given-names><![CDATA[Emerson]]></given-names>
            --         </name>
            --     </contrib>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[Lima]]></surname>
            --             <given-names><![CDATA[Ricardo Moreno]]></given-names>
            --         </name>
            --     </contrib>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[Morais]]></surname>
            --             <given-names><![CDATA[PÃ¢mella Karoline]]></given-names>
            --         </name>
            --     </contrib>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[Denadai]]></surname>
            --             <given-names><![CDATA[Benedito SÃ©rgio]]></given-names>
            --         </name>
            --     </contrib>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[Souza]]></surname>
            --             <given-names><![CDATA[VinÃ­cius Carolino]]></given-names>
            --         </name>
            --     </contrib>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[NÃ³brega]]></surname>
            --             <given-names><![CDATA[OtÃ¡vio Toledo]]></given-names>
            --         </name>
            --     </contrib>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[Campbell]]></surname>
            --             <given-names><![CDATA[Carmem Silvia Grubert]]></given-names>
            --         </name>
            --     </contrib>
            --     <contrib contrib-type="author">
            --         <name>
            --             <surname><![CDATA[SimÃµes]]></surname>
            --             <given-names><![CDATA[Herbert Gustavo]]></given-names>
            --         </name>
            --     </contrib>
            -- </contrib-group>
            -- <aff id="A">
            --     <institution><![CDATA[,  ]]></institution>
            --     <addr-line><![CDATA[ ]]></addr-line>
            -- </aff>
            -- <pub-date pub-type="pub">
            --     <day>00</day>
            --     <month>04</month>
            --     <year>2012</year>
            -- </pub-date>
            -- <pub-date pub-type="epub">
            --     <day>00</day>
            --     <month>04</month>
            --     <year>2012</year>
            -- </pub-date>
            -- <volume>98</volume>
            -- <numero>4</numero>
            -- <fpage>315</fpage>
            -- <lpage>320</lpage>
            -- <copyright-statement />
            -- <copyright-year />
            -- <self-uri xlink:href="http://www.scielo.br/scielo.php?script=sci_arttext&amp;pid=S0066-782X2012000400005&amp;lng=en&amp;nrm=iso"></self-uri>
            -- <self-uri xlink:href="http://www.scielo.br/scielo.php?script=sci_abstract&amp;pid=S0066-782X2012000400005&amp;lng=en&amp;nrm=iso"></self-uri>
            -- <self-uri xlink:href="http://www.scielo.br/scielo.php?script=sci_pdf&amp;pid=S0066-782X2012000400005&amp;lng=en&amp;nrm=iso"></self-uri>
            -- <abstract abstract-type="short" xml:lang="pt">
            --     <p><![CDATA[FUNDAMENTO: O gene da enzima conversora de angiotensina (gene ECA) tem sido amplamente estudado em relaÃ§Ã£o a fenÃ³tipos de aptidÃ£o cardiorrespiratÃ³ria, contudo a associaÃ§Ã£o do genÃ³tipo da ECA com corridas de meia-distÃ¢ncia tem sido pouco investigada. OBJETIVO: O presente estudo investigou a possÃ­vel influÃªncia da enzima conversora de angiotensina (ECA) (I/D) sobre a aptidÃ£o cardiovascular e o desempenho em corridas de meia-distÃ¢ncia por parte de brasileiros jovens do sexo masculino. A validade da previsÃ£o de VO2max em relaÃ§Ã£o ao genÃ³tipo da ECA tambÃ©m foi analisada. MÃTODOS: Um grupo homogÃªneo de homens jovens moderadamente ativos foi avaliado em um teste de corrida (V1600 m; m.min-1) e em um teste adicional em esteira ergomÃ©trica para a determinaÃ§Ã£o de VO2max. Posteriormente, o [(0,177*V1600m) + 8.101] VO2max real e previsto foi comparado com os genÃ³tipos da ECA. RESULTADOS: O VO2max e V1600m registrados para os genÃ³tipos DD, ID e II foram 45,6 (1,8); 51,9 (0,8) e 54,4 (1,0) mL.kg-1.min-1 e 211,2 (8,3); 249,1 (4,3) e 258,6 (5,4 ) m.min-1, respectivamente e foram significativamente mais baixos para os genÃ³tipos DD (p < 0,05). O VO2max real e previsto nÃ£o diferiram entre si, apesar do genÃ³tipo da ECA, mas o nÃ­vel de concordÃ¢ncia entre os mÃ©todos de VO2max real e estimado foi menor para o genÃ³tipo DD. CONCLUSÃO: Concluiu-se que existe uma possÃ­vel associaÃ§Ã£o entre o genÃ³tipo da ECA, a aptidÃ£o cardiovascular e o desempenho em corridas de mÃ©dia distÃ¢ncia de jovens do sexo masculino moderadamente ativos e que a precisÃ£o da previsÃ£o do VO2max tambÃ©m pode ser dependente do genÃ³tipo da ECA dos participantes.]]></p>
            -- </abstract>
            -- <abstract abstract-type="short" xml:lang="en">
            --     <p><![CDATA[BACKGROUND: The angiotensin I-converting enzyme gene (ACE gene) has been broadly studied as for cardiorespiratory fitness phenotypes, but the association of the ACE genotype to middle-distance running has been poorly investigated. OBJECTIVE: This study investigated the possible influence of Angiotensin-Converting Enzyme (ACE) genotype (I/D) on cardiovascular fitness and middle-distance running performance of Brazilian young males. The validity of VO2max to predict the ACE genotype was also analyzed. METHODS: A homogeneous group of moderately active young males were evaluated in a 1,600 m running track test (V1600m; m.min-1) and in an incremental treadmill test for VO2max determination. Subsequently, the actual and the predicted [(0.177*V1600m) + 8.101] VO2max were compared to ACE genotypes. RESULTS: The VO2max and V1600m recorded for DD, ID and II genotypes were 45.6 (1.8); 51.9 (0.8) and 54.4 (1.0) mL.kg-1.min-1 and 211.2 (8.3); 249.1 (4.3) and 258.6 (5.4) m.min-1 respectively, and were significantly lower for DD carriers (p< 0.05). The actual and predicted VO2max did not differ from each other despite ACE genotype, but the agreement between actual and estimated VO2max methods was lower for the DD genotype. CONCLUSION: It was concluded that there is a possible association between ACE genotype, cardiovascular fitness and middle-distance running performance of moderately active young males and that the accuracy of VO2max prediction may also depend on the ACE genotype of the participants.]]></p>
            -- </abstract>
            -- <kwd-group>
            --     <kwd lng="pt"><![CDATA[Enzima conversora de angiotensina]]></kwd>
            --     <kwd lng="pt"><![CDATA[polimorfismo I]]></kwd>
            --     <kwd lng="pt"><![CDATA[VO2max]]></kwd>
            --     <kwd lng="pt"><![CDATA[corrida de mÃ©dia distÃ¢ncia]]></kwd>
            --     <kwd lng="en"><![CDATA[Angiotensin-converting enzyme]]></kwd>
            --     <kwd lng="en"><![CDATA[I]]></kwd>
            --     <kwd lng="en"><![CDATA[VO2max]]></kwd>
            --     <kwd lng="en"><![CDATA[middle-distance running]]></kwd>
            -- </kwd-group>
