{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Main
  where

import           Control.Monad.IO.Class
import           Data.Aeson                    (Value (..), object, (.=))
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe
import           Data.Monoid                   ((<>))
import           Data.String.Here
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Network.HTTP.Types.Status
import           Network.Wai.Middleware.Static
import           System.Environment
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Hamlet
import           Web.Spock
import           Web.Spock.Config

import           Text.JaTex

curlExample :: Text -> Text
curlExample host = "curl " <> host <> " -F \"body=@./`echo ./something.xml`\""

placeholder :: String
placeholder = [here|
<article xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <front>
    <article-meta>
      <title-group>
        <article-title xml:lang="pt"><![CDATA[A influência do genótipo da ECA sobre a aptidão cardiovascular de jovens do sexo masculino moderadamente ativos]]></article-title>
        <article-title xml:lang="en"><![CDATA[The influence of ACE genotype on cardiovascular fitness of moderately active young men]]></article-title>
      </title-group>
    </article-meta>
  </front>
  <body>
    <bold>Bold</bold> text here<break />
    New-line and <code>Some code</code>
  </body>
</article>
|]

getHome host = preferredFormat >>= \case
    PrefHTML -> lazyBytes $ renderHtml $ [shamlet|
$doctype 5
<html>
  <head>
    <meta charset="UTF-8">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">

    <title>JaTeX Demo</title>
    <style>
      #app textarea, pre {
        white-space: nowrap;
        overflow: scroll;
      }
      #app pre code {
        white-space: pre;
      }

      #app .jumbotron h4 {
        font-size: 20px;
      }

      #app .jumbotron h5 {
        font-size: 18px;
      }

      #app .jumbotron p {
        font-size: 16px;
      }

      #app h4 {
        font-weight: 800;
      }
  <body>
    <div id="app">
      <div class="neal-hero jumbotron" style="padding: 0; margin: 0;">
        <div class="container">
          <h1> JaTeX Demo
          <h4>
            Como funciona
          <p>
            Executa um parser de XML e percorre as "tags", recursivamente
            convertendo blocos para LaTeX.
          <h4>
            Entidades Suportadas na Prova de Conceito
          <h5>
            1. Entidades Estruturais
          <p>
            Vão abrir um environment e permitir a customização da saída das
            <strong>Entidades de Saída</strong>.
          <ul>
            <li>
              <code>article</code>
            <li>
              <code>body</code>
            <li>
              <code>front</code>
            <li>
              <code>articlemeta</code>
            <li>
              <code>titlegroup</code>
          <h5>
            2. Entidades de Saída
          <p>
            Vão executar um comando com estrutura pré-definida e resultado customizado.
          <ul>
            <li>
              <code>article-title</code>
          <h5>
            3. Entidades de Formatação / Inline
          <p>
            Se transformam em blocos de texto inline.
          <ul>
            <li>
              <code>bold</code>
            <li>
              <code>p</code>
            <li>
              <code>break</code>
            <li>
              <code>code</code>

      <div class="container">
        <h4> Converta com <code>curl</code>
        <pre><code>#{curlExample host}</code></pre>
        <h4> Digite JATS-XML abaixo e aperte "Converter"
        <.row>
          <.col-md-6>
            <form action="/form" method="POST">
              <.form-group>
                <textarea .form-control name="body" id="body" rows="30" style="resize: none; min-height: 50vh;">
                  #{placeholder}
              <.form-group>
                <button class="btn btn-primary" type="submit">
                  Converter
          <.col-md-6>
            <pre><code class="result"># Result</code></pre>
      <div style="text-align: center; padding: 20px; color: #ccc; margin-bottom: 10px">
        Copyright (c) 2017 - Todos os direitos reservados - Pedro Tacla Yamada
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.1.0/jquery.min.js" />
    <script>

      \$('form').submit(function(e) {
        e.preventDefault();
        \$.post('/form', {body: $('textarea').val()}, function(res) {
          \$('.result').text(res);
        }).fail(function(err) {
          \$('.result').text(JSON.stringify(err, null, 2));
        });
      });
        |]
    _ -> text $ Text.unlines [ "Métodos Disponíveis:"
                             , "POST /"
                             , "  Converte arquivos JATS-XML enviados pelo parâmetro form `body`"
                             , "  para LaTeX"
                             ]

main :: IO ()
main = do
    port <- (fromMaybe 3000 . fmap read) <$> lookupEnv "PORT"
    host <- (Text.pack . fromMaybe ("localhost:" <> show port)) <$> lookupEnv "HOST"
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock port $ spock spockCfg $ do
        middleware (staticPolicy (addBase "static"))

        get "/" (getHome host)

        post "/form" $ do
            pkg <- param' "body"
            let !etex = jatsXmlToLaTeXText "none" $ parseJATS pkg
            text etex

        post "/" $ do
            fs <- files
            case HashMap.lookup "body" fs of
                Nothing -> resError $ Text.unlines [ "Missing `body` parameter"
                                                   ]
                Just UploadedFile{..} -> do
                    !etex <- liftIO (jatsXmlToLaTeXText "none" <$> readJats uf_tempLocation)
                    text etex
  where
    resError e = do
        setStatus status422
        text e
