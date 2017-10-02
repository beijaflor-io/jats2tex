{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.JaTex.UpgradeTemplate
  where

import           Data.Aeson
import qualified Data.Yaml  as Yaml

run :: FilePath -> IO ()
run targetTemplateFile = do
  minput <- Yaml.decodeFile targetTemplateFile :: IO (Maybe Value)
  case minput of
    Nothing -> error "Failed to parse input template"
    Just !input ->
      Yaml.encodeFile targetTemplateFile $
        object ["version" .= (Number 1), "rules" .= input]
