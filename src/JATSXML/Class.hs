module JATSXML.Class where

import           Text.XML.Light

class FromXMLNode e where
    fromXMLNode :: Content -> Maybe e
