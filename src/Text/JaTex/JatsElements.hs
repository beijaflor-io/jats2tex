{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Text.JaTex.JatsElements
  where

import           Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.XML.HaXml.Schema.Schema         (Extension (..),
                                                       Restricts (..),
                                                       SchemaType (..),
                                                       SimpleType (..))
import           Text.XML.HaXml.Schema.Schema         as Schema
-- import           V''module'ali'xsd                       as Ali
-- import           V''standard'modules'mathml2'mathml2'xsd as Mml
-- import           V''standard'modules'xlink'xsd           as Xlink
-- import           V''standard'modules'xml'xsd

-- Some hs-boot imports are required, for fwd-declaring types.


data Abbrev = Abbrev
        { abbrev_alt          :: Maybe Xsd.XsdString
        , abbrev_content'type :: Maybe Xsd.XsdString
        , abbrev_id           :: Maybe Xsd.ID
        , abbrev_specific'use :: Maybe Xsd.XsdString
        , abbrev_actuate      :: Maybe Xsd.XsdString
        , abbrev_href         :: Maybe Xsd.AnyURI
        , abbrev_role         :: Maybe Xsd.XsdString
        , abbrev_show         :: Maybe Xsd.XsdString
        , abbrev_title        :: Maybe Xsd.XsdString
        , abbrev_type         :: Maybe Xsd.XsdString
        , abbrev_base         :: Maybe Xsd.AnyURI
        , abbrev_lang         :: Maybe Xsd.XsdString
        , abbrev_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Abbrev where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "alt" e pos
        a1 <- optional $ getAttribute "content-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "actuate" e pos
        a5 <- optional $ getAttribute "href" e pos
        a6 <- optional $ getAttribute "role" e pos
        a7 <- optional $ getAttribute "show" e pos
        a8 <- optional $ getAttribute "title" e pos
        a9 <- optional $ getAttribute "type" e pos
        a10 <- optional $ getAttribute "base" e pos
        a11 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Abbrev a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
            `apply` many (oneOf' [ -- -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Abbrev{} =
        toXMLElement s [ maybe [] (toXMLAttribute "alt") $ abbrev_alt x
                       , maybe [] (toXMLAttribute "content-type") $ abbrev_content'type x
                       , maybe [] (toXMLAttribute "id") $ abbrev_id x
                       , maybe [] (toXMLAttribute "specific-use") $ abbrev_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ abbrev_actuate x
                       , maybe [] (toXMLAttribute "href") $ abbrev_href x
                       , maybe [] (toXMLAttribute "role") $ abbrev_role x
                       , maybe [] (toXMLAttribute "show") $ abbrev_show x
                       , maybe [] (toXMLAttribute "title") $ abbrev_title x
                       , maybe [] (toXMLAttribute "type") $ abbrev_type x
                       , maybe [] (toXMLAttribute "base") $ abbrev_base x
                       , maybe [] (toXMLAttribute "lang") $ abbrev_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ abbrev_choice0 x
            ]

elementAbbrev :: XMLParser Abbrev
elementAbbrev = parseSchemaType "abbrev"
elementToXMLAbbrev :: Abbrev -> [Content ()]
elementToXMLAbbrev = schemaTypeToXML "abbrev"

data Abbrev'journal'title = Abbrev'journal'title
        { abbrev'journal'title_abbrev'type  :: Maybe Xsd.XsdString
        , abbrev'journal'title_id           :: Maybe Xsd.ID
        , abbrev'journal'title_specific'use :: Maybe Xsd.XsdString
        , abbrev'journal'title_base         :: Maybe Xsd.AnyURI
        , abbrev'journal'title_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Abbrev'journal'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "abbrev-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Abbrev'journal'title a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Abbrev'journal'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "abbrev-type") $ abbrev'journal'title_abbrev'type x
                       , maybe [] (toXMLAttribute "id") $ abbrev'journal'title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ abbrev'journal'title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ abbrev'journal'title_base x
                       , maybe [] (toXMLAttribute "lang") $ abbrev'journal'title_lang x
                       ]
            []

elementAbbrev'journal'title :: XMLParser Abbrev'journal'title
elementAbbrev'journal'title = parseSchemaType "abbrev-journal-title"
elementToXMLAbbrev'journal'title :: Abbrev'journal'title -> [Content ()]
elementToXMLAbbrev'journal'title = schemaTypeToXML "abbrev-journal-title"

data Abstract = Abstract
        { abstract_abstract'type :: Maybe Xsd.XsdString
        , abstract_id            :: Maybe Xsd.ID
        , abstract_specific'use  :: Maybe Xsd.XsdString
        , abstract_base          :: Maybe Xsd.AnyURI
        , abstract_lang          :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Abstract where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "abstract-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Abstract a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Abstract{} =
        toXMLElement s [ maybe [] (toXMLAttribute "abstract-type") $ abstract_abstract'type x
                       , maybe [] (toXMLAttribute "id") $ abstract_id x
                       , maybe [] (toXMLAttribute "specific-use") $ abstract_specific'use x
                       , maybe [] (toXMLAttribute "base") $ abstract_base x
                       , maybe [] (toXMLAttribute "lang") $ abstract_lang x
                       ]
            []

elementAbstract :: XMLParser Abstract
elementAbstract = parseSchemaType "abstract"
elementToXMLAbstract :: Abstract -> [Content ()]
elementToXMLAbstract = schemaTypeToXML "abstract"

data Access'date = Access'date
        { access'date_calendar      :: Maybe Xsd.XsdString
        , access'date_content'type  :: Maybe Xsd.XsdString
        , access'date_id            :: Maybe Xsd.ID
        , access'date_iso'8601'date :: Maybe Xsd.XsdString
        , access'date_specific'use  :: Maybe Xsd.XsdString
        , access'date_base          :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Access'date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "calendar" e pos
        a1 <- optional $ getAttribute "content-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "iso-8601-date" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Access'date a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Access'date{} =
        toXMLElement s [ maybe [] (toXMLAttribute "calendar") $ access'date_calendar x
                       , maybe [] (toXMLAttribute "content-type") $ access'date_content'type x
                       , maybe [] (toXMLAttribute "id") $ access'date_id x
                       , maybe [] (toXMLAttribute "iso-8601-date") $ access'date_iso'8601'date x
                       , maybe [] (toXMLAttribute "specific-use") $ access'date_specific'use x
                       , maybe [] (toXMLAttribute "base") $ access'date_base x
                       ]
            []

elementAccess'date :: XMLParser Access'date
elementAccess'date = parseSchemaType "access-date"
elementToXMLAccess'date :: Access'date -> [Content ()]
elementToXMLAccess'date = schemaTypeToXML "access-date"

data Ack = Ack
        { ack_content'type :: Maybe Xsd.XsdString
        , ack_id           :: Maybe Xsd.ID
        , ack_specific'use :: Maybe Xsd.XsdString
        , ack_base         :: Maybe Xsd.AnyURI
        , ack_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Ack where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Ack a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Ack{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ ack_content'type x
                       , maybe [] (toXMLAttribute "id") $ ack_id x
                       , maybe [] (toXMLAttribute "specific-use") $ ack_specific'use x
                       , maybe [] (toXMLAttribute "base") $ ack_base x
                       , maybe [] (toXMLAttribute "lang") $ ack_lang x
                       ]
            []

elementAck :: XMLParser Ack
elementAck = parseSchemaType "ack"
elementToXMLAck :: Ack -> [Content ()]
elementToXMLAck = schemaTypeToXML "ack"

data Addr'line = Addr'line
        { addr'line_content'type :: Maybe Xsd.XsdString
        , addr'line_id           :: Maybe Xsd.ID
        , addr'line_specific'use :: Maybe Xsd.XsdString
        , addr'line_base         :: Maybe Xsd.AnyURI
        , addr'line_lang         :: Maybe Xsd.XsdString
        , addr'line_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Addr'line where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Addr'line a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Addr'line{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ addr'line_content'type x
                       , maybe [] (toXMLAttribute "id") $ addr'line_id x
                       , maybe [] (toXMLAttribute "specific-use") $ addr'line_specific'use x
                       , maybe [] (toXMLAttribute "base") $ addr'line_base x
                       , maybe [] (toXMLAttribute "lang") $ addr'line_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ addr'line_choice0 x
            ]

elementAddr'line :: XMLParser Addr'line
elementAddr'line = parseSchemaType "addr-line"
elementToXMLAddr'line :: Addr'line -> [Content ()]
elementToXMLAddr'line = schemaTypeToXML "addr-line"

data Address = Address
        { address_content'type :: Maybe Xsd.XsdString
        , address_id           :: Maybe Xsd.ID
        , address_specific'use :: Maybe Xsd.XsdString
        , address_base         :: Maybe Xsd.AnyURI
        , address_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Address where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Address a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Address{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ address_content'type x
                       , maybe [] (toXMLAttribute "id") $ address_id x
                       , maybe [] (toXMLAttribute "specific-use") $ address_specific'use x
                       , maybe [] (toXMLAttribute "base") $ address_base x
                       , maybe [] (toXMLAttribute "lang") $ address_lang x
                       ]
            []

elementAddress :: XMLParser Address
elementAddress = parseSchemaType "address"
elementToXMLAddress :: Address -> [Content ()]
elementToXMLAddress = schemaTypeToXML "address"

data Aff = Aff
        { aff_content'type :: Maybe Xsd.XsdString
        , aff_id           :: Maybe Xsd.ID
        , aff_rid          :: Maybe Xsd.IDREFS
        , aff_specific'use :: Maybe Xsd.XsdString
        , aff_base         :: Maybe Xsd.AnyURI
        , aff_lang         :: Maybe Xsd.XsdString
        , aff_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Aff where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "rid" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Aff a0 a1 a2 a3 a4 a5)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Aff{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ aff_content'type x
                       , maybe [] (toXMLAttribute "id") $ aff_id x
                       , maybe [] (toXMLAttribute "rid") $ aff_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ aff_specific'use x
                       , maybe [] (toXMLAttribute "base") $ aff_base x
                       , maybe [] (toXMLAttribute "lang") $ aff_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ aff_choice0 x
            ]

elementAff :: XMLParser Aff
elementAff = parseSchemaType "aff"
elementToXMLAff :: Aff -> [Content ()]
elementToXMLAff = schemaTypeToXML "aff"

data Aff'alternatives = Aff'alternatives
        { aff'alternatives_id   :: Maybe Xsd.ID
        , aff'alternatives_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Aff'alternatives where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Aff'alternatives a0 a1)
    schemaTypeToXML s x@Aff'alternatives{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ aff'alternatives_id x
                       , maybe [] (toXMLAttribute "base") $ aff'alternatives_base x
                       ]
            []

elementAff'alternatives :: XMLParser Aff'alternatives
elementAff'alternatives = parseSchemaType "aff-alternatives"
elementToXMLAff'alternatives :: Aff'alternatives -> [Content ()]
elementToXMLAff'alternatives = schemaTypeToXML "aff-alternatives"

data Alt'text = Alt'text
        { alt'text_content'type :: Maybe Xsd.XsdString
        , alt'text_id           :: Maybe Xsd.ID
        , alt'text_specific'use :: Maybe Xsd.XsdString
        , alt'text_base         :: Maybe Xsd.AnyURI
        , alt'text_lang         :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Alt'text where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Alt'text a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Alt'text{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ alt'text_content'type x
                       , maybe [] (toXMLAttribute "id") $ alt'text_id x
                       , maybe [] (toXMLAttribute "specific-use") $ alt'text_specific'use x
                       , maybe [] (toXMLAttribute "base") $ alt'text_base x
                       , maybe [] (toXMLAttribute "lang") $ alt'text_lang x
                       ]
            [
            ]

elementAlt'text :: XMLParser Alt'text
elementAlt'text = parseSchemaType "alt-text"
elementToXMLAlt'text :: Alt'text -> [Content ()]
elementToXMLAlt'text = schemaTypeToXML "alt-text"

data Alt'title = Alt'title
        { alt'title_alt'title'type :: Maybe Xsd.XsdString
        , alt'title_id             :: Maybe Xsd.ID
        , alt'title_specific'use   :: Maybe Xsd.XsdString
        , alt'title_base           :: Maybe Xsd.AnyURI
        , alt'title_lang           :: Maybe Xsd.XsdString
        , alt'title_choice0        :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Alt'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "alt-title-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Alt'title a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Alt'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "alt-title-type") $ alt'title_alt'title'type x
                       , maybe [] (toXMLAttribute "id") $ alt'title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ alt'title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ alt'title_base x
                       , maybe [] (toXMLAttribute "lang") $ alt'title_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ alt'title_choice0 x
            ]

elementAlt'title :: XMLParser Alt'title
elementAlt'title = parseSchemaType "alt-title"
elementToXMLAlt'title :: Alt'title -> [Content ()]
elementToXMLAlt'title = schemaTypeToXML "alt-title"

data Alternatives = Alternatives
        { alternatives_id   :: Maybe Xsd.ID
        , alternatives_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Alternatives where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Alternatives a0 a1)
    schemaTypeToXML s x@Alternatives{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ alternatives_id x
                       , maybe [] (toXMLAttribute "base") $ alternatives_base x
                       ]
            []

elementAlternatives :: XMLParser Alternatives
elementAlternatives = parseSchemaType "alternatives"
elementToXMLAlternatives :: Alternatives -> [Content ()]
elementToXMLAlternatives = schemaTypeToXML "alternatives"

data Annotation = Annotation
        { annotation_content'type :: Maybe Xsd.XsdString
        , annotation_id           :: Maybe Xsd.ID
        , annotation_specific'use :: Maybe Xsd.XsdString
        , annotation_base         :: Maybe Xsd.AnyURI
        , annotation_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Annotation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Annotation a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Annotation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ annotation_content'type x
                       , maybe [] (toXMLAttribute "id") $ annotation_id x
                       , maybe [] (toXMLAttribute "specific-use") $ annotation_specific'use x
                       , maybe [] (toXMLAttribute "base") $ annotation_base x
                       , maybe [] (toXMLAttribute "lang") $ annotation_lang x
                       ]
            []

elementAnnotation :: XMLParser Annotation
elementAnnotation = parseSchemaType "annotation"
elementToXMLAnnotation :: Annotation -> [Content ()]
elementToXMLAnnotation = schemaTypeToXML "annotation"

data Anonymous = Anonymous
        { anonymous_id           :: Maybe Xsd.ID
        , anonymous_specific'use :: Maybe Xsd.XsdString
        , anonymous_base         :: Maybe Xsd.AnyURI
        , anonymous_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Anonymous where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Anonymous a0 a1 a2 a3)
    schemaTypeToXML s x@Anonymous{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ anonymous_id x
                       , maybe [] (toXMLAttribute "specific-use") $ anonymous_specific'use x
                       , maybe [] (toXMLAttribute "base") $ anonymous_base x
                       , maybe [] (toXMLAttribute "lang") $ anonymous_lang x
                       ]
            []

elementAnonymous :: XMLParser Anonymous
elementAnonymous = parseSchemaType "anonymous"
elementToXMLAnonymous :: Anonymous -> [Content ()]
elementToXMLAnonymous = schemaTypeToXML "anonymous"

data App = App
        { app_content'type :: Maybe Xsd.XsdString
        , app_id           :: Maybe Xsd.ID
        , app_specific'use :: Maybe Xsd.XsdString
        , app_base         :: Maybe Xsd.AnyURI
        , app_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType App where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (App a0 a1 a2 a3 a4)
    schemaTypeToXML s x@App{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ app_content'type x
                       , maybe [] (toXMLAttribute "id") $ app_id x
                       , maybe [] (toXMLAttribute "specific-use") $ app_specific'use x
                       , maybe [] (toXMLAttribute "base") $ app_base x
                       , maybe [] (toXMLAttribute "lang") $ app_lang x
                       ]
            []

elementApp :: XMLParser App
elementApp = parseSchemaType "app"
elementToXMLApp :: App -> [Content ()]
elementToXMLApp = schemaTypeToXML "app"

data App'group = App'group
        { app'group_content'type :: Maybe Xsd.XsdString
        , app'group_id           :: Maybe Xsd.ID
        , app'group_specific'use :: Maybe Xsd.XsdString
        , app'group_base         :: Maybe Xsd.AnyURI
        , app'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType App'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (App'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@App'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ app'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ app'group_id x
                       , maybe [] (toXMLAttribute "specific-use") $ app'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ app'group_base x
                       , maybe [] (toXMLAttribute "lang") $ app'group_lang x
                       ]
            []

elementApp'group :: XMLParser App'group
elementApp'group = parseSchemaType "app-group"
elementToXMLApp'group :: App'group -> [Content ()]
elementToXMLApp'group = schemaTypeToXML "app-group"

data Array = Array
        { array_content'type :: Maybe Xsd.XsdString
        , array_id           :: Maybe Xsd.ID
        , array_orientation  :: Maybe Xsd.XsdString
        , array_specific'use :: Maybe Xsd.XsdString
        , array_base         :: Maybe Xsd.AnyURI
        , array_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Array where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "orientation" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Array a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Array{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ array_content'type x
                       , maybe [] (toXMLAttribute "id") $ array_id x
                       , maybe [] (toXMLAttribute "orientation") $ array_orientation x
                       , maybe [] (toXMLAttribute "specific-use") $ array_specific'use x
                       , maybe [] (toXMLAttribute "base") $ array_base x
                       , maybe [] (toXMLAttribute "lang") $ array_lang x
                       ]
            []

elementArray :: XMLParser Array
elementArray = parseSchemaType "array"
elementToXMLArray :: Array -> [Content ()]
elementToXMLArray = schemaTypeToXML "array"

data Article = Article
        { article_article'type :: Maybe Xsd.XsdString
        , article_dtd'version  :: Maybe Xsd.XsdString
        , article_id           :: Maybe Xsd.ID
        , article_specific'use :: Maybe Xsd.XsdString
        , article_base         :: Maybe Xsd.AnyURI
        , article_lang         :: Maybe Xsd.XsdString
        , article_children     :: [OneOf4 Front Body Back Floats'group]
        }
        deriving (Eq,Show)
instance SchemaType Article where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "article-type" e pos
        a1 <- optional $ getAttribute "dtd-version" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Article a0 a1 a2 a3 a4 a5)
            `apply` many (oneOf' [ ("", fmap OneOf4 elementFront)
                                 , ("", fmap TwoOf4 elementBody)
                                 , ("", fmap ThreeOf4 elementBack)
                                 , ("", fmap FourOf4 elementFloats'group)
                                 ] )
    schemaTypeToXML s x@Article{} =
        toXMLElement s [ maybe [] (toXMLAttribute "article-type") $ article_article'type x
                       , maybe [] (toXMLAttribute "dtd-version") $ article_dtd'version x
                       , maybe [] (toXMLAttribute "id") $ article_id x
                       , maybe [] (toXMLAttribute "specific-use") $ article_specific'use x
                       , maybe [] (toXMLAttribute "base") $ article_base x
                       , maybe [] (toXMLAttribute "lang") $ article_lang x
                       ]
            []

elementArticle :: XMLParser Article
elementArticle = parseSchemaType "article"
elementToXMLArticle :: Article -> [Content ()]
elementToXMLArticle = schemaTypeToXML "article"

data Article'categories = Article'categories
        { article'categories_id   :: Maybe Xsd.ID
        , article'categories_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Article'categories where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Article'categories a0 a1)
    schemaTypeToXML s x@Article'categories{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ article'categories_id x
                       , maybe [] (toXMLAttribute "base") $ article'categories_base x
                       ]
            []

elementArticle'categories :: XMLParser Article'categories
elementArticle'categories = parseSchemaType "article-categories"
elementToXMLArticle'categories :: Article'categories -> [Content ()]
elementToXMLArticle'categories = schemaTypeToXML "article-categories"

data Article'id = Article'id
        { article'id_id           :: Maybe Xsd.ID
        , article'id_pub'id'type  :: Maybe Xsd.XsdString
        , article'id_specific'use :: Maybe Xsd.XsdString
        , article'id_base         :: Maybe Xsd.AnyURI
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Article'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "pub-id-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Article'id a0 a1 a2 a3)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Article'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ article'id_id x
                       , maybe [] (toXMLAttribute "pub-id-type") $ article'id_pub'id'type x
                       , maybe [] (toXMLAttribute "specific-use") $ article'id_specific'use x
                       , maybe [] (toXMLAttribute "base") $ article'id_base x
                       ]
            [
            ]

elementArticle'id :: XMLParser Article'id
elementArticle'id = parseSchemaType "article-id"
elementToXMLArticle'id :: Article'id -> [Content ()]
elementToXMLArticle'id = schemaTypeToXML "article-id"

data Article'meta = Article'meta
        { article'meta_id   :: Maybe Xsd.ID
        , article'meta_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Article'meta where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Article'meta a0 a1)
    schemaTypeToXML s x@Article'meta{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ article'meta_id x
                       , maybe [] (toXMLAttribute "base") $ article'meta_base x
                       ]
            []

elementArticle'meta :: XMLParser Article'meta
elementArticle'meta = parseSchemaType "article-meta"
elementToXMLArticle'meta :: Article'meta -> [Content ()]
elementToXMLArticle'meta = schemaTypeToXML "article-meta"

data Article'title = Article'title
        { article'title_id      :: Maybe Xsd.ID
        , article'title_base    :: Maybe Xsd.AnyURI
        , article'title_lang    :: Maybe Xsd.XsdString
        , article'title_choice0 :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Article'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        a2 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Article'title a0 a1 a2)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Article'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ article'title_id x
                       , maybe [] (toXMLAttribute "base") $ article'title_base x
                       , maybe [] (toXMLAttribute "lang") $ article'title_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ article'title_choice0 x
            ]

elementArticle'title :: XMLParser Article'title
elementArticle'title = parseSchemaType "article-title"
elementToXMLArticle'title :: Article'title -> [Content ()]
elementToXMLArticle'title = schemaTypeToXML "article-title"

data Attrib = Attrib
        { attrib_id           :: Maybe Xsd.ID
        , attrib_specific'use :: Maybe Xsd.XsdString
        , attrib_base         :: Maybe Xsd.AnyURI
        , attrib_lang         :: Maybe Xsd.XsdString
        , attrib_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Attrib where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Attrib a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Attrib{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ attrib_id x
                       , maybe [] (toXMLAttribute "specific-use") $ attrib_specific'use x
                       , maybe [] (toXMLAttribute "base") $ attrib_base x
                       , maybe [] (toXMLAttribute "lang") $ attrib_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ attrib_choice0 x
            ]

elementAttrib :: XMLParser Attrib
elementAttrib = parseSchemaType "attrib"
elementToXMLAttrib :: Attrib -> [Content ()]
elementToXMLAttrib = schemaTypeToXML "attrib"

data Author'comment = Author'comment
        { author'comment_content'type :: Maybe Xsd.XsdString
        , author'comment_id           :: Maybe Xsd.ID
        , author'comment_specific'use :: Maybe Xsd.XsdString
        , author'comment_base         :: Maybe Xsd.AnyURI
        , author'comment_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Author'comment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Author'comment a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Author'comment{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ author'comment_content'type x
                       , maybe [] (toXMLAttribute "id") $ author'comment_id x
                       , maybe [] (toXMLAttribute "specific-use") $ author'comment_specific'use x
                       , maybe [] (toXMLAttribute "base") $ author'comment_base x
                       , maybe [] (toXMLAttribute "lang") $ author'comment_lang x
                       ]
            []

elementAuthor'comment :: XMLParser Author'comment
elementAuthor'comment = parseSchemaType "author-comment"
elementToXMLAuthor'comment :: Author'comment -> [Content ()]
elementToXMLAuthor'comment = schemaTypeToXML "author-comment"

data Author'notes = Author'notes
        { author'notes_id           :: Maybe Xsd.ID
        , author'notes_rid          :: Maybe Xsd.IDREFS
        , author'notes_specific'use :: Maybe Xsd.XsdString
        , author'notes_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Author'notes where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "rid" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Author'notes a0 a1 a2 a3)
    schemaTypeToXML s x@Author'notes{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ author'notes_id x
                       , maybe [] (toXMLAttribute "rid") $ author'notes_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ author'notes_specific'use x
                       , maybe [] (toXMLAttribute "base") $ author'notes_base x
                       ]
            []

elementAuthor'notes :: XMLParser Author'notes
elementAuthor'notes = parseSchemaType "author-notes"
elementToXMLAuthor'notes :: Author'notes -> [Content ()]
elementToXMLAuthor'notes = schemaTypeToXML "author-notes"

data Award'group = Award'group
        { award'group_award'type   :: Maybe Xsd.XsdString
        , award'group_id           :: Maybe Xsd.ID
        , award'group_rid          :: Maybe Xsd.IDREFS
        , award'group_specific'use :: Maybe Xsd.XsdString
        , award'group_actuate      :: Maybe Xsd.XsdString
        , award'group_href         :: Maybe Xsd.AnyURI
        , award'group_role         :: Maybe Xsd.XsdString
        , award'group_show         :: Maybe Xsd.XsdString
        , award'group_title        :: Maybe Xsd.XsdString
        , award'group_type         :: Maybe Xsd.XsdString
        , award'group_base         :: Maybe Xsd.AnyURI
        , award'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Award'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "award-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "rid" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "actuate" e pos
        a5 <- optional $ getAttribute "href" e pos
        a6 <- optional $ getAttribute "role" e pos
        a7 <- optional $ getAttribute "show" e pos
        a8 <- optional $ getAttribute "title" e pos
        a9 <- optional $ getAttribute "type" e pos
        a10 <- optional $ getAttribute "base" e pos
        a11 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Award'group a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
    schemaTypeToXML s x@Award'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "award-type") $ award'group_award'type x
                       , maybe [] (toXMLAttribute "id") $ award'group_id x
                       , maybe [] (toXMLAttribute "rid") $ award'group_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ award'group_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ award'group_actuate x
                       , maybe [] (toXMLAttribute "href") $ award'group_href x
                       , maybe [] (toXMLAttribute "role") $ award'group_role x
                       , maybe [] (toXMLAttribute "show") $ award'group_show x
                       , maybe [] (toXMLAttribute "title") $ award'group_title x
                       , maybe [] (toXMLAttribute "type") $ award'group_type x
                       , maybe [] (toXMLAttribute "base") $ award'group_base x
                       , maybe [] (toXMLAttribute "lang") $ award'group_lang x
                       ]
            []

elementAward'group :: XMLParser Award'group
elementAward'group = parseSchemaType "award-group"
elementToXMLAward'group :: Award'group -> [Content ()]
elementToXMLAward'group = schemaTypeToXML "award-group"

data Award'id = Award'id
        { award'id_id           :: Maybe Xsd.ID
        , award'id_rid          :: Maybe Xsd.IDREFS
        , award'id_specific'use :: Maybe Xsd.XsdString
        , award'id_base         :: Maybe Xsd.AnyURI
        , award'id_lang         :: Maybe Xsd.XsdString
        , award'id_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Award'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "rid" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Award'id a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Award'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ award'id_id x
                       , maybe [] (toXMLAttribute "rid") $ award'id_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ award'id_specific'use x
                       , maybe [] (toXMLAttribute "base") $ award'id_base x
                       , maybe [] (toXMLAttribute "lang") $ award'id_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ award'id_choice0 x
            ]

elementAward'id :: XMLParser Award'id
elementAward'id = parseSchemaType "award-id"
elementToXMLAward'id :: Award'id -> [Content ()]
elementToXMLAward'id = schemaTypeToXML "award-id"

data Back = Back
        { back_id   :: Maybe Xsd.ID
        , back_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Back where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Back a0 a1)
    schemaTypeToXML s x@Back{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ back_id x
                       , maybe [] (toXMLAttribute "base") $ back_base x
                       ]
            []

elementBack :: XMLParser Back
elementBack = parseSchemaType "back"
elementToXMLBack :: Back -> [Content ()]
elementToXMLBack = schemaTypeToXML "back"

data Bio = Bio
        { bio_content'type :: Maybe Xsd.XsdString
        , bio_id           :: Maybe Xsd.ID
        , bio_rid          :: Maybe Xsd.IDREFS
        , bio_specific'use :: Maybe Xsd.XsdString
        , bio_actuate      :: Maybe Xsd.XsdString
        , bio_href         :: Maybe Xsd.AnyURI
        , bio_role         :: Maybe Xsd.XsdString
        , bio_show         :: Maybe Xsd.XsdString
        , bio_title        :: Maybe Xsd.XsdString
        , bio_type         :: Maybe Xsd.XsdString
        , bio_base         :: Maybe Xsd.AnyURI
        , bio_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Bio where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "rid" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "actuate" e pos
        a5 <- optional $ getAttribute "href" e pos
        a6 <- optional $ getAttribute "role" e pos
        a7 <- optional $ getAttribute "show" e pos
        a8 <- optional $ getAttribute "title" e pos
        a9 <- optional $ getAttribute "type" e pos
        a10 <- optional $ getAttribute "base" e pos
        a11 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Bio a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
    schemaTypeToXML s x@Bio{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ bio_content'type x
                       , maybe [] (toXMLAttribute "id") $ bio_id x
                       , maybe [] (toXMLAttribute "rid") $ bio_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ bio_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ bio_actuate x
                       , maybe [] (toXMLAttribute "href") $ bio_href x
                       , maybe [] (toXMLAttribute "role") $ bio_role x
                       , maybe [] (toXMLAttribute "show") $ bio_show x
                       , maybe [] (toXMLAttribute "title") $ bio_title x
                       , maybe [] (toXMLAttribute "type") $ bio_type x
                       , maybe [] (toXMLAttribute "base") $ bio_base x
                       , maybe [] (toXMLAttribute "lang") $ bio_lang x
                       ]
            []

elementBio :: XMLParser Bio
elementBio = parseSchemaType "bio"
elementToXMLBio :: Bio -> [Content ()]
elementToXMLBio = schemaTypeToXML "bio"

data Body = Body
        { body_id           :: Maybe Xsd.ID
        , body_specific'use :: Maybe Xsd.XsdString
        , body_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Body where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Body a0 a1 a2)
    schemaTypeToXML s x@Body{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ body_id x
                       , maybe [] (toXMLAttribute "specific-use") $ body_specific'use x
                       , maybe [] (toXMLAttribute "base") $ body_base x
                       ]
            []

elementBody :: XMLParser Body
elementBody = parseSchemaType "body"
elementToXMLBody :: Body -> [Content ()]
elementToXMLBody = schemaTypeToXML "body"

data Bold = Bold
        { bold_id           :: Maybe Xsd.ID
        , bold_specific'use :: Maybe Xsd.XsdString
        , bold_toggle       :: Maybe Xsd.XsdString
        , bold_base         :: Maybe Xsd.AnyURI
        , bold_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Bold where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "toggle" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Bold a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Bold{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ bold_id x
                       , maybe [] (toXMLAttribute "specific-use") $ bold_specific'use x
                       , maybe [] (toXMLAttribute "toggle") $ bold_toggle x
                       , maybe [] (toXMLAttribute "base") $ bold_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ bold_choice0 x
            ]

elementBold :: XMLParser Bold
elementBold = parseSchemaType "bold"
elementToXMLBold :: Bold -> [Content ()]
elementToXMLBold = schemaTypeToXML "bold"

data Boxed'text = Boxed'text
        { boxed'text_content'type :: Maybe Xsd.XsdString
        , boxed'text_id           :: Maybe Xsd.ID
        , boxed'text_orientation  :: Maybe Xsd.XsdString
        , boxed'text_position     :: Maybe Xsd.XsdString
        , boxed'text_specific'use :: Maybe Xsd.XsdString
        , boxed'text_base         :: Maybe Xsd.AnyURI
        , boxed'text_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Boxed'text where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "orientation" e pos
        a3 <- optional $ getAttribute "position" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Boxed'text a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Boxed'text{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ boxed'text_content'type x
                       , maybe [] (toXMLAttribute "id") $ boxed'text_id x
                       , maybe [] (toXMLAttribute "orientation") $ boxed'text_orientation x
                       , maybe [] (toXMLAttribute "position") $ boxed'text_position x
                       , maybe [] (toXMLAttribute "specific-use") $ boxed'text_specific'use x
                       , maybe [] (toXMLAttribute "base") $ boxed'text_base x
                       , maybe [] (toXMLAttribute "lang") $ boxed'text_lang x
                       ]
            []

elementBoxed'text :: XMLParser Boxed'text
elementBoxed'text = parseSchemaType "boxed-text"
elementToXMLBoxed'text :: Boxed'text -> [Content ()]
elementToXMLBoxed'text = schemaTypeToXML "boxed-text"

data Break = Break
        { break_id   :: Maybe Xsd.ID
        , break_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Break where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Break a0 a1)
    schemaTypeToXML s x@Break{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ break_id x
                       , maybe [] (toXMLAttribute "base") $ break_base x
                       ]
            []

elementBreak :: XMLParser Break
elementBreak = parseSchemaType "break"
elementToXMLBreak :: Break -> [Content ()]
elementToXMLBreak = schemaTypeToXML "break"

data Caption = Caption
        { caption_content'type :: Maybe Xsd.XsdString
        , caption_id           :: Maybe Xsd.ID
        , caption_specific'use :: Maybe Xsd.XsdString
        , caption_style        :: Maybe Xsd.XsdString
        , caption_base         :: Maybe Xsd.AnyURI
        , caption_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Caption where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "style" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Caption a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Caption{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ caption_content'type x
                       , maybe [] (toXMLAttribute "id") $ caption_id x
                       , maybe [] (toXMLAttribute "specific-use") $ caption_specific'use x
                       , maybe [] (toXMLAttribute "style") $ caption_style x
                       , maybe [] (toXMLAttribute "base") $ caption_base x
                       , maybe [] (toXMLAttribute "lang") $ caption_lang x
                       ]
            []

elementCaption :: XMLParser Caption
elementCaption = parseSchemaType "caption"
elementToXMLCaption :: Caption -> [Content ()]
elementToXMLCaption = schemaTypeToXML "caption"

data Chapter'title = Chapter'title
        { chapter'title_id           :: Maybe Xsd.ID
        , chapter'title_specific'use :: Maybe Xsd.XsdString
        , chapter'title_base         :: Maybe Xsd.AnyURI
        , chapter'title_lang         :: Maybe Xsd.XsdString
        , chapter'title_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Chapter'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Chapter'title a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Chapter'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ chapter'title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ chapter'title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ chapter'title_base x
                       , maybe [] (toXMLAttribute "lang") $ chapter'title_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ chapter'title_choice0 x
            ]

elementChapter'title :: XMLParser Chapter'title
elementChapter'title = parseSchemaType "chapter-title"
elementToXMLChapter'title :: Chapter'title -> [Content ()]
elementToXMLChapter'title = schemaTypeToXML "chapter-title"

data Chem'struct = Chem'struct
        { chem'struct_content'type :: Maybe Xsd.XsdString
        , chem'struct_id           :: Maybe Xsd.ID
        , chem'struct_specific'use :: Maybe Xsd.XsdString
        , chem'struct_actuate      :: Maybe Xsd.XsdString
        , chem'struct_href         :: Maybe Xsd.AnyURI
        , chem'struct_role         :: Maybe Xsd.XsdString
        , chem'struct_show         :: Maybe Xsd.XsdString
        , chem'struct_title        :: Maybe Xsd.XsdString
        , chem'struct_type         :: Maybe Xsd.XsdString
        , chem'struct_base         :: Maybe Xsd.AnyURI
        , chem'struct_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Chem'struct where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "actuate" e pos
        a4 <- optional $ getAttribute "href" e pos
        a5 <- optional $ getAttribute "role" e pos
        a6 <- optional $ getAttribute "show" e pos
        a7 <- optional $ getAttribute "title" e pos
        a8 <- optional $ getAttribute "type" e pos
        a9 <- optional $ getAttribute "base" e pos
        a10 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Chem'struct a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    schemaTypeToXML s x@Chem'struct{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ chem'struct_content'type x
                       , maybe [] (toXMLAttribute "id") $ chem'struct_id x
                       , maybe [] (toXMLAttribute "specific-use") $ chem'struct_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ chem'struct_actuate x
                       , maybe [] (toXMLAttribute "href") $ chem'struct_href x
                       , maybe [] (toXMLAttribute "role") $ chem'struct_role x
                       , maybe [] (toXMLAttribute "show") $ chem'struct_show x
                       , maybe [] (toXMLAttribute "title") $ chem'struct_title x
                       , maybe [] (toXMLAttribute "type") $ chem'struct_type x
                       , maybe [] (toXMLAttribute "base") $ chem'struct_base x
                       , maybe [] (toXMLAttribute "lang") $ chem'struct_lang x
                       ]
            []

elementChem'struct :: XMLParser Chem'struct
elementChem'struct = parseSchemaType "chem-struct"
elementToXMLChem'struct :: Chem'struct -> [Content ()]
elementToXMLChem'struct = schemaTypeToXML "chem-struct"

data Chem'struct'wrap = Chem'struct'wrap
        { chem'struct'wrap_content'type :: Maybe Xsd.XsdString
        , chem'struct'wrap_id           :: Maybe Xsd.ID
        , chem'struct'wrap_orientation  :: Maybe Xsd.XsdString
        , chem'struct'wrap_position     :: Maybe Xsd.XsdString
        , chem'struct'wrap_specific'use :: Maybe Xsd.XsdString
        , chem'struct'wrap_base         :: Maybe Xsd.AnyURI
        , chem'struct'wrap_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Chem'struct'wrap where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "orientation" e pos
        a3 <- optional $ getAttribute "position" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Chem'struct'wrap a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Chem'struct'wrap{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ chem'struct'wrap_content'type x
                       , maybe [] (toXMLAttribute "id") $ chem'struct'wrap_id x
                       , maybe [] (toXMLAttribute "orientation") $ chem'struct'wrap_orientation x
                       , maybe [] (toXMLAttribute "position") $ chem'struct'wrap_position x
                       , maybe [] (toXMLAttribute "specific-use") $ chem'struct'wrap_specific'use x
                       , maybe [] (toXMLAttribute "base") $ chem'struct'wrap_base x
                       , maybe [] (toXMLAttribute "lang") $ chem'struct'wrap_lang x
                       ]
            []

elementChem'struct'wrap :: XMLParser Chem'struct'wrap
elementChem'struct'wrap = parseSchemaType "chem-struct-wrap"
elementToXMLChem'struct'wrap :: Chem'struct'wrap -> [Content ()]
elementToXMLChem'struct'wrap = schemaTypeToXML "chem-struct-wrap"

data Citation'alternatives = Citation'alternatives
        { citation'alternatives_id   :: Maybe Xsd.ID
        , citation'alternatives_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Citation'alternatives where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Citation'alternatives a0 a1)
    schemaTypeToXML s x@Citation'alternatives{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ citation'alternatives_id x
                       , maybe [] (toXMLAttribute "base") $ citation'alternatives_base x
                       ]
            []

elementCitation'alternatives :: XMLParser Citation'alternatives
elementCitation'alternatives = parseSchemaType "citation-alternatives"
elementToXMLCitation'alternatives :: Citation'alternatives -> [Content ()]
elementToXMLCitation'alternatives = schemaTypeToXML "citation-alternatives"

data City = City
        { city_content'type :: Maybe Xsd.XsdString
        , city_id           :: Maybe Xsd.ID
        , city_specific'use :: Maybe Xsd.XsdString
        , city_base         :: Maybe Xsd.AnyURI
        , city_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType City where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (City a0 a1 a2 a3 a4)
    schemaTypeToXML s x@City{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ city_content'type x
                       , maybe [] (toXMLAttribute "id") $ city_id x
                       , maybe [] (toXMLAttribute "specific-use") $ city_specific'use x
                       , maybe [] (toXMLAttribute "base") $ city_base x
                       , maybe [] (toXMLAttribute "lang") $ city_lang x
                       ]
            []

elementCity :: XMLParser City
elementCity = parseSchemaType "city"
elementToXMLCity :: City -> [Content ()]
elementToXMLCity = schemaTypeToXML "city"

data Code = Code
        { code_code'type        :: Maybe Xsd.XsdString
        , code_code'version     :: Maybe Xsd.XsdString
        , code_executable       :: Maybe Xsd.XsdString
        , code_id               :: Maybe Xsd.ID
        , code_language         :: Maybe Xsd.XsdString
        , code_language'version :: Maybe Xsd.XsdString
        , code_orientation      :: Maybe Xsd.XsdString
        , code_platforms        :: Maybe Xsd.XsdString
        , code_position         :: Maybe Xsd.XsdString
        , code_specific'use     :: Maybe Xsd.XsdString
        , code_base             :: Maybe Xsd.AnyURI
        , code_lang             :: Maybe Xsd.XsdString
        , code_space            :: Maybe Xsd.XsdString
        , code_choice0          :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Code where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "code-type" e pos
        a1 <- optional $ getAttribute "code-version" e pos
        a2 <- optional $ getAttribute "executable" e pos
        a3 <- optional $ getAttribute "id" e pos
        a4 <- optional $ getAttribute "language" e pos
        a5 <- optional $ getAttribute "language-version" e pos
        a6 <- optional $ getAttribute "orientation" e pos
        a7 <- optional $ getAttribute "platforms" e pos
        a8 <- optional $ getAttribute "position" e pos
        a9 <- optional $ getAttribute "specific-use" e pos
        a10 <- optional $ getAttribute "base" e pos
        a11 <- optional $ getAttribute "lang" e pos
        a12 <- optional $ getAttribute "space" e pos
        commit $ interior e $ return (Code a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Code{} =
        toXMLElement s [ maybe [] (toXMLAttribute "code-type") $ code_code'type x
                       , maybe [] (toXMLAttribute "code-version") $ code_code'version x
                       , maybe [] (toXMLAttribute "executable") $ code_executable x
                       , maybe [] (toXMLAttribute "id") $ code_id x
                       , maybe [] (toXMLAttribute "language") $ code_language x
                       , maybe [] (toXMLAttribute "language-version") $ code_language'version x
                       , maybe [] (toXMLAttribute "orientation") $ code_orientation x
                       , maybe [] (toXMLAttribute "platforms") $ code_platforms x
                       , maybe [] (toXMLAttribute "position") $ code_position x
                       , maybe [] (toXMLAttribute "specific-use") $ code_specific'use x
                       , maybe [] (toXMLAttribute "base") $ code_base x
                       , maybe [] (toXMLAttribute "lang") $ code_lang x
                       , maybe [] (toXMLAttribute "space") $ code_space x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ code_choice0 x
            ]

elementCode :: XMLParser Code
elementCode = parseSchemaType "code"
elementToXMLCode :: Code -> [Content ()]
elementToXMLCode = schemaTypeToXML "code"

data Col = Col
        { col_align        :: Maybe Xsd.XsdString
        , col_char         :: Maybe Xsd.XsdString
        , col_charoff      :: Maybe Xsd.XsdString
        , col_content'type :: Maybe Xsd.XsdString
        , col_id           :: Maybe Xsd.ID
        , col_span         :: Maybe Xsd.XsdString
        , col_style        :: Maybe Xsd.XsdString
        , col_valign       :: Maybe Xsd.XsdString
        , col_width        :: Maybe Xsd.XsdString
        , col_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Col where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "align" e pos
        a1 <- optional $ getAttribute "char" e pos
        a2 <- optional $ getAttribute "charoff" e pos
        a3 <- optional $ getAttribute "content-type" e pos
        a4 <- optional $ getAttribute "id" e pos
        a5 <- optional $ getAttribute "span" e pos
        a6 <- optional $ getAttribute "style" e pos
        a7 <- optional $ getAttribute "valign" e pos
        a8 <- optional $ getAttribute "width" e pos
        a9 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Col a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
    schemaTypeToXML s x@Col{} =
        toXMLElement s [ maybe [] (toXMLAttribute "align") $ col_align x
                       , maybe [] (toXMLAttribute "char") $ col_char x
                       , maybe [] (toXMLAttribute "charoff") $ col_charoff x
                       , maybe [] (toXMLAttribute "content-type") $ col_content'type x
                       , maybe [] (toXMLAttribute "id") $ col_id x
                       , maybe [] (toXMLAttribute "span") $ col_span x
                       , maybe [] (toXMLAttribute "style") $ col_style x
                       , maybe [] (toXMLAttribute "valign") $ col_valign x
                       , maybe [] (toXMLAttribute "width") $ col_width x
                       , maybe [] (toXMLAttribute "base") $ col_base x
                       ]
            []

elementCol :: XMLParser Col
elementCol = parseSchemaType "col"
elementToXMLCol :: Col -> [Content ()]
elementToXMLCol = schemaTypeToXML "col"

data Colgroup = Colgroup
        { colgroup_align        :: Maybe Xsd.XsdString
        , colgroup_char         :: Maybe Xsd.XsdString
        , colgroup_charoff      :: Maybe Xsd.XsdString
        , colgroup_content'type :: Maybe Xsd.XsdString
        , colgroup_id           :: Maybe Xsd.ID
        , colgroup_span         :: Maybe Xsd.XsdString
        , colgroup_style        :: Maybe Xsd.XsdString
        , colgroup_valign       :: Maybe Xsd.XsdString
        , colgroup_width        :: Maybe Xsd.XsdString
        , colgroup_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Colgroup where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "align" e pos
        a1 <- optional $ getAttribute "char" e pos
        a2 <- optional $ getAttribute "charoff" e pos
        a3 <- optional $ getAttribute "content-type" e pos
        a4 <- optional $ getAttribute "id" e pos
        a5 <- optional $ getAttribute "span" e pos
        a6 <- optional $ getAttribute "style" e pos
        a7 <- optional $ getAttribute "valign" e pos
        a8 <- optional $ getAttribute "width" e pos
        a9 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Colgroup a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
    schemaTypeToXML s x@Colgroup{} =
        toXMLElement s [ maybe [] (toXMLAttribute "align") $ colgroup_align x
                       , maybe [] (toXMLAttribute "char") $ colgroup_char x
                       , maybe [] (toXMLAttribute "charoff") $ colgroup_charoff x
                       , maybe [] (toXMLAttribute "content-type") $ colgroup_content'type x
                       , maybe [] (toXMLAttribute "id") $ colgroup_id x
                       , maybe [] (toXMLAttribute "span") $ colgroup_span x
                       , maybe [] (toXMLAttribute "style") $ colgroup_style x
                       , maybe [] (toXMLAttribute "valign") $ colgroup_valign x
                       , maybe [] (toXMLAttribute "width") $ colgroup_width x
                       , maybe [] (toXMLAttribute "base") $ colgroup_base x
                       ]
            []

elementColgroup :: XMLParser Colgroup
elementColgroup = parseSchemaType "colgroup"
elementToXMLColgroup :: Colgroup -> [Content ()]
elementToXMLColgroup = schemaTypeToXML "colgroup"

data Collab = Collab
        { collab_collab'type  :: Maybe Xsd.XsdString
        , collab_id           :: Maybe Xsd.ID
        , collab_specific'use :: Maybe Xsd.XsdString
        , collab_symbol       :: Maybe Xsd.XsdString
        , collab_actuate      :: Maybe Xsd.XsdString
        , collab_href         :: Maybe Xsd.AnyURI
        , collab_role         :: Maybe Xsd.XsdString
        , collab_show         :: Maybe Xsd.XsdString
        , collab_title        :: Maybe Xsd.XsdString
        , collab_type         :: Maybe Xsd.XsdString
        , collab_base         :: Maybe Xsd.AnyURI
        , collab_lang         :: Maybe Xsd.XsdString
        , collab_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Collab where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "collab-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "symbol" e pos
        a4 <- optional $ getAttribute "actuate" e pos
        a5 <- optional $ getAttribute "href" e pos
        a6 <- optional $ getAttribute "role" e pos
        a7 <- optional $ getAttribute "show" e pos
        a8 <- optional $ getAttribute "title" e pos
        a9 <- optional $ getAttribute "type" e pos
        a10 <- optional $ getAttribute "base" e pos
        a11 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Collab a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Collab{} =
        toXMLElement s [ maybe [] (toXMLAttribute "collab-type") $ collab_collab'type x
                       , maybe [] (toXMLAttribute "id") $ collab_id x
                       , maybe [] (toXMLAttribute "specific-use") $ collab_specific'use x
                       , maybe [] (toXMLAttribute "symbol") $ collab_symbol x
                       , maybe [] (toXMLAttribute "actuate") $ collab_actuate x
                       , maybe [] (toXMLAttribute "href") $ collab_href x
                       , maybe [] (toXMLAttribute "role") $ collab_role x
                       , maybe [] (toXMLAttribute "show") $ collab_show x
                       , maybe [] (toXMLAttribute "title") $ collab_title x
                       , maybe [] (toXMLAttribute "type") $ collab_type x
                       , maybe [] (toXMLAttribute "base") $ collab_base x
                       , maybe [] (toXMLAttribute "lang") $ collab_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ collab_choice0 x
            ]

elementCollab :: XMLParser Collab
elementCollab = parseSchemaType "collab"
elementToXMLCollab :: Collab -> [Content ()]
elementToXMLCollab = schemaTypeToXML "collab"

data Collab'alternatives = Collab'alternatives
        { collab'alternatives_id   :: Maybe Xsd.ID
        , collab'alternatives_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Collab'alternatives where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Collab'alternatives a0 a1)
    schemaTypeToXML s x@Collab'alternatives{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ collab'alternatives_id x
                       , maybe [] (toXMLAttribute "base") $ collab'alternatives_base x
                       ]
            []

elementCollab'alternatives :: XMLParser Collab'alternatives
elementCollab'alternatives = parseSchemaType "collab-alternatives"
elementToXMLCollab'alternatives :: Collab'alternatives -> [Content ()]
elementToXMLCollab'alternatives = schemaTypeToXML "collab-alternatives"

data Comment = Comment
        { comment_content'type :: Maybe Xsd.XsdString
        , comment_id           :: Maybe Xsd.ID
        , comment_specific'use :: Maybe Xsd.XsdString
        , comment_base         :: Maybe Xsd.AnyURI
        , comment_lang         :: Maybe Xsd.XsdString
        , comment_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Comment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Comment a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Comment{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ comment_content'type x
                       , maybe [] (toXMLAttribute "id") $ comment_id x
                       , maybe [] (toXMLAttribute "specific-use") $ comment_specific'use x
                       , maybe [] (toXMLAttribute "base") $ comment_base x
                       , maybe [] (toXMLAttribute "lang") $ comment_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ comment_choice0 x
            ]

elementComment :: XMLParser Comment
elementComment = parseSchemaType "comment"
elementToXMLComment :: Comment -> [Content ()]
elementToXMLComment = schemaTypeToXML "comment"

data Compound'kwd = Compound'kwd
        { compound'kwd_content'type :: Maybe Xsd.XsdString
        , compound'kwd_id           :: Maybe Xsd.ID
        , compound'kwd_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Compound'kwd where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Compound'kwd a0 a1 a2)
    schemaTypeToXML s x@Compound'kwd{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ compound'kwd_content'type x
                       , maybe [] (toXMLAttribute "id") $ compound'kwd_id x
                       , maybe [] (toXMLAttribute "base") $ compound'kwd_base x
                       ]
            []

elementCompound'kwd :: XMLParser Compound'kwd
elementCompound'kwd = parseSchemaType "compound-kwd"
elementToXMLCompound'kwd :: Compound'kwd -> [Content ()]
elementToXMLCompound'kwd = schemaTypeToXML "compound-kwd"

data Compound'kwd'part = Compound'kwd'part
        { compound'kwd'part_content'type :: Maybe Xsd.XsdString
        , compound'kwd'part_id           :: Maybe Xsd.ID
        , compound'kwd'part_base         :: Maybe Xsd.AnyURI
        , compound'kwd'part_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Compound'kwd'part where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Compound'kwd'part a0 a1 a2)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Compound'kwd'part{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ compound'kwd'part_content'type x
                       , maybe [] (toXMLAttribute "id") $ compound'kwd'part_id x
                       , maybe [] (toXMLAttribute "base") $ compound'kwd'part_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ compound'kwd'part_choice0 x
            ]

elementCompound'kwd'part :: XMLParser Compound'kwd'part
elementCompound'kwd'part = parseSchemaType "compound-kwd-part"
elementToXMLCompound'kwd'part :: Compound'kwd'part -> [Content ()]
elementToXMLCompound'kwd'part = schemaTypeToXML "compound-kwd-part"

data Compound'subject = Compound'subject
        { compound'subject_content'type :: Maybe Xsd.XsdString
        , compound'subject_id           :: Maybe Xsd.ID
        , compound'subject_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Compound'subject where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Compound'subject a0 a1 a2)
    schemaTypeToXML s x@Compound'subject{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ compound'subject_content'type x
                       , maybe [] (toXMLAttribute "id") $ compound'subject_id x
                       , maybe [] (toXMLAttribute "base") $ compound'subject_base x
                       ]
            []

elementCompound'subject :: XMLParser Compound'subject
elementCompound'subject = parseSchemaType "compound-subject"
elementToXMLCompound'subject :: Compound'subject -> [Content ()]
elementToXMLCompound'subject = schemaTypeToXML "compound-subject"

data Compound'subject'part = Compound'subject'part
        { compound'subject'part_content'type :: Maybe Xsd.XsdString
        , compound'subject'part_id           :: Maybe Xsd.ID
        , compound'subject'part_base         :: Maybe Xsd.AnyURI
        , compound'subject'part_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Compound'subject'part where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Compound'subject'part a0 a1 a2)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Compound'subject'part{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ compound'subject'part_content'type x
                       , maybe [] (toXMLAttribute "id") $ compound'subject'part_id x
                       , maybe [] (toXMLAttribute "base") $ compound'subject'part_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ compound'subject'part_choice0 x
            ]

elementCompound'subject'part :: XMLParser Compound'subject'part
elementCompound'subject'part = parseSchemaType "compound-subject-part"
elementToXMLCompound'subject'part :: Compound'subject'part -> [Content ()]
elementToXMLCompound'subject'part = schemaTypeToXML "compound-subject-part"

data Conf'acronym = Conf'acronym
        { conf'acronym_content'type :: Maybe Xsd.XsdString
        , conf'acronym_id           :: Maybe Xsd.ID
        , conf'acronym_specific'use :: Maybe Xsd.XsdString
        , conf'acronym_base         :: Maybe Xsd.AnyURI
        , conf'acronym_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Conf'acronym where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Conf'acronym a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Conf'acronym{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ conf'acronym_content'type x
                       , maybe [] (toXMLAttribute "id") $ conf'acronym_id x
                       , maybe [] (toXMLAttribute "specific-use") $ conf'acronym_specific'use x
                       , maybe [] (toXMLAttribute "base") $ conf'acronym_base x
                       , maybe [] (toXMLAttribute "lang") $ conf'acronym_lang x
                       ]
            []

elementConf'acronym :: XMLParser Conf'acronym
elementConf'acronym = parseSchemaType "conf-acronym"
elementToXMLConf'acronym :: Conf'acronym -> [Content ()]
elementToXMLConf'acronym = schemaTypeToXML "conf-acronym"

data Conf'date = Conf'date
        { conf'date_calendar      :: Maybe Xsd.XsdString
        , conf'date_content'type  :: Maybe Xsd.XsdString
        , conf'date_id            :: Maybe Xsd.ID
        , conf'date_iso'8601'date :: Maybe Xsd.XsdString
        , conf'date_specific'use  :: Maybe Xsd.XsdString
        , conf'date_base          :: Maybe Xsd.AnyURI
        , conf'date_lang          :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Conf'date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "calendar" e pos
        a1 <- optional $ getAttribute "content-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "iso-8601-date" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Conf'date a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Conf'date{} =
        toXMLElement s [ maybe [] (toXMLAttribute "calendar") $ conf'date_calendar x
                       , maybe [] (toXMLAttribute "content-type") $ conf'date_content'type x
                       , maybe [] (toXMLAttribute "id") $ conf'date_id x
                       , maybe [] (toXMLAttribute "iso-8601-date") $ conf'date_iso'8601'date x
                       , maybe [] (toXMLAttribute "specific-use") $ conf'date_specific'use x
                       , maybe [] (toXMLAttribute "base") $ conf'date_base x
                       , maybe [] (toXMLAttribute "lang") $ conf'date_lang x
                       ]
            []

elementConf'date :: XMLParser Conf'date
elementConf'date = parseSchemaType "conf-date"
elementToXMLConf'date :: Conf'date -> [Content ()]
elementToXMLConf'date = schemaTypeToXML "conf-date"

data Conf'loc = Conf'loc
        { conf'loc_content'type :: Maybe Xsd.XsdString
        , conf'loc_id           :: Maybe Xsd.ID
        , conf'loc_specific'use :: Maybe Xsd.XsdString
        , conf'loc_base         :: Maybe Xsd.AnyURI
        , conf'loc_lang         :: Maybe Xsd.XsdString
        , conf'loc_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Conf'loc where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Conf'loc a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Conf'loc{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ conf'loc_content'type x
                       , maybe [] (toXMLAttribute "id") $ conf'loc_id x
                       , maybe [] (toXMLAttribute "specific-use") $ conf'loc_specific'use x
                       , maybe [] (toXMLAttribute "base") $ conf'loc_base x
                       , maybe [] (toXMLAttribute "lang") $ conf'loc_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ conf'loc_choice0 x
            ]

elementConf'loc :: XMLParser Conf'loc
elementConf'loc = parseSchemaType "conf-loc"
elementToXMLConf'loc :: Conf'loc -> [Content ()]
elementToXMLConf'loc = schemaTypeToXML "conf-loc"

data Conf'name = Conf'name
        { conf'name_content'type :: Maybe Xsd.XsdString
        , conf'name_id           :: Maybe Xsd.ID
        , conf'name_specific'use :: Maybe Xsd.XsdString
        , conf'name_base         :: Maybe Xsd.AnyURI
        , conf'name_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Conf'name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Conf'name a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Conf'name{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ conf'name_content'type x
                       , maybe [] (toXMLAttribute "id") $ conf'name_id x
                       , maybe [] (toXMLAttribute "specific-use") $ conf'name_specific'use x
                       , maybe [] (toXMLAttribute "base") $ conf'name_base x
                       , maybe [] (toXMLAttribute "lang") $ conf'name_lang x
                       ]
            []

elementConf'name :: XMLParser Conf'name
elementConf'name = parseSchemaType "conf-name"
elementToXMLConf'name :: Conf'name -> [Content ()]
elementToXMLConf'name = schemaTypeToXML "conf-name"

data Conf'num = Conf'num
        { conf'num_content'type :: Maybe Xsd.XsdString
        , conf'num_id           :: Maybe Xsd.ID
        , conf'num_specific'use :: Maybe Xsd.XsdString
        , conf'num_base         :: Maybe Xsd.AnyURI
        , conf'num_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Conf'num where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Conf'num a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Conf'num{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ conf'num_content'type x
                       , maybe [] (toXMLAttribute "id") $ conf'num_id x
                       , maybe [] (toXMLAttribute "specific-use") $ conf'num_specific'use x
                       , maybe [] (toXMLAttribute "base") $ conf'num_base x
                       , maybe [] (toXMLAttribute "lang") $ conf'num_lang x
                       ]
            []

elementConf'num :: XMLParser Conf'num
elementConf'num = parseSchemaType "conf-num"
elementToXMLConf'num :: Conf'num -> [Content ()]
elementToXMLConf'num = schemaTypeToXML "conf-num"

data Conf'sponsor = Conf'sponsor
        { conf'sponsor_content'type :: Maybe Xsd.XsdString
        , conf'sponsor_id           :: Maybe Xsd.ID
        , conf'sponsor_specific'use :: Maybe Xsd.XsdString
        , conf'sponsor_base         :: Maybe Xsd.AnyURI
        , conf'sponsor_lang         :: Maybe Xsd.XsdString
        , conf'sponsor_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Conf'sponsor where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Conf'sponsor a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Conf'sponsor{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ conf'sponsor_content'type x
                       , maybe [] (toXMLAttribute "id") $ conf'sponsor_id x
                       , maybe [] (toXMLAttribute "specific-use") $ conf'sponsor_specific'use x
                       , maybe [] (toXMLAttribute "base") $ conf'sponsor_base x
                       , maybe [] (toXMLAttribute "lang") $ conf'sponsor_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ conf'sponsor_choice0 x
            ]

elementConf'sponsor :: XMLParser Conf'sponsor
elementConf'sponsor = parseSchemaType "conf-sponsor"
elementToXMLConf'sponsor :: Conf'sponsor -> [Content ()]
elementToXMLConf'sponsor = schemaTypeToXML "conf-sponsor"

data Conf'theme = Conf'theme
        { conf'theme_content'type :: Maybe Xsd.XsdString
        , conf'theme_id           :: Maybe Xsd.ID
        , conf'theme_specific'use :: Maybe Xsd.XsdString
        , conf'theme_base         :: Maybe Xsd.AnyURI
        , conf'theme_lang         :: Maybe Xsd.XsdString
        , conf'theme_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Conf'theme where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Conf'theme a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Conf'theme{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ conf'theme_content'type x
                       , maybe [] (toXMLAttribute "id") $ conf'theme_id x
                       , maybe [] (toXMLAttribute "specific-use") $ conf'theme_specific'use x
                       , maybe [] (toXMLAttribute "base") $ conf'theme_base x
                       , maybe [] (toXMLAttribute "lang") $ conf'theme_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ conf'theme_choice0 x
            ]

elementConf'theme :: XMLParser Conf'theme
elementConf'theme = parseSchemaType "conf-theme"
elementToXMLConf'theme :: Conf'theme -> [Content ()]
elementToXMLConf'theme = schemaTypeToXML "conf-theme"

data Conference = Conference
        { conference_content'type :: Maybe Xsd.XsdString
        , conference_id           :: Maybe Xsd.ID
        , conference_specific'use :: Maybe Xsd.XsdString
        , conference_actuate      :: Maybe Xsd.XsdString
        , conference_href         :: Maybe Xsd.AnyURI
        , conference_role         :: Maybe Xsd.XsdString
        , conference_show         :: Maybe Xsd.XsdString
        , conference_title        :: Maybe Xsd.XsdString
        , conference_type         :: Maybe Xsd.XsdString
        , conference_base         :: Maybe Xsd.AnyURI
        , conference_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Conference where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "actuate" e pos
        a4 <- optional $ getAttribute "href" e pos
        a5 <- optional $ getAttribute "role" e pos
        a6 <- optional $ getAttribute "show" e pos
        a7 <- optional $ getAttribute "title" e pos
        a8 <- optional $ getAttribute "type" e pos
        a9 <- optional $ getAttribute "base" e pos
        a10 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Conference a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    schemaTypeToXML s x@Conference{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ conference_content'type x
                       , maybe [] (toXMLAttribute "id") $ conference_id x
                       , maybe [] (toXMLAttribute "specific-use") $ conference_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ conference_actuate x
                       , maybe [] (toXMLAttribute "href") $ conference_href x
                       , maybe [] (toXMLAttribute "role") $ conference_role x
                       , maybe [] (toXMLAttribute "show") $ conference_show x
                       , maybe [] (toXMLAttribute "title") $ conference_title x
                       , maybe [] (toXMLAttribute "type") $ conference_type x
                       , maybe [] (toXMLAttribute "base") $ conference_base x
                       , maybe [] (toXMLAttribute "lang") $ conference_lang x
                       ]
            []

elementConference :: XMLParser Conference
elementConference = parseSchemaType "conference"
elementToXMLConference :: Conference -> [Content ()]
elementToXMLConference = schemaTypeToXML "conference"

data Contrib = Contrib
        { contrib_contrib'type  :: Maybe Xsd.XsdString
        , contrib_corresp       :: Maybe Xsd.XsdString
        , contrib_deceased      :: Maybe Xsd.XsdString
        , contrib_equal'contrib :: Maybe Xsd.XsdString
        , contrib_id            :: Maybe Xsd.ID
        , contrib_rid           :: Maybe Xsd.IDREFS
        , contrib_specific'use  :: Maybe Xsd.XsdString
        , contrib_actuate       :: Maybe Xsd.XsdString
        , contrib_href          :: Maybe Xsd.AnyURI
        , contrib_role          :: Maybe Xsd.XsdString
        , contrib_show          :: Maybe Xsd.XsdString
        , contrib_title         :: Maybe Xsd.XsdString
        , contrib_type          :: Maybe Xsd.XsdString
        , contrib_base          :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Contrib where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "contrib-type" e pos
        a1 <- optional $ getAttribute "corresp" e pos
        a2 <- optional $ getAttribute "deceased" e pos
        a3 <- optional $ getAttribute "equal-contrib" e pos
        a4 <- optional $ getAttribute "id" e pos
        a5 <- optional $ getAttribute "rid" e pos
        a6 <- optional $ getAttribute "specific-use" e pos
        a7 <- optional $ getAttribute "actuate" e pos
        a8 <- optional $ getAttribute "href" e pos
        a9 <- optional $ getAttribute "role" e pos
        a10 <- optional $ getAttribute "show" e pos
        a11 <- optional $ getAttribute "title" e pos
        a12 <- optional $ getAttribute "type" e pos
        a13 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Contrib a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13)
    schemaTypeToXML s x@Contrib{} =
        toXMLElement s [ maybe [] (toXMLAttribute "contrib-type") $ contrib_contrib'type x
                       , maybe [] (toXMLAttribute "corresp") $ contrib_corresp x
                       , maybe [] (toXMLAttribute "deceased") $ contrib_deceased x
                       , maybe [] (toXMLAttribute "equal-contrib") $ contrib_equal'contrib x
                       , maybe [] (toXMLAttribute "id") $ contrib_id x
                       , maybe [] (toXMLAttribute "rid") $ contrib_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ contrib_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ contrib_actuate x
                       , maybe [] (toXMLAttribute "href") $ contrib_href x
                       , maybe [] (toXMLAttribute "role") $ contrib_role x
                       , maybe [] (toXMLAttribute "show") $ contrib_show x
                       , maybe [] (toXMLAttribute "title") $ contrib_title x
                       , maybe [] (toXMLAttribute "type") $ contrib_type x
                       , maybe [] (toXMLAttribute "base") $ contrib_base x
                       ]
            []

elementContrib :: XMLParser Contrib
elementContrib = parseSchemaType "contrib"
elementToXMLContrib :: Contrib -> [Content ()]
elementToXMLContrib = schemaTypeToXML "contrib"

data Contrib'group = Contrib'group
        { contrib'group_content'type :: Maybe Xsd.XsdString
        , contrib'group_id           :: Maybe Xsd.ID
        , contrib'group_specific'use :: Maybe Xsd.XsdString
        , contrib'group_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Contrib'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Contrib'group a0 a1 a2 a3)
    schemaTypeToXML s x@Contrib'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ contrib'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ contrib'group_id x
                       , maybe [] (toXMLAttribute "specific-use") $ contrib'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ contrib'group_base x
                       ]
            []

elementContrib'group :: XMLParser Contrib'group
elementContrib'group = parseSchemaType "contrib-group"
elementToXMLContrib'group :: Contrib'group -> [Content ()]
elementToXMLContrib'group = schemaTypeToXML "contrib-group"

data Contrib'id = Contrib'id
        { contrib'id_authenticated   :: Maybe Xsd.XsdString
        , contrib'id_content'type    :: Maybe Xsd.XsdString
        , contrib'id_contrib'id'type :: Maybe Xsd.XsdString
        , contrib'id_id              :: Maybe Xsd.ID
        , contrib'id_specific'use    :: Maybe Xsd.XsdString
        , contrib'id_base            :: Maybe Xsd.AnyURI
        , contrib'id_lang            :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Contrib'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "authenticated" e pos
        a1 <- optional $ getAttribute "content-type" e pos
        a2 <- optional $ getAttribute "contrib-id-type" e pos
        a3 <- optional $ getAttribute "id" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Contrib'id a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Contrib'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "authenticated") $ contrib'id_authenticated x
                       , maybe [] (toXMLAttribute "content-type") $ contrib'id_content'type x
                       , maybe [] (toXMLAttribute "contrib-id-type") $ contrib'id_contrib'id'type x
                       , maybe [] (toXMLAttribute "id") $ contrib'id_id x
                       , maybe [] (toXMLAttribute "specific-use") $ contrib'id_specific'use x
                       , maybe [] (toXMLAttribute "base") $ contrib'id_base x
                       , maybe [] (toXMLAttribute "lang") $ contrib'id_lang x
                       ]
            []

elementContrib'id :: XMLParser Contrib'id
elementContrib'id = parseSchemaType "contrib-id"
elementToXMLContrib'id :: Contrib'id -> [Content ()]
elementToXMLContrib'id = schemaTypeToXML "contrib-id"

data Copyright'holder = Copyright'holder
        { copyright'holder_content'type :: Maybe Xsd.XsdString
        , copyright'holder_id           :: Maybe Xsd.ID
        , copyright'holder_specific'use :: Maybe Xsd.XsdString
        , copyright'holder_base         :: Maybe Xsd.AnyURI
        , copyright'holder_lang         :: Maybe Xsd.XsdString
        , copyright'holder_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Copyright'holder where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Copyright'holder a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Copyright'holder{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ copyright'holder_content'type x
                       , maybe [] (toXMLAttribute "id") $ copyright'holder_id x
                       , maybe [] (toXMLAttribute "specific-use") $ copyright'holder_specific'use x
                       , maybe [] (toXMLAttribute "base") $ copyright'holder_base x
                       , maybe [] (toXMLAttribute "lang") $ copyright'holder_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ copyright'holder_choice0 x
            ]

elementCopyright'holder :: XMLParser Copyright'holder
elementCopyright'holder = parseSchemaType "copyright-holder"
elementToXMLCopyright'holder :: Copyright'holder -> [Content ()]
elementToXMLCopyright'holder = schemaTypeToXML "copyright-holder"

data Copyright'statement = Copyright'statement
        { copyright'statement_content'type :: Maybe Xsd.XsdString
        , copyright'statement_id           :: Maybe Xsd.ID
        , copyright'statement_specific'use :: Maybe Xsd.XsdString
        , copyright'statement_base         :: Maybe Xsd.AnyURI
        , copyright'statement_lang         :: Maybe Xsd.XsdString
        , copyright'statement_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Copyright'statement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Copyright'statement a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Copyright'statement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ copyright'statement_content'type x
                       , maybe [] (toXMLAttribute "id") $ copyright'statement_id x
                       , maybe [] (toXMLAttribute "specific-use") $ copyright'statement_specific'use x
                       , maybe [] (toXMLAttribute "base") $ copyright'statement_base x
                       , maybe [] (toXMLAttribute "lang") $ copyright'statement_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ copyright'statement_choice0 x
            ]

elementCopyright'statement :: XMLParser Copyright'statement
elementCopyright'statement = parseSchemaType "copyright-statement"
elementToXMLCopyright'statement :: Copyright'statement -> [Content ()]
elementToXMLCopyright'statement = schemaTypeToXML "copyright-statement"

data Copyright'year = Copyright'year
        { copyright'year_content'type :: Maybe Xsd.XsdString
        , copyright'year_id           :: Maybe Xsd.ID
        , copyright'year_specific'use :: Maybe Xsd.XsdString
        , copyright'year_base         :: Maybe Xsd.AnyURI
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Copyright'year where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Copyright'year a0 a1 a2 a3)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Copyright'year{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ copyright'year_content'type x
                       , maybe [] (toXMLAttribute "id") $ copyright'year_id x
                       , maybe [] (toXMLAttribute "specific-use") $ copyright'year_specific'use x
                       , maybe [] (toXMLAttribute "base") $ copyright'year_base x
                       ]
            [
            ]

elementCopyright'year :: XMLParser Copyright'year
elementCopyright'year = parseSchemaType "copyright-year"
elementToXMLCopyright'year :: Copyright'year -> [Content ()]
elementToXMLCopyright'year = schemaTypeToXML "copyright-year"

data Corresp = Corresp
        { corresp_content'type :: Maybe Xsd.XsdString
        , corresp_id           :: Maybe Xsd.ID
        , corresp_specific'use :: Maybe Xsd.XsdString
        , corresp_base         :: Maybe Xsd.AnyURI
        , corresp_lang         :: Maybe Xsd.XsdString
        , corresp_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Corresp where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Corresp a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Corresp{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ corresp_content'type x
                       , maybe [] (toXMLAttribute "id") $ corresp_id x
                       , maybe [] (toXMLAttribute "specific-use") $ corresp_specific'use x
                       , maybe [] (toXMLAttribute "base") $ corresp_base x
                       , maybe [] (toXMLAttribute "lang") $ corresp_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ corresp_choice0 x
            ]

elementCorresp :: XMLParser Corresp
elementCorresp = parseSchemaType "corresp"
elementToXMLCorresp :: Corresp -> [Content ()]
elementToXMLCorresp = schemaTypeToXML "corresp"

data Count = Count
        { count_count      :: Xsd.NMTOKEN
        , count_count'type :: Xsd.XsdString
        , count_id         :: Maybe Xsd.ID
        , count_base       :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Count where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "count" e pos
        a1 <- getAttribute "count-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Count a0 a1 a2 a3)
    schemaTypeToXML s x@Count{} =
        toXMLElement s [ toXMLAttribute "count" $ count_count x
                       , toXMLAttribute "count-type" $ count_count'type x
                       , maybe [] (toXMLAttribute "id") $ count_id x
                       , maybe [] (toXMLAttribute "base") $ count_base x
                       ]
            []

elementCount :: XMLParser Count
elementCount = parseSchemaType "count"
elementToXMLCount :: Count -> [Content ()]
elementToXMLCount = schemaTypeToXML "count"

data Country = Country
        { country_content'type :: Maybe Xsd.XsdString
        , country_country      :: Maybe Xsd.XsdString
        , country_id           :: Maybe Xsd.ID
        , country_specific'use :: Maybe Xsd.XsdString
        , country_base         :: Maybe Xsd.AnyURI
        , country_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Country where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "country" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Country a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Country{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ country_content'type x
                       , maybe [] (toXMLAttribute "country") $ country_country x
                       , maybe [] (toXMLAttribute "id") $ country_id x
                       , maybe [] (toXMLAttribute "specific-use") $ country_specific'use x
                       , maybe [] (toXMLAttribute "base") $ country_base x
                       , maybe [] (toXMLAttribute "lang") $ country_lang x
                       ]
            []

elementCountry :: XMLParser Country
elementCountry = parseSchemaType "country"
elementToXMLCountry :: Country -> [Content ()]
elementToXMLCountry = schemaTypeToXML "country"

data Counts = Counts
        { counts_id   :: Maybe Xsd.ID
        , counts_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Counts where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Counts a0 a1)
    schemaTypeToXML s x@Counts{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ counts_id x
                       , maybe [] (toXMLAttribute "base") $ counts_base x
                       ]
            []

elementCounts :: XMLParser Counts
elementCounts = parseSchemaType "counts"
elementToXMLCounts :: Counts -> [Content ()]
elementToXMLCounts = schemaTypeToXML "counts"

data Custom'meta = Custom'meta
        { custom'meta_id           :: Maybe Xsd.ID
        , custom'meta_specific'use :: Maybe Xsd.XsdString
        , custom'meta_actuate      :: Maybe Xsd.XsdString
        , custom'meta_href         :: Maybe Xsd.AnyURI
        , custom'meta_role         :: Maybe Xsd.XsdString
        , custom'meta_show         :: Maybe Xsd.XsdString
        , custom'meta_title        :: Maybe Xsd.XsdString
        , custom'meta_type         :: Maybe Xsd.XsdString
        , custom'meta_base         :: Maybe Xsd.AnyURI
        , custom'meta_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Custom'meta where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "actuate" e pos
        a3 <- optional $ getAttribute "href" e pos
        a4 <- optional $ getAttribute "role" e pos
        a5 <- optional $ getAttribute "show" e pos
        a6 <- optional $ getAttribute "title" e pos
        a7 <- optional $ getAttribute "type" e pos
        a8 <- optional $ getAttribute "base" e pos
        a9 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Custom'meta a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
    schemaTypeToXML s x@Custom'meta{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ custom'meta_id x
                       , maybe [] (toXMLAttribute "specific-use") $ custom'meta_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ custom'meta_actuate x
                       , maybe [] (toXMLAttribute "href") $ custom'meta_href x
                       , maybe [] (toXMLAttribute "role") $ custom'meta_role x
                       , maybe [] (toXMLAttribute "show") $ custom'meta_show x
                       , maybe [] (toXMLAttribute "title") $ custom'meta_title x
                       , maybe [] (toXMLAttribute "type") $ custom'meta_type x
                       , maybe [] (toXMLAttribute "base") $ custom'meta_base x
                       , maybe [] (toXMLAttribute "lang") $ custom'meta_lang x
                       ]
            []

elementCustom'meta :: XMLParser Custom'meta
elementCustom'meta = parseSchemaType "custom-meta"
elementToXMLCustom'meta :: Custom'meta -> [Content ()]
elementToXMLCustom'meta = schemaTypeToXML "custom-meta"

data Custom'meta'group = Custom'meta'group
        { custom'meta'group_id   :: Maybe Xsd.ID
        , custom'meta'group_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Custom'meta'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Custom'meta'group a0 a1)
    schemaTypeToXML s x@Custom'meta'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ custom'meta'group_id x
                       , maybe [] (toXMLAttribute "base") $ custom'meta'group_base x
                       ]
            []

elementCustom'meta'group :: XMLParser Custom'meta'group
elementCustom'meta'group = parseSchemaType "custom-meta-group"
elementToXMLCustom'meta'group :: Custom'meta'group -> [Content ()]
elementToXMLCustom'meta'group = schemaTypeToXML "custom-meta-group"

data Data'title = Data'title
        { data'title_content'type :: Maybe Xsd.XsdString
        , data'title_id           :: Maybe Xsd.ID
        , data'title_specific'use :: Maybe Xsd.XsdString
        , data'title_base         :: Maybe Xsd.AnyURI
        , data'title_lang         :: Maybe Xsd.XsdString
        , data'title_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Data'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Data'title a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Data'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ data'title_content'type x
                       , maybe [] (toXMLAttribute "id") $ data'title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ data'title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ data'title_base x
                       , maybe [] (toXMLAttribute "lang") $ data'title_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ data'title_choice0 x
            ]

elementData'title :: XMLParser Data'title
elementData'title = parseSchemaType "data-title"
elementToXMLData'title :: Data'title -> [Content ()]
elementToXMLData'title = schemaTypeToXML "data-title"

data Date = Date
        { date_calendar           :: Maybe Xsd.XsdString
        , date_date'type          :: Maybe Xsd.XsdString
        , date_id                 :: Maybe Xsd.ID
        , date_iso'8601'date      :: Maybe Xsd.XsdString
        , date_publication'format :: Maybe Xsd.XsdString
        , date_specific'use       :: Maybe Xsd.XsdString
        , date_base               :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "calendar" e pos
        a1 <- optional $ getAttribute "date-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "iso-8601-date" e pos
        a4 <- optional $ getAttribute "publication-format" e pos
        a5 <- optional $ getAttribute "specific-use" e pos
        a6 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Date a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Date{} =
        toXMLElement s [ maybe [] (toXMLAttribute "calendar") $ date_calendar x
                       , maybe [] (toXMLAttribute "date-type") $ date_date'type x
                       , maybe [] (toXMLAttribute "id") $ date_id x
                       , maybe [] (toXMLAttribute "iso-8601-date") $ date_iso'8601'date x
                       , maybe [] (toXMLAttribute "publication-format") $ date_publication'format x
                       , maybe [] (toXMLAttribute "specific-use") $ date_specific'use x
                       , maybe [] (toXMLAttribute "base") $ date_base x
                       ]
            []

elementDate :: XMLParser Date
elementDate = parseSchemaType "date"
elementToXMLDate :: Date -> [Content ()]
elementToXMLDate = schemaTypeToXML "date"

data Date'in'citation = Date'in'citation
        { date'in'citation_calendar      :: Maybe Xsd.XsdString
        , date'in'citation_content'type  :: Maybe Xsd.XsdString
        , date'in'citation_id            :: Maybe Xsd.ID
        , date'in'citation_iso'8601'date :: Maybe Xsd.XsdString
        , date'in'citation_specific'use  :: Maybe Xsd.XsdString
        , date'in'citation_base          :: Maybe Xsd.AnyURI
        , date'in'citation_lang          :: Maybe Xsd.XsdString
        , date'in'citation_choice0       :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Date'in'citation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "calendar" e pos
        a1 <- optional $ getAttribute "content-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "iso-8601-date" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Date'in'citation a0 a1 a2 a3 a4 a5 a6)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Date'in'citation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "calendar") $ date'in'citation_calendar x
                       , maybe [] (toXMLAttribute "content-type") $ date'in'citation_content'type x
                       , maybe [] (toXMLAttribute "id") $ date'in'citation_id x
                       , maybe [] (toXMLAttribute "iso-8601-date") $ date'in'citation_iso'8601'date x
                       , maybe [] (toXMLAttribute "specific-use") $ date'in'citation_specific'use x
                       , maybe [] (toXMLAttribute "base") $ date'in'citation_base x
                       , maybe [] (toXMLAttribute "lang") $ date'in'citation_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ date'in'citation_choice0 x
            ]

elementDate'in'citation :: XMLParser Date'in'citation
elementDate'in'citation = parseSchemaType "date-in-citation"
elementToXMLDate'in'citation :: Date'in'citation -> [Content ()]
elementToXMLDate'in'citation = schemaTypeToXML "date-in-citation"

data Day = Day
        { day_content'type :: Maybe Xsd.XsdString
        , day_id           :: Maybe Xsd.ID
        , day_specific'use :: Maybe Xsd.XsdString
        , day_base         :: Maybe Xsd.AnyURI
        , day_lang         :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Day where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Day a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Day{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ day_content'type x
                       , maybe [] (toXMLAttribute "id") $ day_id x
                       , maybe [] (toXMLAttribute "specific-use") $ day_specific'use x
                       , maybe [] (toXMLAttribute "base") $ day_base x
                       , maybe [] (toXMLAttribute "lang") $ day_lang x
                       ]
            [
            ]

elementDay :: XMLParser Day
elementDay = parseSchemaType "day"
elementToXMLDay :: Day -> [Content ()]
elementToXMLDay = schemaTypeToXML "day"

data Def = Def
        { def_id           :: Maybe Xsd.ID
        , def_rid          :: Maybe Xsd.IDREFS
        , def_specific'use :: Maybe Xsd.XsdString
        , def_base         :: Maybe Xsd.AnyURI
        , def_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Def where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "rid" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Def a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Def{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ def_id x
                       , maybe [] (toXMLAttribute "rid") $ def_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ def_specific'use x
                       , maybe [] (toXMLAttribute "base") $ def_base x
                       , maybe [] (toXMLAttribute "lang") $ def_lang x
                       ]
            []

elementDef :: XMLParser Def
elementDef = parseSchemaType "def"
elementToXMLDef :: Def -> [Content ()]
elementToXMLDef = schemaTypeToXML "def"

data Def'head = Def'head
        { def'head_id      :: Maybe Xsd.ID
        , def'head_base    :: Maybe Xsd.AnyURI
        , def'head_choice0 :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Def'head where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Def'head a0 a1)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Def'head{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ def'head_id x
                       , maybe [] (toXMLAttribute "base") $ def'head_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ def'head_choice0 x
            ]

elementDef'head :: XMLParser Def'head
elementDef'head = parseSchemaType "def-head"
elementToXMLDef'head :: Def'head -> [Content ()]
elementToXMLDef'head = schemaTypeToXML "def-head"

data Def'item = Def'item
        { def'item_id           :: Maybe Xsd.ID
        , def'item_specific'use :: Maybe Xsd.XsdString
        , def'item_base         :: Maybe Xsd.AnyURI
        , def'item_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Def'item where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Def'item a0 a1 a2 a3)
    schemaTypeToXML s x@Def'item{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ def'item_id x
                       , maybe [] (toXMLAttribute "specific-use") $ def'item_specific'use x
                       , maybe [] (toXMLAttribute "base") $ def'item_base x
                       , maybe [] (toXMLAttribute "lang") $ def'item_lang x
                       ]
            []

elementDef'item :: XMLParser Def'item
elementDef'item = parseSchemaType "def-item"
elementToXMLDef'item :: Def'item -> [Content ()]
elementToXMLDef'item = schemaTypeToXML "def-item"

data Def'list = Def'list
        { def'list_continued'from :: Maybe Xsd.IDREF
        , def'list_id             :: Maybe Xsd.ID
        , def'list_list'content   :: Maybe Xsd.XsdString
        , def'list_list'type      :: Maybe Xsd.XsdString
        , def'list_prefix'word    :: Maybe Xsd.XsdString
        , def'list_specific'use   :: Maybe Xsd.XsdString
        , def'list_base           :: Maybe Xsd.AnyURI
        , def'list_lang           :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Def'list where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "continued-from" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "list-content" e pos
        a3 <- optional $ getAttribute "list-type" e pos
        a4 <- optional $ getAttribute "prefix-word" e pos
        a5 <- optional $ getAttribute "specific-use" e pos
        a6 <- optional $ getAttribute "base" e pos
        a7 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Def'list a0 a1 a2 a3 a4 a5 a6 a7)
    schemaTypeToXML s x@Def'list{} =
        toXMLElement s [ maybe [] (toXMLAttribute "continued-from") $ def'list_continued'from x
                       , maybe [] (toXMLAttribute "id") $ def'list_id x
                       , maybe [] (toXMLAttribute "list-content") $ def'list_list'content x
                       , maybe [] (toXMLAttribute "list-type") $ def'list_list'type x
                       , maybe [] (toXMLAttribute "prefix-word") $ def'list_prefix'word x
                       , maybe [] (toXMLAttribute "specific-use") $ def'list_specific'use x
                       , maybe [] (toXMLAttribute "base") $ def'list_base x
                       , maybe [] (toXMLAttribute "lang") $ def'list_lang x
                       ]
            []

elementDef'list :: XMLParser Def'list
elementDef'list = parseSchemaType "def-list"
elementToXMLDef'list :: Def'list -> [Content ()]
elementToXMLDef'list = schemaTypeToXML "def-list"

data Degrees = Degrees
        { degrees_content'type :: Maybe Xsd.XsdString
        , degrees_id           :: Maybe Xsd.ID
        , degrees_specific'use :: Maybe Xsd.XsdString
        , degrees_base         :: Maybe Xsd.AnyURI
        , degrees_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Degrees where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Degrees a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Degrees{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ degrees_content'type x
                       , maybe [] (toXMLAttribute "id") $ degrees_id x
                       , maybe [] (toXMLAttribute "specific-use") $ degrees_specific'use x
                       , maybe [] (toXMLAttribute "base") $ degrees_base x
                       , maybe [] (toXMLAttribute "lang") $ degrees_lang x
                       ]
            []

elementDegrees :: XMLParser Degrees
elementDegrees = parseSchemaType "degrees"
elementToXMLDegrees :: Degrees -> [Content ()]
elementToXMLDegrees = schemaTypeToXML "degrees"

data Disp'formula = Disp'formula
        { disp'formula_content'type :: Maybe Xsd.XsdString
        , disp'formula_id           :: Maybe Xsd.ID
        , disp'formula_specific'use :: Maybe Xsd.XsdString
        , disp'formula_base         :: Maybe Xsd.AnyURI
        , disp'formula_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Disp'formula where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Disp'formula a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Disp'formula{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ disp'formula_content'type x
                       , maybe [] (toXMLAttribute "id") $ disp'formula_id x
                       , maybe [] (toXMLAttribute "specific-use") $ disp'formula_specific'use x
                       , maybe [] (toXMLAttribute "base") $ disp'formula_base x
                       , maybe [] (toXMLAttribute "lang") $ disp'formula_lang x
                       ]
            []

elementDisp'formula :: XMLParser Disp'formula
elementDisp'formula = parseSchemaType "disp-formula"
elementToXMLDisp'formula :: Disp'formula -> [Content ()]
elementToXMLDisp'formula = schemaTypeToXML "disp-formula"

data Disp'formula'group = Disp'formula'group
        { disp'formula'group_content'type :: Maybe Xsd.XsdString
        , disp'formula'group_id           :: Maybe Xsd.ID
        , disp'formula'group_specific'use :: Maybe Xsd.XsdString
        , disp'formula'group_base         :: Maybe Xsd.AnyURI
        , disp'formula'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Disp'formula'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Disp'formula'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Disp'formula'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ disp'formula'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ disp'formula'group_id x
                       , maybe [] (toXMLAttribute "specific-use") $ disp'formula'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ disp'formula'group_base x
                       , maybe [] (toXMLAttribute "lang") $ disp'formula'group_lang x
                       ]
            []

elementDisp'formula'group :: XMLParser Disp'formula'group
elementDisp'formula'group = parseSchemaType "disp-formula-group"
elementToXMLDisp'formula'group :: Disp'formula'group -> [Content ()]
elementToXMLDisp'formula'group = schemaTypeToXML "disp-formula-group"

data Disp'quote = Disp'quote
        { disp'quote_content'type :: Maybe Xsd.XsdString
        , disp'quote_id           :: Maybe Xsd.ID
        , disp'quote_specific'use :: Maybe Xsd.XsdString
        , disp'quote_base         :: Maybe Xsd.AnyURI
        , disp'quote_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Disp'quote where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Disp'quote a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Disp'quote{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ disp'quote_content'type x
                       , maybe [] (toXMLAttribute "id") $ disp'quote_id x
                       , maybe [] (toXMLAttribute "specific-use") $ disp'quote_specific'use x
                       , maybe [] (toXMLAttribute "base") $ disp'quote_base x
                       , maybe [] (toXMLAttribute "lang") $ disp'quote_lang x
                       ]
            []

elementDisp'quote :: XMLParser Disp'quote
elementDisp'quote = parseSchemaType "disp-quote"
elementToXMLDisp'quote :: Disp'quote -> [Content ()]
elementToXMLDisp'quote = schemaTypeToXML "disp-quote"

data Edition = Edition
        { edition_content'type :: Maybe Xsd.XsdString
        , edition_designator   :: Maybe Xsd.XsdString
        , edition_id           :: Maybe Xsd.ID
        , edition_specific'use :: Maybe Xsd.XsdString
        , edition_base         :: Maybe Xsd.AnyURI
        , edition_lang         :: Maybe Xsd.XsdString
        , edition_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Edition where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "designator" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Edition a0 a1 a2 a3 a4 a5)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Edition{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ edition_content'type x
                       , maybe [] (toXMLAttribute "designator") $ edition_designator x
                       , maybe [] (toXMLAttribute "id") $ edition_id x
                       , maybe [] (toXMLAttribute "specific-use") $ edition_specific'use x
                       , maybe [] (toXMLAttribute "base") $ edition_base x
                       , maybe [] (toXMLAttribute "lang") $ edition_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ edition_choice0 x
            ]

elementEdition :: XMLParser Edition
elementEdition = parseSchemaType "edition"
elementToXMLEdition :: Edition -> [Content ()]
elementToXMLEdition = schemaTypeToXML "edition"

data Element'citation = Element'citation
        { element'citation_id                 :: Maybe Xsd.ID
        , element'citation_publication'format :: Maybe Xsd.XsdString
        , element'citation_publication'type   :: Maybe Xsd.XsdString
        , element'citation_publisher'type     :: Maybe Xsd.XsdString
        , element'citation_specific'use       :: Maybe Xsd.XsdString
        , element'citation_actuate            :: Maybe Xsd.XsdString
        , element'citation_href               :: Maybe Xsd.AnyURI
        , element'citation_role               :: Maybe Xsd.XsdString
        , element'citation_show               :: Maybe Xsd.XsdString
        , element'citation_title              :: Maybe Xsd.XsdString
        , element'citation_type               :: Maybe Xsd.XsdString
        , element'citation_base               :: Maybe Xsd.AnyURI
        , element'citation_lang               :: Maybe Xsd.XsdString
        , element'citation_choice0            :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Element'citation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "publication-format" e pos
        a2 <- optional $ getAttribute "publication-type" e pos
        a3 <- optional $ getAttribute "publisher-type" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "actuate" e pos
        a6 <- optional $ getAttribute "href" e pos
        a7 <- optional $ getAttribute "role" e pos
        a8 <- optional $ getAttribute "show" e pos
        a9 <- optional $ getAttribute "title" e pos
        a10 <- optional $ getAttribute "type" e pos
        a11 <- optional $ getAttribute "base" e pos
        a12 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Element'citation a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            `apply` many1 (oneOf' [ -- ("", fmap OneOf1 ())
                                  ])
    schemaTypeToXML s x@Element'citation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ element'citation_id x
                       , maybe [] (toXMLAttribute "publication-format") $ element'citation_publication'format x
                       , maybe [] (toXMLAttribute "publication-type") $ element'citation_publication'type x
                       , maybe [] (toXMLAttribute "publisher-type") $ element'citation_publisher'type x
                       , maybe [] (toXMLAttribute "specific-use") $ element'citation_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ element'citation_actuate x
                       , maybe [] (toXMLAttribute "href") $ element'citation_href x
                       , maybe [] (toXMLAttribute "role") $ element'citation_role x
                       , maybe [] (toXMLAttribute "show") $ element'citation_show x
                       , maybe [] (toXMLAttribute "title") $ element'citation_title x
                       , maybe [] (toXMLAttribute "type") $ element'citation_type x
                       , maybe [] (toXMLAttribute "base") $ element'citation_base x
                       , maybe [] (toXMLAttribute "lang") $ element'citation_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ element'citation_choice0 x
            ]

elementElement'citation :: XMLParser Element'citation
elementElement'citation = parseSchemaType "element-citation"
elementToXMLElement'citation :: Element'citation -> [Content ()]
elementToXMLElement'citation = schemaTypeToXML "element-citation"

data Elocation'id = Elocation'id
        { elocation'id_content'type :: Maybe Xsd.XsdString
        , elocation'id_id           :: Maybe Xsd.ID
        , elocation'id_seq          :: Maybe Xsd.XsdString
        , elocation'id_specific'use :: Maybe Xsd.XsdString
        , elocation'id_base         :: Maybe Xsd.AnyURI
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Elocation'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "seq" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Elocation'id a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Elocation'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ elocation'id_content'type x
                       , maybe [] (toXMLAttribute "id") $ elocation'id_id x
                       , maybe [] (toXMLAttribute "seq") $ elocation'id_seq x
                       , maybe [] (toXMLAttribute "specific-use") $ elocation'id_specific'use x
                       , maybe [] (toXMLAttribute "base") $ elocation'id_base x
                       ]
            [
            ]

elementElocation'id :: XMLParser Elocation'id
elementElocation'id = parseSchemaType "elocation-id"
elementToXMLElocation'id :: Elocation'id -> [Content ()]
elementToXMLElocation'id = schemaTypeToXML "elocation-id"

data Email = Email
        { email_content'type :: Maybe Xsd.XsdString
        , email_id           :: Maybe Xsd.ID
        , email_specific'use :: Maybe Xsd.XsdString
        , email_actuate      :: Maybe Xsd.XsdString
        , email_href         :: Maybe Xsd.AnyURI
        , email_role         :: Maybe Xsd.XsdString
        , email_show         :: Maybe Xsd.XsdString
        , email_title        :: Maybe Xsd.XsdString
        , email_type         :: Maybe Xsd.XsdString
        , email_base         :: Maybe Xsd.AnyURI
        , email_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Email where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "actuate" e pos
        a4 <- optional $ getAttribute "href" e pos
        a5 <- optional $ getAttribute "role" e pos
        a6 <- optional $ getAttribute "show" e pos
        a7 <- optional $ getAttribute "title" e pos
        a8 <- optional $ getAttribute "type" e pos
        a9 <- optional $ getAttribute "base" e pos
        a10 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Email a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    schemaTypeToXML s x@Email{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ email_content'type x
                       , maybe [] (toXMLAttribute "id") $ email_id x
                       , maybe [] (toXMLAttribute "specific-use") $ email_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ email_actuate x
                       , maybe [] (toXMLAttribute "href") $ email_href x
                       , maybe [] (toXMLAttribute "role") $ email_role x
                       , maybe [] (toXMLAttribute "show") $ email_show x
                       , maybe [] (toXMLAttribute "title") $ email_title x
                       , maybe [] (toXMLAttribute "type") $ email_type x
                       , maybe [] (toXMLAttribute "base") $ email_base x
                       , maybe [] (toXMLAttribute "lang") $ email_lang x
                       ]
            []

elementEmail :: XMLParser Email
elementEmail = parseSchemaType "email"
elementToXMLEmail :: Email -> [Content ()]
elementToXMLEmail = schemaTypeToXML "email"

data Equation'count = Equation'count
        { equation'count_count :: Xsd.NMTOKEN
        , equation'count_id    :: Maybe Xsd.ID
        , equation'count_base  :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Equation'count where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "count" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Equation'count a0 a1 a2)
    schemaTypeToXML s x@Equation'count{} =
        toXMLElement s [ toXMLAttribute "count" $ equation'count_count x
                       , maybe [] (toXMLAttribute "id") $ equation'count_id x
                       , maybe [] (toXMLAttribute "base") $ equation'count_base x
                       ]
            []

elementEquation'count :: XMLParser Equation'count
elementEquation'count = parseSchemaType "equation-count"
elementToXMLEquation'count :: Equation'count -> [Content ()]
elementToXMLEquation'count = schemaTypeToXML "equation-count"

data Era = Era
        { era_content'type :: Maybe Xsd.XsdString
        , era_id           :: Maybe Xsd.ID
        , era_specific'use :: Maybe Xsd.XsdString
        , era_base         :: Maybe Xsd.AnyURI
        , era_lang         :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Era where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Era a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Era{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ era_content'type x
                       , maybe [] (toXMLAttribute "id") $ era_id x
                       , maybe [] (toXMLAttribute "specific-use") $ era_specific'use x
                       , maybe [] (toXMLAttribute "base") $ era_base x
                       , maybe [] (toXMLAttribute "lang") $ era_lang x
                       ]
            [
            ]

elementEra :: XMLParser Era
elementEra = parseSchemaType "era"
elementToXMLEra :: Era -> [Content ()]
elementToXMLEra = schemaTypeToXML "era"

data Etal = Etal
        { etal_id           :: Maybe Xsd.ID
        , etal_specific'use :: Maybe Xsd.XsdString
        , etal_base         :: Maybe Xsd.AnyURI
        , etal_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Etal where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Etal a0 a1 a2 a3)
    schemaTypeToXML s x@Etal{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ etal_id x
                       , maybe [] (toXMLAttribute "specific-use") $ etal_specific'use x
                       , maybe [] (toXMLAttribute "base") $ etal_base x
                       , maybe [] (toXMLAttribute "lang") $ etal_lang x
                       ]
            []

elementEtal :: XMLParser Etal
elementEtal = parseSchemaType "etal"
elementToXMLEtal :: Etal -> [Content ()]
elementToXMLEtal = schemaTypeToXML "etal"

data Ext'link = Ext'link
        { ext'link_assigning'authority :: Maybe Xsd.XsdString
        , ext'link_ext'link'type       :: Maybe Xsd.XsdString
        , ext'link_id                  :: Maybe Xsd.ID
        , ext'link_specific'use        :: Maybe Xsd.XsdString
        , ext'link_actuate             :: Maybe Xsd.XsdString
        , ext'link_href                :: Maybe Xsd.AnyURI
        , ext'link_role                :: Maybe Xsd.XsdString
        , ext'link_show                :: Maybe Xsd.XsdString
        , ext'link_title               :: Maybe Xsd.XsdString
        , ext'link_type                :: Maybe Xsd.XsdString
        , ext'link_base                :: Maybe Xsd.AnyURI
        , ext'link_lang                :: Maybe Xsd.XsdString
        , ext'link_choice0             :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Ext'link where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "assigning-authority" e pos
        a1 <- optional $ getAttribute "ext-link-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "actuate" e pos
        a5 <- optional $ getAttribute "href" e pos
        a6 <- optional $ getAttribute "role" e pos
        a7 <- optional $ getAttribute "show" e pos
        a8 <- optional $ getAttribute "title" e pos
        a9 <- optional $ getAttribute "type" e pos
        a10 <- optional $ getAttribute "base" e pos
        a11 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Ext'link a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Ext'link{} =
        toXMLElement s [ maybe [] (toXMLAttribute "assigning-authority") $ ext'link_assigning'authority x
                       , maybe [] (toXMLAttribute "ext-link-type") $ ext'link_ext'link'type x
                       , maybe [] (toXMLAttribute "id") $ ext'link_id x
                       , maybe [] (toXMLAttribute "specific-use") $ ext'link_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ ext'link_actuate x
                       , maybe [] (toXMLAttribute "href") $ ext'link_href x
                       , maybe [] (toXMLAttribute "role") $ ext'link_role x
                       , maybe [] (toXMLAttribute "show") $ ext'link_show x
                       , maybe [] (toXMLAttribute "title") $ ext'link_title x
                       , maybe [] (toXMLAttribute "type") $ ext'link_type x
                       , maybe [] (toXMLAttribute "base") $ ext'link_base x
                       , maybe [] (toXMLAttribute "lang") $ ext'link_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ ext'link_choice0 x
            ]

elementExt'link :: XMLParser Ext'link
elementExt'link = parseSchemaType "ext-link"
elementToXMLExt'link :: Ext'link -> [Content ()]
elementToXMLExt'link = schemaTypeToXML "ext-link"

data Fax = Fax
        { fax_content'type :: Maybe Xsd.XsdString
        , fax_id           :: Maybe Xsd.ID
        , fax_specific'use :: Maybe Xsd.XsdString
        , fax_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Fax where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Fax a0 a1 a2 a3)
    schemaTypeToXML s x@Fax{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ fax_content'type x
                       , maybe [] (toXMLAttribute "id") $ fax_id x
                       , maybe [] (toXMLAttribute "specific-use") $ fax_specific'use x
                       , maybe [] (toXMLAttribute "base") $ fax_base x
                       ]
            []

elementFax :: XMLParser Fax
elementFax = parseSchemaType "fax"
elementToXMLFax :: Fax -> [Content ()]
elementToXMLFax = schemaTypeToXML "fax"

data Fig = Fig
        { fig_fig'type     :: Maybe Xsd.XsdString
        , fig_id           :: Maybe Xsd.ID
        , fig_orientation  :: Maybe Xsd.XsdString
        , fig_position     :: Maybe Xsd.XsdString
        , fig_specific'use :: Maybe Xsd.XsdString
        , fig_base         :: Maybe Xsd.AnyURI
        , fig_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Fig where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "fig-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "orientation" e pos
        a3 <- optional $ getAttribute "position" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Fig a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Fig{} =
        toXMLElement s [ maybe [] (toXMLAttribute "fig-type") $ fig_fig'type x
                       , maybe [] (toXMLAttribute "id") $ fig_id x
                       , maybe [] (toXMLAttribute "orientation") $ fig_orientation x
                       , maybe [] (toXMLAttribute "position") $ fig_position x
                       , maybe [] (toXMLAttribute "specific-use") $ fig_specific'use x
                       , maybe [] (toXMLAttribute "base") $ fig_base x
                       , maybe [] (toXMLAttribute "lang") $ fig_lang x
                       ]
            []

elementFig :: XMLParser Fig
elementFig = parseSchemaType "fig"
elementToXMLFig :: Fig -> [Content ()]
elementToXMLFig = schemaTypeToXML "fig"

data Fig'count = Fig'count
        { fig'count_count :: Xsd.NMTOKEN
        , fig'count_id    :: Maybe Xsd.ID
        , fig'count_base  :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Fig'count where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "count" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Fig'count a0 a1 a2)
    schemaTypeToXML s x@Fig'count{} =
        toXMLElement s [ toXMLAttribute "count" $ fig'count_count x
                       , maybe [] (toXMLAttribute "id") $ fig'count_id x
                       , maybe [] (toXMLAttribute "base") $ fig'count_base x
                       ]
            []

elementFig'count :: XMLParser Fig'count
elementFig'count = parseSchemaType "fig-count"
elementToXMLFig'count :: Fig'count -> [Content ()]
elementToXMLFig'count = schemaTypeToXML "fig-count"

data Fig'group = Fig'group
        { fig'group_content'type :: Maybe Xsd.XsdString
        , fig'group_id           :: Maybe Xsd.ID
        , fig'group_orientation  :: Maybe Xsd.XsdString
        , fig'group_position     :: Maybe Xsd.XsdString
        , fig'group_specific'use :: Maybe Xsd.XsdString
        , fig'group_base         :: Maybe Xsd.AnyURI
        , fig'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Fig'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "orientation" e pos
        a3 <- optional $ getAttribute "position" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Fig'group a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Fig'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ fig'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ fig'group_id x
                       , maybe [] (toXMLAttribute "orientation") $ fig'group_orientation x
                       , maybe [] (toXMLAttribute "position") $ fig'group_position x
                       , maybe [] (toXMLAttribute "specific-use") $ fig'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ fig'group_base x
                       , maybe [] (toXMLAttribute "lang") $ fig'group_lang x
                       ]
            []

elementFig'group :: XMLParser Fig'group
elementFig'group = parseSchemaType "fig-group"
elementToXMLFig'group :: Fig'group -> [Content ()]
elementToXMLFig'group = schemaTypeToXML "fig-group"

data Fixed'case = Fixed'case
        { fixed'case_content'type :: Maybe Xsd.XsdString
        , fixed'case_id           :: Maybe Xsd.ID
        , fixed'case_specific'use :: Maybe Xsd.XsdString
        , fixed'case_base         :: Maybe Xsd.AnyURI
        , fixed'case_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Fixed'case where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Fixed'case a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Fixed'case{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ fixed'case_content'type x
                       , maybe [] (toXMLAttribute "id") $ fixed'case_id x
                       , maybe [] (toXMLAttribute "specific-use") $ fixed'case_specific'use x
                       , maybe [] (toXMLAttribute "base") $ fixed'case_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ fixed'case_choice0 x
            ]

elementFixed'case :: XMLParser Fixed'case
elementFixed'case = parseSchemaType "fixed-case"
elementToXMLFixed'case :: Fixed'case -> [Content ()]
elementToXMLFixed'case = schemaTypeToXML "fixed-case"

data Floats'group = Floats'group
        { floats'group_id   :: Maybe Xsd.ID
        , floats'group_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Floats'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Floats'group a0 a1)
    schemaTypeToXML s x@Floats'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ floats'group_id x
                       , maybe [] (toXMLAttribute "base") $ floats'group_base x
                       ]
            []

elementFloats'group :: XMLParser Floats'group
elementFloats'group = parseSchemaType "floats-group"
elementToXMLFloats'group :: Floats'group -> [Content ()]
elementToXMLFloats'group = schemaTypeToXML "floats-group"

data Fn = Fn
        { fn_fn'type      :: Maybe Xsd.XsdString
        , fn_id           :: Maybe Xsd.ID
        , fn_specific'use :: Maybe Xsd.XsdString
        , fn_symbol       :: Maybe Xsd.XsdString
        , fn_base         :: Maybe Xsd.AnyURI
        , fn_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Fn where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "fn-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "symbol" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Fn a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Fn{} =
        toXMLElement s [ maybe [] (toXMLAttribute "fn-type") $ fn_fn'type x
                       , maybe [] (toXMLAttribute "id") $ fn_id x
                       , maybe [] (toXMLAttribute "specific-use") $ fn_specific'use x
                       , maybe [] (toXMLAttribute "symbol") $ fn_symbol x
                       , maybe [] (toXMLAttribute "base") $ fn_base x
                       , maybe [] (toXMLAttribute "lang") $ fn_lang x
                       ]
            []

elementFn :: XMLParser Fn
elementFn = parseSchemaType "fn"
elementToXMLFn :: Fn -> [Content ()]
elementToXMLFn = schemaTypeToXML "fn"

data Fn'group = Fn'group
        { fn'group_content'type :: Maybe Xsd.XsdString
        , fn'group_id           :: Maybe Xsd.ID
        , fn'group_specific'use :: Maybe Xsd.XsdString
        , fn'group_base         :: Maybe Xsd.AnyURI
        , fn'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Fn'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Fn'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Fn'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ fn'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ fn'group_id x
                       , maybe [] (toXMLAttribute "specific-use") $ fn'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ fn'group_base x
                       , maybe [] (toXMLAttribute "lang") $ fn'group_lang x
                       ]
            []

elementFn'group :: XMLParser Fn'group
elementFn'group = parseSchemaType "fn-group"
elementToXMLFn'group :: Fn'group -> [Content ()]
elementToXMLFn'group = schemaTypeToXML "fn-group"

data Fpage = Fpage
        { fpage_content'type :: Maybe Xsd.XsdString
        , fpage_id           :: Maybe Xsd.ID
        , fpage_seq          :: Maybe Xsd.XsdString
        , fpage_specific'use :: Maybe Xsd.XsdString
        , fpage_base         :: Maybe Xsd.AnyURI
        , fpage_lang         :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Fpage where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "seq" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Fpage a0 a1 a2 a3 a4 a5)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Fpage{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ fpage_content'type x
                       , maybe [] (toXMLAttribute "id") $ fpage_id x
                       , maybe [] (toXMLAttribute "seq") $ fpage_seq x
                       , maybe [] (toXMLAttribute "specific-use") $ fpage_specific'use x
                       , maybe [] (toXMLAttribute "base") $ fpage_base x
                       , maybe [] (toXMLAttribute "lang") $ fpage_lang x
                       ]
            [
            ]

elementFpage :: XMLParser Fpage
elementFpage = parseSchemaType "fpage"
elementToXMLFpage :: Fpage -> [Content ()]
elementToXMLFpage = schemaTypeToXML "fpage"

data Front = Front
        { front_id       :: Maybe Xsd.ID
        , front_base     :: Maybe Xsd.AnyURI
        , front_children :: [OneOf2 Journal'meta Article'meta]
        }
        deriving (Eq,Show)
instance SchemaType Front where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Front a0 a1)
          `apply` many (oneOf' [ ("", OneOf2 <$> elementJournal'meta)
                               , ("", TwoOf2 <$> elementArticle'meta)
                               ])
    schemaTypeToXML s x@Front{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ front_id x
                       , maybe [] (toXMLAttribute "base") $ front_base x
                       ]
            []

elementFront :: XMLParser Front
elementFront = parseSchemaType "front"
elementToXMLFront :: Front -> [Content ()]
elementToXMLFront = schemaTypeToXML "front"

data Front'stub = Front'stub
        { front'stub_id   :: Maybe Xsd.ID
        , front'stub_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Front'stub where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Front'stub a0 a1)
    schemaTypeToXML s x@Front'stub{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ front'stub_id x
                       , maybe [] (toXMLAttribute "base") $ front'stub_base x
                       ]
            []

elementFront'stub :: XMLParser Front'stub
elementFront'stub = parseSchemaType "front-stub"
elementToXMLFront'stub :: Front'stub -> [Content ()]
elementToXMLFront'stub = schemaTypeToXML "front-stub"

data Funding'group = Funding'group
        { funding'group_id           :: Maybe Xsd.ID
        , funding'group_specific'use :: Maybe Xsd.XsdString
        , funding'group_base         :: Maybe Xsd.AnyURI
        , funding'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Funding'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Funding'group a0 a1 a2 a3)
    schemaTypeToXML s x@Funding'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ funding'group_id x
                       , maybe [] (toXMLAttribute "specific-use") $ funding'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ funding'group_base x
                       , maybe [] (toXMLAttribute "lang") $ funding'group_lang x
                       ]
            []

elementFunding'group :: XMLParser Funding'group
elementFunding'group = parseSchemaType "funding-group"
elementToXMLFunding'group :: Funding'group -> [Content ()]
elementToXMLFunding'group = schemaTypeToXML "funding-group"

data Funding'source = Funding'source
        { funding'source_country      :: Maybe Xsd.XsdString
        , funding'source_id           :: Maybe Xsd.ID
        , funding'source_rid          :: Maybe Xsd.IDREFS
        , funding'source_source'type  :: Maybe Xsd.XsdString
        , funding'source_specific'use :: Maybe Xsd.XsdString
        , funding'source_actuate      :: Maybe Xsd.XsdString
        , funding'source_href         :: Maybe Xsd.AnyURI
        , funding'source_role         :: Maybe Xsd.XsdString
        , funding'source_show         :: Maybe Xsd.XsdString
        , funding'source_title        :: Maybe Xsd.XsdString
        , funding'source_type         :: Maybe Xsd.XsdString
        , funding'source_base         :: Maybe Xsd.AnyURI
        , funding'source_lang         :: Maybe Xsd.XsdString
        , funding'source_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Funding'source where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "country" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "rid" e pos
        a3 <- optional $ getAttribute "source-type" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "actuate" e pos
        a6 <- optional $ getAttribute "href" e pos
        a7 <- optional $ getAttribute "role" e pos
        a8 <- optional $ getAttribute "show" e pos
        a9 <- optional $ getAttribute "title" e pos
        a10 <- optional $ getAttribute "type" e pos
        a11 <- optional $ getAttribute "base" e pos
        a12 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Funding'source a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Funding'source{} =
        toXMLElement s [ maybe [] (toXMLAttribute "country") $ funding'source_country x
                       , maybe [] (toXMLAttribute "id") $ funding'source_id x
                       , maybe [] (toXMLAttribute "rid") $ funding'source_rid x
                       , maybe [] (toXMLAttribute "source-type") $ funding'source_source'type x
                       , maybe [] (toXMLAttribute "specific-use") $ funding'source_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ funding'source_actuate x
                       , maybe [] (toXMLAttribute "href") $ funding'source_href x
                       , maybe [] (toXMLAttribute "role") $ funding'source_role x
                       , maybe [] (toXMLAttribute "show") $ funding'source_show x
                       , maybe [] (toXMLAttribute "title") $ funding'source_title x
                       , maybe [] (toXMLAttribute "type") $ funding'source_type x
                       , maybe [] (toXMLAttribute "base") $ funding'source_base x
                       , maybe [] (toXMLAttribute "lang") $ funding'source_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ funding'source_choice0 x
            ]

elementFunding'source :: XMLParser Funding'source
elementFunding'source = parseSchemaType "funding-source"
elementToXMLFunding'source :: Funding'source -> [Content ()]
elementToXMLFunding'source = schemaTypeToXML "funding-source"

data Funding'statement = Funding'statement
        { funding'statement_id           :: Maybe Xsd.ID
        , funding'statement_rid          :: Maybe Xsd.IDREFS
        , funding'statement_specific'use :: Maybe Xsd.XsdString
        , funding'statement_base         :: Maybe Xsd.AnyURI
        , funding'statement_lang         :: Maybe Xsd.XsdString
        , funding'statement_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Funding'statement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "rid" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Funding'statement a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Funding'statement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ funding'statement_id x
                       , maybe [] (toXMLAttribute "rid") $ funding'statement_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ funding'statement_specific'use x
                       , maybe [] (toXMLAttribute "base") $ funding'statement_base x
                       , maybe [] (toXMLAttribute "lang") $ funding'statement_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ funding'statement_choice0 x
            ]

elementFunding'statement :: XMLParser Funding'statement
elementFunding'statement = parseSchemaType "funding-statement"
elementToXMLFunding'statement :: Funding'statement -> [Content ()]
elementToXMLFunding'statement = schemaTypeToXML "funding-statement"

data Given'names = Given'names
        { given'names_id       :: Maybe Xsd.ID
        , given'names_initials :: Maybe Xsd.XsdString
        , given'names_base     :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Given'names where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "initials" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Given'names a0 a1 a2)
    schemaTypeToXML s x@Given'names{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ given'names_id x
                       , maybe [] (toXMLAttribute "initials") $ given'names_initials x
                       , maybe [] (toXMLAttribute "base") $ given'names_base x
                       ]
            []

elementGiven'names :: XMLParser Given'names
elementGiven'names = parseSchemaType "given-names"
elementToXMLGiven'names :: Given'names -> [Content ()]
elementToXMLGiven'names = schemaTypeToXML "given-names"

data Glossary = Glossary
        { glossary_content'type :: Maybe Xsd.XsdString
        , glossary_id           :: Maybe Xsd.ID
        , glossary_specific'use :: Maybe Xsd.XsdString
        , glossary_base         :: Maybe Xsd.AnyURI
        , glossary_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Glossary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Glossary a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Glossary{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ glossary_content'type x
                       , maybe [] (toXMLAttribute "id") $ glossary_id x
                       , maybe [] (toXMLAttribute "specific-use") $ glossary_specific'use x
                       , maybe [] (toXMLAttribute "base") $ glossary_base x
                       , maybe [] (toXMLAttribute "lang") $ glossary_lang x
                       ]
            []

elementGlossary :: XMLParser Glossary
elementGlossary = parseSchemaType "glossary"
elementToXMLGlossary :: Glossary -> [Content ()]
elementToXMLGlossary = schemaTypeToXML "glossary"

data Glyph'data = Glyph'data
        { glyph'data_fontchar   :: Maybe Xsd.XsdString
        , glyph'data_fontname   :: Maybe Xsd.XsdString
        , glyph'data_format     :: Maybe Xsd.NMTOKEN
        , glyph'data_id         :: Maybe Xsd.ID
        , glyph'data_resolution :: Maybe Xsd.XsdString
        , glyph'data_x'size     :: Maybe Xsd.XsdString
        , glyph'data_base       :: Maybe Xsd.AnyURI
        , glyph'data_space      :: Maybe Xsd.XsdString
        , glyph'data_y'size     :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Glyph'data where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "fontchar" e pos
        a1 <- optional $ getAttribute "fontname" e pos
        a2 <- optional $ getAttribute "format" e pos
        a3 <- optional $ getAttribute "id" e pos
        a4 <- optional $ getAttribute "resolution" e pos
        a5 <- optional $ getAttribute "x-size" e pos
        a6 <- optional $ getAttribute "base" e pos
        a7 <- optional $ getAttribute "space" e pos
        a8 <- optional $ getAttribute "y-size" e pos
        commit $ interior e $ return (Glyph'data a0 a1 a2 a3 a4 a5 a6 a7 a8)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Glyph'data{} =
        toXMLElement s [ maybe [] (toXMLAttribute "fontchar") $ glyph'data_fontchar x
                       , maybe [] (toXMLAttribute "fontname") $ glyph'data_fontname x
                       , maybe [] (toXMLAttribute "format") $ glyph'data_format x
                       , maybe [] (toXMLAttribute "id") $ glyph'data_id x
                       , maybe [] (toXMLAttribute "resolution") $ glyph'data_resolution x
                       , maybe [] (toXMLAttribute "x-size") $ glyph'data_x'size x
                       , maybe [] (toXMLAttribute "base") $ glyph'data_base x
                       , maybe [] (toXMLAttribute "space") $ glyph'data_space x
                       , maybe [] (toXMLAttribute "y-size") $ glyph'data_y'size x
                       ]
            [
            ]

elementGlyph'data :: XMLParser Glyph'data
elementGlyph'data = parseSchemaType "glyph-data"
elementToXMLGlyph'data :: Glyph'data -> [Content ()]
elementToXMLGlyph'data = schemaTypeToXML "glyph-data"

data Glyph'ref = Glyph'ref
        { glyph'ref_glyph'data :: Maybe Xsd.IDREF
        , glyph'ref_id         :: Maybe Xsd.ID
        , glyph'ref_base       :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Glyph'ref where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "glyph-data" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Glyph'ref a0 a1 a2)
    schemaTypeToXML s x@Glyph'ref{} =
        toXMLElement s [ maybe [] (toXMLAttribute "glyph-data") $ glyph'ref_glyph'data x
                       , maybe [] (toXMLAttribute "id") $ glyph'ref_id x
                       , maybe [] (toXMLAttribute "base") $ glyph'ref_base x
                       ]
            []

elementGlyph'ref :: XMLParser Glyph'ref
elementGlyph'ref = parseSchemaType "glyph-ref"
elementToXMLGlyph'ref :: Glyph'ref -> [Content ()]
elementToXMLGlyph'ref = schemaTypeToXML "glyph-ref"

data Gov = Gov
        { gov_content'type :: Maybe Xsd.XsdString
        , gov_id           :: Maybe Xsd.ID
        , gov_specific'use :: Maybe Xsd.XsdString
        , gov_base         :: Maybe Xsd.AnyURI
        , gov_lang         :: Maybe Xsd.XsdString
        , gov_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Gov where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Gov a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Gov{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ gov_content'type x
                       , maybe [] (toXMLAttribute "id") $ gov_id x
                       , maybe [] (toXMLAttribute "specific-use") $ gov_specific'use x
                       , maybe [] (toXMLAttribute "base") $ gov_base x
                       , maybe [] (toXMLAttribute "lang") $ gov_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ gov_choice0 x
            ]

elementGov :: XMLParser Gov
elementGov = parseSchemaType "gov"
elementToXMLGov :: Gov -> [Content ()]
elementToXMLGov = schemaTypeToXML "gov"

data Graphic = Graphic
        { graphic_content'type :: Maybe Xsd.XsdString
        , graphic_id           :: Maybe Xsd.ID
        , graphic_mime'subtype :: Maybe Xsd.XsdString
        , graphic_mimetype     :: Maybe Xsd.XsdString
        , graphic_orientation  :: Maybe Xsd.XsdString
        , graphic_position     :: Maybe Xsd.XsdString
        , graphic_specific'use :: Maybe Xsd.XsdString
        , graphic_actuate      :: Maybe Xsd.XsdString
        , graphic_href         :: Maybe Xsd.AnyURI
        , graphic_role         :: Maybe Xsd.XsdString
        , graphic_show         :: Maybe Xsd.XsdString
        , graphic_title        :: Maybe Xsd.XsdString
        , graphic_type         :: Maybe Xsd.XsdString
        , graphic_base         :: Maybe Xsd.AnyURI
        , graphic_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Graphic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "mime-subtype" e pos
        a3 <- optional $ getAttribute "mimetype" e pos
        a4 <- optional $ getAttribute "orientation" e pos
        a5 <- optional $ getAttribute "position" e pos
        a6 <- optional $ getAttribute "specific-use" e pos
        a7 <- optional $ getAttribute "actuate" e pos
        a8 <- optional $ getAttribute "href" e pos
        a9 <- optional $ getAttribute "role" e pos
        a10 <- optional $ getAttribute "show" e pos
        a11 <- optional $ getAttribute "title" e pos
        a12 <- optional $ getAttribute "type" e pos
        a13 <- optional $ getAttribute "base" e pos
        a14 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Graphic a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
    schemaTypeToXML s x@Graphic{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ graphic_content'type x
                       , maybe [] (toXMLAttribute "id") $ graphic_id x
                       , maybe [] (toXMLAttribute "mime-subtype") $ graphic_mime'subtype x
                       , maybe [] (toXMLAttribute "mimetype") $ graphic_mimetype x
                       , maybe [] (toXMLAttribute "orientation") $ graphic_orientation x
                       , maybe [] (toXMLAttribute "position") $ graphic_position x
                       , maybe [] (toXMLAttribute "specific-use") $ graphic_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ graphic_actuate x
                       , maybe [] (toXMLAttribute "href") $ graphic_href x
                       , maybe [] (toXMLAttribute "role") $ graphic_role x
                       , maybe [] (toXMLAttribute "show") $ graphic_show x
                       , maybe [] (toXMLAttribute "title") $ graphic_title x
                       , maybe [] (toXMLAttribute "type") $ graphic_type x
                       , maybe [] (toXMLAttribute "base") $ graphic_base x
                       , maybe [] (toXMLAttribute "lang") $ graphic_lang x
                       ]
            []

elementGraphic :: XMLParser Graphic
elementGraphic = parseSchemaType "graphic"
elementToXMLGraphic :: Graphic -> [Content ()]
elementToXMLGraphic = schemaTypeToXML "graphic"

data History = History
        { history_id   :: Maybe Xsd.ID
        , history_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType History where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (History a0 a1)
    schemaTypeToXML s x@History{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ history_id x
                       , maybe [] (toXMLAttribute "base") $ history_base x
                       ]
            []

elementHistory :: XMLParser History
elementHistory = parseSchemaType "history"
elementToXMLHistory :: History -> [Content ()]
elementToXMLHistory = schemaTypeToXML "history"

data Hr = Hr
        { hr_id   :: Maybe Xsd.ID
        , hr_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Hr where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Hr a0 a1)
    schemaTypeToXML s x@Hr{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ hr_id x
                       , maybe [] (toXMLAttribute "base") $ hr_base x
                       ]
            []

elementHr :: XMLParser Hr
elementHr = parseSchemaType "hr"
elementToXMLHr :: Hr -> [Content ()]
elementToXMLHr = schemaTypeToXML "hr"

data Inline'formula = Inline'formula
        { inline'formula_content'type :: Maybe Xsd.XsdString
        , inline'formula_id           :: Maybe Xsd.ID
        , inline'formula_specific'use :: Maybe Xsd.XsdString
        , inline'formula_base         :: Maybe Xsd.AnyURI
        , inline'formula_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Inline'formula where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Inline'formula a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Inline'formula{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ inline'formula_content'type x
                       , maybe [] (toXMLAttribute "id") $ inline'formula_id x
                       , maybe [] (toXMLAttribute "specific-use") $ inline'formula_specific'use x
                       , maybe [] (toXMLAttribute "base") $ inline'formula_base x
                       , maybe [] (toXMLAttribute "lang") $ inline'formula_lang x
                       ]
            []

elementInline'formula :: XMLParser Inline'formula
elementInline'formula = parseSchemaType "inline-formula"
elementToXMLInline'formula :: Inline'formula -> [Content ()]
elementToXMLInline'formula = schemaTypeToXML "inline-formula"

data Inline'graphic = Inline'graphic
        { inline'graphic_baseline'shift :: Maybe Xsd.XsdString
        , inline'graphic_content'type   :: Maybe Xsd.XsdString
        , inline'graphic_id             :: Maybe Xsd.ID
        , inline'graphic_mime'subtype   :: Maybe Xsd.XsdString
        , inline'graphic_mimetype       :: Maybe Xsd.XsdString
        , inline'graphic_specific'use   :: Maybe Xsd.XsdString
        , inline'graphic_actuate        :: Maybe Xsd.XsdString
        , inline'graphic_href           :: Maybe Xsd.AnyURI
        , inline'graphic_role           :: Maybe Xsd.XsdString
        , inline'graphic_show           :: Maybe Xsd.XsdString
        , inline'graphic_title          :: Maybe Xsd.XsdString
        , inline'graphic_type           :: Maybe Xsd.XsdString
        , inline'graphic_base           :: Maybe Xsd.AnyURI
        , inline'graphic_lang           :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Inline'graphic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "baseline-shift" e pos
        a1 <- optional $ getAttribute "content-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "mime-subtype" e pos
        a4 <- optional $ getAttribute "mimetype" e pos
        a5 <- optional $ getAttribute "specific-use" e pos
        a6 <- optional $ getAttribute "actuate" e pos
        a7 <- optional $ getAttribute "href" e pos
        a8 <- optional $ getAttribute "role" e pos
        a9 <- optional $ getAttribute "show" e pos
        a10 <- optional $ getAttribute "title" e pos
        a11 <- optional $ getAttribute "type" e pos
        a12 <- optional $ getAttribute "base" e pos
        a13 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Inline'graphic a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13)
    schemaTypeToXML s x@Inline'graphic{} =
        toXMLElement s [ maybe [] (toXMLAttribute "baseline-shift") $ inline'graphic_baseline'shift x
                       , maybe [] (toXMLAttribute "content-type") $ inline'graphic_content'type x
                       , maybe [] (toXMLAttribute "id") $ inline'graphic_id x
                       , maybe [] (toXMLAttribute "mime-subtype") $ inline'graphic_mime'subtype x
                       , maybe [] (toXMLAttribute "mimetype") $ inline'graphic_mimetype x
                       , maybe [] (toXMLAttribute "specific-use") $ inline'graphic_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ inline'graphic_actuate x
                       , maybe [] (toXMLAttribute "href") $ inline'graphic_href x
                       , maybe [] (toXMLAttribute "role") $ inline'graphic_role x
                       , maybe [] (toXMLAttribute "show") $ inline'graphic_show x
                       , maybe [] (toXMLAttribute "title") $ inline'graphic_title x
                       , maybe [] (toXMLAttribute "type") $ inline'graphic_type x
                       , maybe [] (toXMLAttribute "base") $ inline'graphic_base x
                       , maybe [] (toXMLAttribute "lang") $ inline'graphic_lang x
                       ]
            []

elementInline'graphic :: XMLParser Inline'graphic
elementInline'graphic = parseSchemaType "inline-graphic"
elementToXMLInline'graphic :: Inline'graphic -> [Content ()]
elementToXMLInline'graphic = schemaTypeToXML "inline-graphic"

data Inline'supplementary'material = Inline'supplementary'material
        { inline'supplementary'material_content'type :: Maybe Xsd.XsdString
        , inline'supplementary'material_id           :: Maybe Xsd.ID
        , inline'supplementary'material_mime'subtype :: Maybe Xsd.XsdString
        , inline'supplementary'material_mimetype     :: Maybe Xsd.XsdString
        , inline'supplementary'material_specific'use :: Maybe Xsd.XsdString
        , inline'supplementary'material_actuate      :: Maybe Xsd.XsdString
        , inline'supplementary'material_href         :: Maybe Xsd.AnyURI
        , inline'supplementary'material_role         :: Maybe Xsd.XsdString
        , inline'supplementary'material_show         :: Maybe Xsd.XsdString
        , inline'supplementary'material_title        :: Maybe Xsd.XsdString
        , inline'supplementary'material_type         :: Maybe Xsd.XsdString
        , inline'supplementary'material_base         :: Maybe Xsd.AnyURI
        , inline'supplementary'material_lang         :: Maybe Xsd.XsdString
        , inline'supplementary'material_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Inline'supplementary'material where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "mime-subtype" e pos
        a3 <- optional $ getAttribute "mimetype" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "actuate" e pos
        a6 <- optional $ getAttribute "href" e pos
        a7 <- optional $ getAttribute "role" e pos
        a8 <- optional $ getAttribute "show" e pos
        a9 <- optional $ getAttribute "title" e pos
        a10 <- optional $ getAttribute "type" e pos
        a11 <- optional $ getAttribute "base" e pos
        a12 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Inline'supplementary'material a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Inline'supplementary'material{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ inline'supplementary'material_content'type x
                       , maybe [] (toXMLAttribute "id") $ inline'supplementary'material_id x
                       , maybe [] (toXMLAttribute "mime-subtype") $ inline'supplementary'material_mime'subtype x
                       , maybe [] (toXMLAttribute "mimetype") $ inline'supplementary'material_mimetype x
                       , maybe [] (toXMLAttribute "specific-use") $ inline'supplementary'material_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ inline'supplementary'material_actuate x
                       , maybe [] (toXMLAttribute "href") $ inline'supplementary'material_href x
                       , maybe [] (toXMLAttribute "role") $ inline'supplementary'material_role x
                       , maybe [] (toXMLAttribute "show") $ inline'supplementary'material_show x
                       , maybe [] (toXMLAttribute "title") $ inline'supplementary'material_title x
                       , maybe [] (toXMLAttribute "type") $ inline'supplementary'material_type x
                       , maybe [] (toXMLAttribute "base") $ inline'supplementary'material_base x
                       , maybe [] (toXMLAttribute "lang") $ inline'supplementary'material_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ inline'supplementary'material_choice0 x
            ]

elementInline'supplementary'material :: XMLParser Inline'supplementary'material
elementInline'supplementary'material = parseSchemaType "inline-supplementary-material"
elementToXMLInline'supplementary'material :: Inline'supplementary'material -> [Content ()]
elementToXMLInline'supplementary'material = schemaTypeToXML "inline-supplementary-material"

data Institution = Institution
        { institution_content'type :: Maybe Xsd.XsdString
        , institution_id           :: Maybe Xsd.ID
        , institution_specific'use :: Maybe Xsd.XsdString
        , institution_actuate      :: Maybe Xsd.XsdString
        , institution_href         :: Maybe Xsd.AnyURI
        , institution_role         :: Maybe Xsd.XsdString
        , institution_show         :: Maybe Xsd.XsdString
        , institution_title        :: Maybe Xsd.XsdString
        , institution_type         :: Maybe Xsd.XsdString
        , institution_base         :: Maybe Xsd.AnyURI
        , institution_lang         :: Maybe Xsd.XsdString
        , institution_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Institution where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "actuate" e pos
        a4 <- optional $ getAttribute "href" e pos
        a5 <- optional $ getAttribute "role" e pos
        a6 <- optional $ getAttribute "show" e pos
        a7 <- optional $ getAttribute "title" e pos
        a8 <- optional $ getAttribute "type" e pos
        a9 <- optional $ getAttribute "base" e pos
        a10 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Institution a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Institution{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ institution_content'type x
                       , maybe [] (toXMLAttribute "id") $ institution_id x
                       , maybe [] (toXMLAttribute "specific-use") $ institution_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ institution_actuate x
                       , maybe [] (toXMLAttribute "href") $ institution_href x
                       , maybe [] (toXMLAttribute "role") $ institution_role x
                       , maybe [] (toXMLAttribute "show") $ institution_show x
                       , maybe [] (toXMLAttribute "title") $ institution_title x
                       , maybe [] (toXMLAttribute "type") $ institution_type x
                       , maybe [] (toXMLAttribute "base") $ institution_base x
                       , maybe [] (toXMLAttribute "lang") $ institution_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ institution_choice0 x
            ]

elementInstitution :: XMLParser Institution
elementInstitution = parseSchemaType "institution"
elementToXMLInstitution :: Institution -> [Content ()]
elementToXMLInstitution = schemaTypeToXML "institution"

data Institution'id = Institution'id
        { institution'id_content'type        :: Maybe Xsd.XsdString
        , institution'id_id                  :: Maybe Xsd.ID
        , institution'id_institution'id'type :: Maybe Xsd.XsdString
        , institution'id_specific'use        :: Maybe Xsd.XsdString
        , institution'id_base                :: Maybe Xsd.AnyURI
        , institution'id_lang                :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Institution'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "institution-id-type" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Institution'id a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Institution'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ institution'id_content'type x
                       , maybe [] (toXMLAttribute "id") $ institution'id_id x
                       , maybe [] (toXMLAttribute "institution-id-type") $ institution'id_institution'id'type x
                       , maybe [] (toXMLAttribute "specific-use") $ institution'id_specific'use x
                       , maybe [] (toXMLAttribute "base") $ institution'id_base x
                       , maybe [] (toXMLAttribute "lang") $ institution'id_lang x
                       ]
            []

elementInstitution'id :: XMLParser Institution'id
elementInstitution'id = parseSchemaType "institution-id"
elementToXMLInstitution'id :: Institution'id -> [Content ()]
elementToXMLInstitution'id = schemaTypeToXML "institution-id"

data Institution'wrap = Institution'wrap
        { institution'wrap_id   :: Maybe Xsd.ID
        , institution'wrap_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Institution'wrap where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Institution'wrap a0 a1)
    schemaTypeToXML s x@Institution'wrap{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ institution'wrap_id x
                       , maybe [] (toXMLAttribute "base") $ institution'wrap_base x
                       ]
            []

elementInstitution'wrap :: XMLParser Institution'wrap
elementInstitution'wrap = parseSchemaType "institution-wrap"
elementToXMLInstitution'wrap :: Institution'wrap -> [Content ()]
elementToXMLInstitution'wrap = schemaTypeToXML "institution-wrap"

data Isbn = Isbn
        { isbn_content'type       :: Maybe Xsd.XsdString
        , isbn_id                 :: Maybe Xsd.ID
        , isbn_publication'format :: Maybe Xsd.XsdString
        , isbn_specific'use       :: Maybe Xsd.XsdString
        , isbn_base               :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Isbn where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "publication-format" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Isbn a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Isbn{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ isbn_content'type x
                       , maybe [] (toXMLAttribute "id") $ isbn_id x
                       , maybe [] (toXMLAttribute "publication-format") $ isbn_publication'format x
                       , maybe [] (toXMLAttribute "specific-use") $ isbn_specific'use x
                       , maybe [] (toXMLAttribute "base") $ isbn_base x
                       ]
            []

elementIsbn :: XMLParser Isbn
elementIsbn = parseSchemaType "isbn"
elementToXMLIsbn :: Isbn -> [Content ()]
elementToXMLIsbn = schemaTypeToXML "isbn"

data Issn = Issn
        { issn_content'type       :: Maybe Xsd.XsdString
        , issn_id                 :: Maybe Xsd.ID
        , issn_pub'type           :: Maybe Xsd.XsdString
        , issn_publication'format :: Maybe Xsd.XsdString
        , issn_specific'use       :: Maybe Xsd.XsdString
        , issn_base               :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Issn where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "pub-type" e pos
        a3 <- optional $ getAttribute "publication-format" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Issn a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Issn{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ issn_content'type x
                       , maybe [] (toXMLAttribute "id") $ issn_id x
                       , maybe [] (toXMLAttribute "pub-type") $ issn_pub'type x
                       , maybe [] (toXMLAttribute "publication-format") $ issn_publication'format x
                       , maybe [] (toXMLAttribute "specific-use") $ issn_specific'use x
                       , maybe [] (toXMLAttribute "base") $ issn_base x
                       ]
            []

elementIssn :: XMLParser Issn
elementIssn = parseSchemaType "issn"
elementToXMLIssn :: Issn -> [Content ()]
elementToXMLIssn = schemaTypeToXML "issn"

data Issn'l = Issn'l
        { issn'l_id           :: Maybe Xsd.ID
        , issn'l_specific'use :: Maybe Xsd.XsdString
        , issn'l_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Issn'l where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Issn'l a0 a1 a2)
    schemaTypeToXML s x@Issn'l{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ issn'l_id x
                       , maybe [] (toXMLAttribute "specific-use") $ issn'l_specific'use x
                       , maybe [] (toXMLAttribute "base") $ issn'l_base x
                       ]
            []

elementIssn'l :: XMLParser Issn'l
elementIssn'l = parseSchemaType "issn-l"
elementToXMLIssn'l :: Issn'l -> [Content ()]
elementToXMLIssn'l = schemaTypeToXML "issn-l"

data Issue = Issue
        { issue_content'type :: Maybe Xsd.XsdString
        , issue_id           :: Maybe Xsd.ID
        , issue_seq          :: Maybe Xsd.XsdString
        , issue_specific'use :: Maybe Xsd.XsdString
        , issue_base         :: Maybe Xsd.AnyURI
        , issue_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Issue where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "seq" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Issue a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Issue{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ issue_content'type x
                       , maybe [] (toXMLAttribute "id") $ issue_id x
                       , maybe [] (toXMLAttribute "seq") $ issue_seq x
                       , maybe [] (toXMLAttribute "specific-use") $ issue_specific'use x
                       , maybe [] (toXMLAttribute "base") $ issue_base x
                       , maybe [] (toXMLAttribute "lang") $ issue_lang x
                       ]
            []

elementIssue :: XMLParser Issue
elementIssue = parseSchemaType "issue"
elementToXMLIssue :: Issue -> [Content ()]
elementToXMLIssue = schemaTypeToXML "issue"

data Issue'id = Issue'id
        { issue'id_content'type :: Maybe Xsd.XsdString
        , issue'id_id           :: Maybe Xsd.ID
        , issue'id_pub'id'type  :: Maybe Xsd.XsdString
        , issue'id_specific'use :: Maybe Xsd.XsdString
        , issue'id_actuate      :: Maybe Xsd.XsdString
        , issue'id_href         :: Maybe Xsd.AnyURI
        , issue'id_role         :: Maybe Xsd.XsdString
        , issue'id_show         :: Maybe Xsd.XsdString
        , issue'id_title        :: Maybe Xsd.XsdString
        , issue'id_type         :: Maybe Xsd.XsdString
        , issue'id_base         :: Maybe Xsd.AnyURI
        , issue'id_lang         :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Issue'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "pub-id-type" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "actuate" e pos
        a5 <- optional $ getAttribute "href" e pos
        a6 <- optional $ getAttribute "role" e pos
        a7 <- optional $ getAttribute "show" e pos
        a8 <- optional $ getAttribute "title" e pos
        a9 <- optional $ getAttribute "type" e pos
        a10 <- optional $ getAttribute "base" e pos
        a11 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Issue'id a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Issue'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ issue'id_content'type x
                       , maybe [] (toXMLAttribute "id") $ issue'id_id x
                       , maybe [] (toXMLAttribute "pub-id-type") $ issue'id_pub'id'type x
                       , maybe [] (toXMLAttribute "specific-use") $ issue'id_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ issue'id_actuate x
                       , maybe [] (toXMLAttribute "href") $ issue'id_href x
                       , maybe [] (toXMLAttribute "role") $ issue'id_role x
                       , maybe [] (toXMLAttribute "show") $ issue'id_show x
                       , maybe [] (toXMLAttribute "title") $ issue'id_title x
                       , maybe [] (toXMLAttribute "type") $ issue'id_type x
                       , maybe [] (toXMLAttribute "base") $ issue'id_base x
                       , maybe [] (toXMLAttribute "lang") $ issue'id_lang x
                       ]
            [
            ]

elementIssue'id :: XMLParser Issue'id
elementIssue'id = parseSchemaType "issue-id"
elementToXMLIssue'id :: Issue'id -> [Content ()]
elementToXMLIssue'id = schemaTypeToXML "issue-id"

data Issue'part = Issue'part
        { issue'part_content'type :: Maybe Xsd.XsdString
        , issue'part_id           :: Maybe Xsd.ID
        , issue'part_specific'use :: Maybe Xsd.XsdString
        , issue'part_base         :: Maybe Xsd.AnyURI
        , issue'part_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Issue'part where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Issue'part a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Issue'part{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ issue'part_content'type x
                       , maybe [] (toXMLAttribute "id") $ issue'part_id x
                       , maybe [] (toXMLAttribute "specific-use") $ issue'part_specific'use x
                       , maybe [] (toXMLAttribute "base") $ issue'part_base x
                       , maybe [] (toXMLAttribute "lang") $ issue'part_lang x
                       ]
            []

elementIssue'part :: XMLParser Issue'part
elementIssue'part = parseSchemaType "issue-part"
elementToXMLIssue'part :: Issue'part -> [Content ()]
elementToXMLIssue'part = schemaTypeToXML "issue-part"

data Issue'sponsor = Issue'sponsor
        { issue'sponsor_content'type :: Maybe Xsd.XsdString
        , issue'sponsor_id           :: Maybe Xsd.ID
        , issue'sponsor_specific'use :: Maybe Xsd.XsdString
        , issue'sponsor_base         :: Maybe Xsd.AnyURI
        , issue'sponsor_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Issue'sponsor where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Issue'sponsor a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Issue'sponsor{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ issue'sponsor_content'type x
                       , maybe [] (toXMLAttribute "id") $ issue'sponsor_id x
                       , maybe [] (toXMLAttribute "specific-use") $ issue'sponsor_specific'use x
                       , maybe [] (toXMLAttribute "base") $ issue'sponsor_base x
                       , maybe [] (toXMLAttribute "lang") $ issue'sponsor_lang x
                       ]
            []

elementIssue'sponsor :: XMLParser Issue'sponsor
elementIssue'sponsor = parseSchemaType "issue-sponsor"
elementToXMLIssue'sponsor :: Issue'sponsor -> [Content ()]
elementToXMLIssue'sponsor = schemaTypeToXML "issue-sponsor"

data Issue'title = Issue'title
        { issue'title_content'type :: Maybe Xsd.XsdString
        , issue'title_id           :: Maybe Xsd.ID
        , issue'title_specific'use :: Maybe Xsd.XsdString
        , issue'title_base         :: Maybe Xsd.AnyURI
        , issue'title_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Issue'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Issue'title a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Issue'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ issue'title_content'type x
                       , maybe [] (toXMLAttribute "id") $ issue'title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ issue'title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ issue'title_base x
                       , maybe [] (toXMLAttribute "lang") $ issue'title_lang x
                       ]
            []

elementIssue'title :: XMLParser Issue'title
elementIssue'title = parseSchemaType "issue-title"
elementToXMLIssue'title :: Issue'title -> [Content ()]
elementToXMLIssue'title = schemaTypeToXML "issue-title"

data Italic = Italic
        { italic_id           :: Maybe Xsd.ID
        , italic_specific'use :: Maybe Xsd.XsdString
        , italic_toggle       :: Maybe Xsd.XsdString
        , italic_base         :: Maybe Xsd.AnyURI
        , italic_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Italic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "toggle" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Italic a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Italic{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ italic_id x
                       , maybe [] (toXMLAttribute "specific-use") $ italic_specific'use x
                       , maybe [] (toXMLAttribute "toggle") $ italic_toggle x
                       , maybe [] (toXMLAttribute "base") $ italic_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ italic_choice0 x
            ]

elementItalic :: XMLParser Italic
elementItalic = parseSchemaType "italic"
elementToXMLItalic :: Italic -> [Content ()]
elementToXMLItalic = schemaTypeToXML "italic"

data Journal'id = Journal'id
        { journal'id_id              :: Maybe Xsd.ID
        , journal'id_journal'id'type :: Maybe Xsd.XsdString
        , journal'id_specific'use    :: Maybe Xsd.XsdString
        , journal'id_base            :: Maybe Xsd.AnyURI
        , journal'id_lang            :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Journal'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "journal-id-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Journal'id a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Journal'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ journal'id_id x
                       , maybe [] (toXMLAttribute "journal-id-type") $ journal'id_journal'id'type x
                       , maybe [] (toXMLAttribute "specific-use") $ journal'id_specific'use x
                       , maybe [] (toXMLAttribute "base") $ journal'id_base x
                       , maybe [] (toXMLAttribute "lang") $ journal'id_lang x
                       ]
            [
            ]

elementJournal'id :: XMLParser Journal'id
elementJournal'id = parseSchemaType "journal-id"
elementToXMLJournal'id :: Journal'id -> [Content ()]
elementToXMLJournal'id = schemaTypeToXML "journal-id"

data Journal'meta = Journal'meta
        { journal'meta_id   :: Maybe Xsd.ID
        , journal'meta_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Journal'meta where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Journal'meta a0 a1)
    schemaTypeToXML s x@Journal'meta{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ journal'meta_id x
                       , maybe [] (toXMLAttribute "base") $ journal'meta_base x
                       ]
            []

elementJournal'meta :: XMLParser Journal'meta
elementJournal'meta = parseSchemaType "journal-meta"
elementToXMLJournal'meta :: Journal'meta -> [Content ()]
elementToXMLJournal'meta = schemaTypeToXML "journal-meta"

data Journal'subtitle = Journal'subtitle
        { journal'subtitle_content'type :: Maybe Xsd.XsdString
        , journal'subtitle_id           :: Maybe Xsd.ID
        , journal'subtitle_specific'use :: Maybe Xsd.XsdString
        , journal'subtitle_base         :: Maybe Xsd.AnyURI
        , journal'subtitle_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Journal'subtitle where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Journal'subtitle a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Journal'subtitle{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ journal'subtitle_content'type x
                       , maybe [] (toXMLAttribute "id") $ journal'subtitle_id x
                       , maybe [] (toXMLAttribute "specific-use") $ journal'subtitle_specific'use x
                       , maybe [] (toXMLAttribute "base") $ journal'subtitle_base x
                       , maybe [] (toXMLAttribute "lang") $ journal'subtitle_lang x
                       ]
            []

elementJournal'subtitle :: XMLParser Journal'subtitle
elementJournal'subtitle = parseSchemaType "journal-subtitle"
elementToXMLJournal'subtitle :: Journal'subtitle -> [Content ()]
elementToXMLJournal'subtitle = schemaTypeToXML "journal-subtitle"

data Journal'title = Journal'title
        { journal'title_content'type :: Maybe Xsd.XsdString
        , journal'title_id           :: Maybe Xsd.ID
        , journal'title_specific'use :: Maybe Xsd.XsdString
        , journal'title_base         :: Maybe Xsd.AnyURI
        , journal'title_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Journal'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Journal'title a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Journal'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ journal'title_content'type x
                       , maybe [] (toXMLAttribute "id") $ journal'title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ journal'title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ journal'title_base x
                       , maybe [] (toXMLAttribute "lang") $ journal'title_lang x
                       ]
            []

elementJournal'title :: XMLParser Journal'title
elementJournal'title = parseSchemaType "journal-title"
elementToXMLJournal'title :: Journal'title -> [Content ()]
elementToXMLJournal'title = schemaTypeToXML "journal-title"

data Journal'title'group = Journal'title'group
        { journal'title'group_content'type :: Maybe Xsd.XsdString
        , journal'title'group_id           :: Maybe Xsd.ID
        , journal'title'group_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Journal'title'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Journal'title'group a0 a1 a2)
    schemaTypeToXML s x@Journal'title'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ journal'title'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ journal'title'group_id x
                       , maybe [] (toXMLAttribute "base") $ journal'title'group_base x
                       ]
            []

elementJournal'title'group :: XMLParser Journal'title'group
elementJournal'title'group = parseSchemaType "journal-title-group"
elementToXMLJournal'title'group :: Journal'title'group -> [Content ()]
elementToXMLJournal'title'group = schemaTypeToXML "journal-title-group"

data Kwd = Kwd
        { kwd_content'type :: Maybe Xsd.XsdString
        , kwd_id           :: Maybe Xsd.ID
        , kwd_base         :: Maybe Xsd.AnyURI
        , kwd_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Kwd where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Kwd a0 a1 a2)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Kwd{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ kwd_content'type x
                       , maybe [] (toXMLAttribute "id") $ kwd_id x
                       , maybe [] (toXMLAttribute "base") $ kwd_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ kwd_choice0 x
            ]

elementKwd :: XMLParser Kwd
elementKwd = parseSchemaType "kwd"
elementToXMLKwd :: Kwd -> [Content ()]
elementToXMLKwd = schemaTypeToXML "kwd"

data Kwd'group = Kwd'group
        { kwd'group_id             :: Maybe Xsd.ID
        , kwd'group_kwd'group'type :: Maybe Xsd.XsdString
        , kwd'group_specific'use   :: Maybe Xsd.XsdString
        , kwd'group_base           :: Maybe Xsd.AnyURI
        , kwd'group_lang           :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Kwd'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "kwd-group-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Kwd'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Kwd'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ kwd'group_id x
                       , maybe [] (toXMLAttribute "kwd-group-type") $ kwd'group_kwd'group'type x
                       , maybe [] (toXMLAttribute "specific-use") $ kwd'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ kwd'group_base x
                       , maybe [] (toXMLAttribute "lang") $ kwd'group_lang x
                       ]
            []

elementKwd'group :: XMLParser Kwd'group
elementKwd'group = parseSchemaType "kwd-group"
elementToXMLKwd'group :: Kwd'group -> [Content ()]
elementToXMLKwd'group = schemaTypeToXML "kwd-group"

data Label = Label
        { label_alt     :: Maybe Xsd.XsdString
        , label_id      :: Maybe Xsd.ID
        , label_base    :: Maybe Xsd.AnyURI
        , label_lang    :: Maybe Xsd.XsdString
        , label_choice0 :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Label where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "alt" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Label a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Label{} =
        toXMLElement s [ maybe [] (toXMLAttribute "alt") $ label_alt x
                       , maybe [] (toXMLAttribute "id") $ label_id x
                       , maybe [] (toXMLAttribute "base") $ label_base x
                       , maybe [] (toXMLAttribute "lang") $ label_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ label_choice0 x
            ]

elementLabel :: XMLParser Label
elementLabel = parseSchemaType "label"
elementToXMLLabel :: Label -> [Content ()]
elementToXMLLabel = schemaTypeToXML "label"

data License = License
        { license_id           :: Maybe Xsd.ID
        , license_license'type :: Maybe Xsd.XsdString
        , license_specific'use :: Maybe Xsd.XsdString
        , license_actuate      :: Maybe Xsd.XsdString
        , license_href         :: Maybe Xsd.AnyURI
        , license_role         :: Maybe Xsd.XsdString
        , license_show         :: Maybe Xsd.XsdString
        , license_title        :: Maybe Xsd.XsdString
        , license_type         :: Maybe Xsd.XsdString
        , license_base         :: Maybe Xsd.AnyURI
        , license_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType License where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "license-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "actuate" e pos
        a4 <- optional $ getAttribute "href" e pos
        a5 <- optional $ getAttribute "role" e pos
        a6 <- optional $ getAttribute "show" e pos
        a7 <- optional $ getAttribute "title" e pos
        a8 <- optional $ getAttribute "type" e pos
        a9 <- optional $ getAttribute "base" e pos
        a10 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (License a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    schemaTypeToXML s x@License{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ license_id x
                       , maybe [] (toXMLAttribute "license-type") $ license_license'type x
                       , maybe [] (toXMLAttribute "specific-use") $ license_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ license_actuate x
                       , maybe [] (toXMLAttribute "href") $ license_href x
                       , maybe [] (toXMLAttribute "role") $ license_role x
                       , maybe [] (toXMLAttribute "show") $ license_show x
                       , maybe [] (toXMLAttribute "title") $ license_title x
                       , maybe [] (toXMLAttribute "type") $ license_type x
                       , maybe [] (toXMLAttribute "base") $ license_base x
                       , maybe [] (toXMLAttribute "lang") $ license_lang x
                       ]
            []

elementLicense :: XMLParser License
elementLicense = parseSchemaType "license"
elementToXMLLicense :: License -> [Content ()]
elementToXMLLicense = schemaTypeToXML "license"

data License'p = License'p
        { license'p_content'type :: Maybe Xsd.XsdString
        , license'p_id           :: Maybe Xsd.ID
        , license'p_specific'use :: Maybe Xsd.XsdString
        , license'p_base         :: Maybe Xsd.AnyURI
        , license'p_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType License'p where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (License'p a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@License'p{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ license'p_content'type x
                       , maybe [] (toXMLAttribute "id") $ license'p_id x
                       , maybe [] (toXMLAttribute "specific-use") $ license'p_specific'use x
                       , maybe [] (toXMLAttribute "base") $ license'p_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ license'p_choice0 x
            ]

elementLicense'p :: XMLParser License'p
elementLicense'p = parseSchemaType "license-p"
elementToXMLLicense'p :: License'p -> [Content ()]
elementToXMLLicense'p = schemaTypeToXML "license-p"

data List = List
        { list_continued'from :: Maybe Xsd.IDREF
        , list_id             :: Maybe Xsd.ID
        , list_list'content   :: Maybe Xsd.XsdString
        , list_list'type      :: Maybe Xsd.XsdString
        , list_prefix'word    :: Maybe Xsd.XsdString
        , list_specific'use   :: Maybe Xsd.XsdString
        , list_base           :: Maybe Xsd.AnyURI
        , list_lang           :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType List where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "continued-from" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "list-content" e pos
        a3 <- optional $ getAttribute "list-type" e pos
        a4 <- optional $ getAttribute "prefix-word" e pos
        a5 <- optional $ getAttribute "specific-use" e pos
        a6 <- optional $ getAttribute "base" e pos
        a7 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (List a0 a1 a2 a3 a4 a5 a6 a7)
    schemaTypeToXML s x@List{} =
        toXMLElement s [ maybe [] (toXMLAttribute "continued-from") $ list_continued'from x
                       , maybe [] (toXMLAttribute "id") $ list_id x
                       , maybe [] (toXMLAttribute "list-content") $ list_list'content x
                       , maybe [] (toXMLAttribute "list-type") $ list_list'type x
                       , maybe [] (toXMLAttribute "prefix-word") $ list_prefix'word x
                       , maybe [] (toXMLAttribute "specific-use") $ list_specific'use x
                       , maybe [] (toXMLAttribute "base") $ list_base x
                       , maybe [] (toXMLAttribute "lang") $ list_lang x
                       ]
            []

elementList :: XMLParser List
elementList = parseSchemaType "list"
elementToXMLList :: List -> [Content ()]
elementToXMLList = schemaTypeToXML "list"

data List'item = List'item
        { list'item_id           :: Maybe Xsd.ID
        , list'item_specific'use :: Maybe Xsd.XsdString
        , list'item_base         :: Maybe Xsd.AnyURI
        , list'item_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType List'item where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (List'item a0 a1 a2 a3)
    schemaTypeToXML s x@List'item{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ list'item_id x
                       , maybe [] (toXMLAttribute "specific-use") $ list'item_specific'use x
                       , maybe [] (toXMLAttribute "base") $ list'item_base x
                       , maybe [] (toXMLAttribute "lang") $ list'item_lang x
                       ]
            []

elementList'item :: XMLParser List'item
elementList'item = parseSchemaType "list-item"
elementToXMLList'item :: List'item -> [Content ()]
elementToXMLList'item = schemaTypeToXML "list-item"

data Long'desc = Long'desc
        { long'desc_content'type :: Maybe Xsd.XsdString
        , long'desc_id           :: Maybe Xsd.ID
        , long'desc_specific'use :: Maybe Xsd.XsdString
        , long'desc_actuate      :: Maybe Xsd.XsdString
        , long'desc_href         :: Maybe Xsd.AnyURI
        , long'desc_role         :: Maybe Xsd.XsdString
        , long'desc_show         :: Maybe Xsd.XsdString
        , long'desc_title        :: Maybe Xsd.XsdString
        , long'desc_type         :: Maybe Xsd.XsdString
        , long'desc_base         :: Maybe Xsd.AnyURI
        , long'desc_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Long'desc where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "actuate" e pos
        a4 <- optional $ getAttribute "href" e pos
        a5 <- optional $ getAttribute "role" e pos
        a6 <- optional $ getAttribute "show" e pos
        a7 <- optional $ getAttribute "title" e pos
        a8 <- optional $ getAttribute "type" e pos
        a9 <- optional $ getAttribute "base" e pos
        a10 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Long'desc a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    schemaTypeToXML s x@Long'desc{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ long'desc_content'type x
                       , maybe [] (toXMLAttribute "id") $ long'desc_id x
                       , maybe [] (toXMLAttribute "specific-use") $ long'desc_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ long'desc_actuate x
                       , maybe [] (toXMLAttribute "href") $ long'desc_href x
                       , maybe [] (toXMLAttribute "role") $ long'desc_role x
                       , maybe [] (toXMLAttribute "show") $ long'desc_show x
                       , maybe [] (toXMLAttribute "title") $ long'desc_title x
                       , maybe [] (toXMLAttribute "type") $ long'desc_type x
                       , maybe [] (toXMLAttribute "base") $ long'desc_base x
                       , maybe [] (toXMLAttribute "lang") $ long'desc_lang x
                       ]
            []

elementLong'desc :: XMLParser Long'desc
elementLong'desc = parseSchemaType "long-desc"
elementToXMLLong'desc :: Long'desc -> [Content ()]
elementToXMLLong'desc = schemaTypeToXML "long-desc"

data Lpage = Lpage
        { lpage_content'type :: Maybe Xsd.XsdString
        , lpage_id           :: Maybe Xsd.ID
        , lpage_specific'use :: Maybe Xsd.XsdString
        , lpage_base         :: Maybe Xsd.AnyURI
        , lpage_lang         :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Lpage where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Lpage a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Lpage{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ lpage_content'type x
                       , maybe [] (toXMLAttribute "id") $ lpage_id x
                       , maybe [] (toXMLAttribute "specific-use") $ lpage_specific'use x
                       , maybe [] (toXMLAttribute "base") $ lpage_base x
                       , maybe [] (toXMLAttribute "lang") $ lpage_lang x
                       ]
            [
            ]

elementLpage :: XMLParser Lpage
elementLpage = parseSchemaType "lpage"
elementToXMLLpage :: Lpage -> [Content ()]
elementToXMLLpage = schemaTypeToXML "lpage"

data Media = Media
        { media_content'type :: Maybe Xsd.XsdString
        , media_id           :: Maybe Xsd.ID
        , media_mime'subtype :: Maybe Xsd.XsdString
        , media_mimetype     :: Maybe Xsd.XsdString
        , media_orientation  :: Maybe Xsd.XsdString
        , media_position     :: Maybe Xsd.XsdString
        , media_specific'use :: Maybe Xsd.XsdString
        , media_actuate      :: Maybe Xsd.XsdString
        , media_href         :: Maybe Xsd.AnyURI
        , media_role         :: Maybe Xsd.XsdString
        , media_show         :: Maybe Xsd.XsdString
        , media_title        :: Maybe Xsd.XsdString
        , media_type         :: Maybe Xsd.XsdString
        , media_base         :: Maybe Xsd.AnyURI
        , media_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Media where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "mime-subtype" e pos
        a3 <- optional $ getAttribute "mimetype" e pos
        a4 <- optional $ getAttribute "orientation" e pos
        a5 <- optional $ getAttribute "position" e pos
        a6 <- optional $ getAttribute "specific-use" e pos
        a7 <- optional $ getAttribute "actuate" e pos
        a8 <- optional $ getAttribute "href" e pos
        a9 <- optional $ getAttribute "role" e pos
        a10 <- optional $ getAttribute "show" e pos
        a11 <- optional $ getAttribute "title" e pos
        a12 <- optional $ getAttribute "type" e pos
        a13 <- optional $ getAttribute "base" e pos
        a14 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Media a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
    schemaTypeToXML s x@Media{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ media_content'type x
                       , maybe [] (toXMLAttribute "id") $ media_id x
                       , maybe [] (toXMLAttribute "mime-subtype") $ media_mime'subtype x
                       , maybe [] (toXMLAttribute "mimetype") $ media_mimetype x
                       , maybe [] (toXMLAttribute "orientation") $ media_orientation x
                       , maybe [] (toXMLAttribute "position") $ media_position x
                       , maybe [] (toXMLAttribute "specific-use") $ media_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ media_actuate x
                       , maybe [] (toXMLAttribute "href") $ media_href x
                       , maybe [] (toXMLAttribute "role") $ media_role x
                       , maybe [] (toXMLAttribute "show") $ media_show x
                       , maybe [] (toXMLAttribute "title") $ media_title x
                       , maybe [] (toXMLAttribute "type") $ media_type x
                       , maybe [] (toXMLAttribute "base") $ media_base x
                       , maybe [] (toXMLAttribute "lang") $ media_lang x
                       ]
            []

elementMedia :: XMLParser Media
elementMedia = parseSchemaType "media"
elementToXMLMedia :: Media -> [Content ()]
elementToXMLMedia = schemaTypeToXML "media"

data Meta'name = Meta'name
        { meta'name_id   :: Maybe Xsd.ID
        , meta'name_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Meta'name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Meta'name a0 a1)
    schemaTypeToXML s x@Meta'name{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ meta'name_id x
                       , maybe [] (toXMLAttribute "base") $ meta'name_base x
                       ]
            []

elementMeta'name :: XMLParser Meta'name
elementMeta'name = parseSchemaType "meta-name"
elementToXMLMeta'name :: Meta'name -> [Content ()]
elementToXMLMeta'name = schemaTypeToXML "meta-name"

data Meta'value = Meta'value
        { meta'value_id      :: Maybe Xsd.ID
        , meta'value_base    :: Maybe Xsd.AnyURI
        , meta'value_choice0 :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Meta'value where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Meta'value a0 a1)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Meta'value{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ meta'value_id x
                       , maybe [] (toXMLAttribute "base") $ meta'value_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ meta'value_choice0 x
            ]

elementMeta'value :: XMLParser Meta'value
elementMeta'value = parseSchemaType "meta-value"
elementToXMLMeta'value :: Meta'value -> [Content ()]
elementToXMLMeta'value = schemaTypeToXML "meta-value"

data Milestone'end = Milestone'end
        { milestone'end_content'type :: Maybe Xsd.XsdString
        , milestone'end_id           :: Maybe Xsd.ID
        , milestone'end_rationale    :: Maybe Xsd.XsdString
        , milestone'end_rid          :: Maybe Xsd.IDREF
        , milestone'end_specific'use :: Maybe Xsd.XsdString
        , milestone'end_base         :: Maybe Xsd.AnyURI
        , milestone'end_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Milestone'end where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "rationale" e pos
        a3 <- optional $ getAttribute "rid" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Milestone'end a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Milestone'end{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ milestone'end_content'type x
                       , maybe [] (toXMLAttribute "id") $ milestone'end_id x
                       , maybe [] (toXMLAttribute "rationale") $ milestone'end_rationale x
                       , maybe [] (toXMLAttribute "rid") $ milestone'end_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ milestone'end_specific'use x
                       , maybe [] (toXMLAttribute "base") $ milestone'end_base x
                       , maybe [] (toXMLAttribute "lang") $ milestone'end_lang x
                       ]
            []

elementMilestone'end :: XMLParser Milestone'end
elementMilestone'end = parseSchemaType "milestone-end"
elementToXMLMilestone'end :: Milestone'end -> [Content ()]
elementToXMLMilestone'end = schemaTypeToXML "milestone-end"

data Milestone'start = Milestone'start
        { milestone'start_content'type :: Maybe Xsd.XsdString
        , milestone'start_id           :: Maybe Xsd.ID
        , milestone'start_rationale    :: Maybe Xsd.XsdString
        , milestone'start_rid          :: Maybe Xsd.IDREF
        , milestone'start_specific'use :: Maybe Xsd.XsdString
        , milestone'start_base         :: Maybe Xsd.AnyURI
        , milestone'start_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Milestone'start where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "rationale" e pos
        a3 <- optional $ getAttribute "rid" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Milestone'start a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Milestone'start{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ milestone'start_content'type x
                       , maybe [] (toXMLAttribute "id") $ milestone'start_id x
                       , maybe [] (toXMLAttribute "rationale") $ milestone'start_rationale x
                       , maybe [] (toXMLAttribute "rid") $ milestone'start_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ milestone'start_specific'use x
                       , maybe [] (toXMLAttribute "base") $ milestone'start_base x
                       , maybe [] (toXMLAttribute "lang") $ milestone'start_lang x
                       ]
            []

elementMilestone'start :: XMLParser Milestone'start
elementMilestone'start = parseSchemaType "milestone-start"
elementToXMLMilestone'start :: Milestone'start -> [Content ()]
elementToXMLMilestone'start = schemaTypeToXML "milestone-start"

data Mixed'citation = Mixed'citation
        { mixed'citation_id                 :: Maybe Xsd.ID
        , mixed'citation_publication'format :: Maybe Xsd.XsdString
        , mixed'citation_publication'type   :: Maybe Xsd.XsdString
        , mixed'citation_publisher'type     :: Maybe Xsd.XsdString
        , mixed'citation_specific'use       :: Maybe Xsd.XsdString
        , mixed'citation_actuate            :: Maybe Xsd.XsdString
        , mixed'citation_href               :: Maybe Xsd.AnyURI
        , mixed'citation_role               :: Maybe Xsd.XsdString
        , mixed'citation_show               :: Maybe Xsd.XsdString
        , mixed'citation_title              :: Maybe Xsd.XsdString
        , mixed'citation_type               :: Maybe Xsd.XsdString
        , mixed'citation_base               :: Maybe Xsd.AnyURI
        , mixed'citation_lang               :: Maybe Xsd.XsdString
        , mixed'citation_choice0            :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Mixed'citation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "publication-format" e pos
        a2 <- optional $ getAttribute "publication-type" e pos
        a3 <- optional $ getAttribute "publisher-type" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "actuate" e pos
        a6 <- optional $ getAttribute "href" e pos
        a7 <- optional $ getAttribute "role" e pos
        a8 <- optional $ getAttribute "show" e pos
        a9 <- optional $ getAttribute "title" e pos
        a10 <- optional $ getAttribute "type" e pos
        a11 <- optional $ getAttribute "base" e pos
        a12 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Mixed'citation a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Mixed'citation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ mixed'citation_id x
                       , maybe [] (toXMLAttribute "publication-format") $ mixed'citation_publication'format x
                       , maybe [] (toXMLAttribute "publication-type") $ mixed'citation_publication'type x
                       , maybe [] (toXMLAttribute "publisher-type") $ mixed'citation_publisher'type x
                       , maybe [] (toXMLAttribute "specific-use") $ mixed'citation_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ mixed'citation_actuate x
                       , maybe [] (toXMLAttribute "href") $ mixed'citation_href x
                       , maybe [] (toXMLAttribute "role") $ mixed'citation_role x
                       , maybe [] (toXMLAttribute "show") $ mixed'citation_show x
                       , maybe [] (toXMLAttribute "title") $ mixed'citation_title x
                       , maybe [] (toXMLAttribute "type") $ mixed'citation_type x
                       , maybe [] (toXMLAttribute "base") $ mixed'citation_base x
                       , maybe [] (toXMLAttribute "lang") $ mixed'citation_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ mixed'citation_choice0 x
            ]

elementMixed'citation :: XMLParser Mixed'citation
elementMixed'citation = parseSchemaType "mixed-citation"
elementToXMLMixed'citation :: Mixed'citation -> [Content ()]
elementToXMLMixed'citation = schemaTypeToXML "mixed-citation"

data Monospace = Monospace
        { monospace_id           :: Maybe Xsd.ID
        , monospace_specific'use :: Maybe Xsd.XsdString
        , monospace_toggle       :: Maybe Xsd.XsdString
        , monospace_base         :: Maybe Xsd.AnyURI
        , monospace_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Monospace where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "toggle" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Monospace a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Monospace{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ monospace_id x
                       , maybe [] (toXMLAttribute "specific-use") $ monospace_specific'use x
                       , maybe [] (toXMLAttribute "toggle") $ monospace_toggle x
                       , maybe [] (toXMLAttribute "base") $ monospace_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ monospace_choice0 x
            ]

elementMonospace :: XMLParser Monospace
elementMonospace = parseSchemaType "monospace"
elementToXMLMonospace :: Monospace -> [Content ()]
elementToXMLMonospace = schemaTypeToXML "monospace"

data Month = Month
        { month_content'type :: Maybe Xsd.XsdString
        , month_id           :: Maybe Xsd.ID
        , month_specific'use :: Maybe Xsd.XsdString
        , month_base         :: Maybe Xsd.AnyURI
        , month_lang         :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Month where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Month a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Month{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ month_content'type x
                       , maybe [] (toXMLAttribute "id") $ month_id x
                       , maybe [] (toXMLAttribute "specific-use") $ month_specific'use x
                       , maybe [] (toXMLAttribute "base") $ month_base x
                       , maybe [] (toXMLAttribute "lang") $ month_lang x
                       ]
            [
            ]

elementMonth :: XMLParser Month
elementMonth = parseSchemaType "month"
elementToXMLMonth :: Month -> [Content ()]
elementToXMLMonth = schemaTypeToXML "month"

data Name = Name
        { name_content'type :: Maybe Xsd.XsdString
        , name_id :: Maybe Xsd.ID
        , name_name'style :: Maybe Xsd.XsdString
        , name_specific'use :: Maybe Xsd.XsdString
        , name_base :: Maybe Xsd.AnyURI
        , name_lang :: Maybe Xsd.XsdString
        , name_choice0 :: OneOf2 (Surname,(Maybe (Given'names))) Given'names
          -- ^ Choice between:
          --
          --   (1) Sequence of:
          --
          --     * surname
          --
          --     * given-names
          --
          --   (2) given-names
        , name_prefix :: Maybe Prefix
        , name_suffix :: Maybe Suffix
        }
        deriving (Eq,Show)
instance SchemaType Name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "name-style" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Name a0 a1 a2 a3 a4 a5)
            `apply` oneOf' [ ("Surname Maybe Given'names", fmap OneOf2 (return (,) `apply` elementSurname
                                                                                   `apply` optional (elementGiven'names)))
                           , ("Given'names", fmap TwoOf2 (elementGiven'names))
                           ]
            `apply` optional (elementPrefix)
            `apply` optional (elementSuffix)
    schemaTypeToXML s x@Name{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ name_content'type x
                       , maybe [] (toXMLAttribute "id") $ name_id x
                       , maybe [] (toXMLAttribute "name-style") $ name_name'style x
                       , maybe [] (toXMLAttribute "specific-use") $ name_specific'use x
                       , maybe [] (toXMLAttribute "base") $ name_base x
                       , maybe [] (toXMLAttribute "lang") $ name_lang x
                       ]
            [ foldOneOf2  (\ (a,b) -> concat [ elementToXMLSurname a
                                             , maybe [] (elementToXMLGiven'names) b
                                             ])
                          (elementToXMLGiven'names)
                          $ name_choice0 x
            , maybe [] (elementToXMLPrefix) $ name_prefix x
            , maybe [] (elementToXMLSuffix) $ name_suffix x
            ]

elementName :: XMLParser Name
elementName = parseSchemaType "name"
elementToXMLName :: Name -> [Content ()]
elementToXMLName = schemaTypeToXML "name"

data Name'alternatives = Name'alternatives
        { name'alternatives_id   :: Maybe Xsd.ID
        , name'alternatives_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Name'alternatives where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Name'alternatives a0 a1)
    schemaTypeToXML s x@Name'alternatives{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ name'alternatives_id x
                       , maybe [] (toXMLAttribute "base") $ name'alternatives_base x
                       ]
            []

elementName'alternatives :: XMLParser Name'alternatives
elementName'alternatives = parseSchemaType "name-alternatives"
elementToXMLName'alternatives :: Name'alternatives -> [Content ()]
elementToXMLName'alternatives = schemaTypeToXML "name-alternatives"

data Named'content = Named'content
        { named'content_alt          :: Maybe Xsd.XsdString
        , named'content_content'type :: Xsd.XsdString
        , named'content_id           :: Maybe Xsd.ID
        , named'content_rid          :: Maybe Xsd.IDREFS
        , named'content_specific'use :: Maybe Xsd.XsdString
        , named'content_actuate      :: Maybe Xsd.XsdString
        , named'content_href         :: Maybe Xsd.AnyURI
        , named'content_role         :: Maybe Xsd.XsdString
        , named'content_show         :: Maybe Xsd.XsdString
        , named'content_title        :: Maybe Xsd.XsdString
        , named'content_type         :: Maybe Xsd.XsdString
        , named'content_base         :: Maybe Xsd.AnyURI
        , named'content_lang         :: Maybe Xsd.XsdString
        , named'content_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Named'content where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "alt" e pos
        a1 <- getAttribute "content-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "rid" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "actuate" e pos
        a6 <- optional $ getAttribute "href" e pos
        a7 <- optional $ getAttribute "role" e pos
        a8 <- optional $ getAttribute "show" e pos
        a9 <- optional $ getAttribute "title" e pos
        a10 <- optional $ getAttribute "type" e pos
        a11 <- optional $ getAttribute "base" e pos
        a12 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Named'content a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Named'content{} =
        toXMLElement s [ maybe [] (toXMLAttribute "alt") $ named'content_alt x
                       , toXMLAttribute "content-type" $ named'content_content'type x
                       , maybe [] (toXMLAttribute "id") $ named'content_id x
                       , maybe [] (toXMLAttribute "rid") $ named'content_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ named'content_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ named'content_actuate x
                       , maybe [] (toXMLAttribute "href") $ named'content_href x
                       , maybe [] (toXMLAttribute "role") $ named'content_role x
                       , maybe [] (toXMLAttribute "show") $ named'content_show x
                       , maybe [] (toXMLAttribute "title") $ named'content_title x
                       , maybe [] (toXMLAttribute "type") $ named'content_type x
                       , maybe [] (toXMLAttribute "base") $ named'content_base x
                       , maybe [] (toXMLAttribute "lang") $ named'content_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ named'content_choice0 x
            ]

elementNamed'content :: XMLParser Named'content
elementNamed'content = parseSchemaType "named-content"
elementToXMLNamed'content :: Named'content -> [Content ()]
elementToXMLNamed'content = schemaTypeToXML "named-content"

data Nested'kwd = Nested'kwd
        { nested'kwd_content'type :: Maybe Xsd.XsdString
        , nested'kwd_id           :: Maybe Xsd.ID
        , nested'kwd_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Nested'kwd where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Nested'kwd a0 a1 a2)
    schemaTypeToXML s x@Nested'kwd{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ nested'kwd_content'type x
                       , maybe [] (toXMLAttribute "id") $ nested'kwd_id x
                       , maybe [] (toXMLAttribute "base") $ nested'kwd_base x
                       ]
            []

elementNested'kwd :: XMLParser Nested'kwd
elementNested'kwd = parseSchemaType "nested-kwd"
elementToXMLNested'kwd :: Nested'kwd -> [Content ()]
elementToXMLNested'kwd = schemaTypeToXML "nested-kwd"

data Nlm'citation = Nlm'citation
        { nlm'citation_id :: Maybe Xsd.ID
        , nlm'citation_publication'format :: Maybe Xsd.XsdString
        , nlm'citation_publication'type :: Maybe Xsd.XsdString
        , nlm'citation_publisher'type :: Maybe Xsd.XsdString
        , nlm'citation_specific'use :: Maybe Xsd.XsdString
        , nlm'citation_actuate :: Maybe Xsd.XsdString
        , nlm'citation_href :: Maybe Xsd.AnyURI
        , nlm'citation_role :: Maybe Xsd.XsdString
        , nlm'citation_show :: Maybe Xsd.XsdString
        , nlm'citation_title :: Maybe Xsd.XsdString
        , nlm'citation_type :: Maybe Xsd.XsdString
        , nlm'citation_base :: Maybe Xsd.AnyURI
        , nlm'citation_lang :: Maybe Xsd.XsdString
        , nlm'citation_choice0 :: [OneOf2 Person'group Collab]
          -- ^ Choice between:
          --
          --   (1) person-group
          --
          --   (2) collab
        , nlm'citation_choice1 :: [OneOf2 Article'title Trans'title]
          -- ^ Choice between:
          --
          --   (1) article-title
          --
          --   (2) trans-title
        , nlm'citation_source :: Maybe Source
        , nlm'citation_patent :: Maybe Patent
        , nlm'citation_trans'source :: Maybe Trans'source
        , nlm'citation_year :: Maybe Year
        , nlm'citation_choice6 :: (Maybe (OneOf2 ((Maybe (Month)),(Maybe (Day)),(Maybe (Time'stamp))) (Maybe (Season))))
          -- ^ Choice between:
          --
          --   (1) Sequence of:
          --
          --     * month
          --
          --     * day
          --
          --     * time-stamp
          --
          --   (2) season
        , nlm'citation_access'date :: Maybe Access'date
        , nlm'citation_volume :: Maybe Volume
        , nlm'citation_edition :: Maybe Edition
        , nlm'citation_conf'name :: Maybe Conf'name
        , nlm'citation_conf'date :: Maybe Conf'date
        , nlm'citation_conf'loc :: Maybe Conf'loc
        , nlm'citation_choice13 :: [OneOf2 Issue Supplement]
          -- ^ Choice between:
          --
          --   (1) issue
          --
          --   (2) supplement
        , nlm'citation_publisher'loc :: Maybe Publisher'loc
        , nlm'citation_publisher'name :: Maybe Publisher'name
        , nlm'citation_fpage :: Maybe Fpage
        , nlm'citation_lpage :: Maybe Lpage
        , nlm'citation_page'count :: Maybe Page'count
        , nlm'citation_series :: Maybe Series
        , nlm'citation_comment :: [Comment]
        , nlm'citation_pub'id :: [Pub'id]
        , nlm'citation_annotation :: Maybe Annotation
        }
        deriving (Eq,Show)
instance SchemaType Nlm'citation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "publication-format" e pos
        a2 <- optional $ getAttribute "publication-type" e pos
        a3 <- optional $ getAttribute "publisher-type" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "actuate" e pos
        a6 <- optional $ getAttribute "href" e pos
        a7 <- optional $ getAttribute "role" e pos
        a8 <- optional $ getAttribute "show" e pos
        a9 <- optional $ getAttribute "title" e pos
        a10 <- optional $ getAttribute "type" e pos
        a11 <- optional $ getAttribute "base" e pos
        a12 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Nlm'citation a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            `apply` many (oneOf' [ ("Person'group", fmap OneOf2 (elementPerson'group))
                                 , ("Collab", fmap TwoOf2 (elementCollab))
                                 ])
            `apply` many (oneOf' [ ("Article'title", fmap OneOf2 (elementArticle'title))
                                 , ("Trans'title", fmap TwoOf2 (elementTrans'title))
                                 ])
            `apply` optional (elementSource)
            `apply` optional (elementPatent)
            `apply` optional (elementTrans'source)
            `apply` optional (elementYear)
            `apply` optional (oneOf' [ ("Maybe Month Maybe Day Maybe Time'stamp", fmap OneOf2 (return (,,) `apply` optional (elementMonth)
                                                                                                           `apply` optional (elementDay)
                                                                                                           `apply` optional (elementTime'stamp)))
                                     , ("Maybe Season", fmap TwoOf2 (optional (elementSeason)))
                                     ])
            `apply` optional (elementAccess'date)
            `apply` optional (elementVolume)
            `apply` optional (elementEdition)
            `apply` optional (elementConf'name)
            `apply` optional (elementConf'date)
            `apply` optional (elementConf'loc)
            `apply` many (oneOf' [ ("Issue", fmap OneOf2 (elementIssue))
                                 , ("Supplement", fmap TwoOf2 (elementSupplement))
                                 ])
            `apply` optional (elementPublisher'loc)
            `apply` optional (elementPublisher'name)
            `apply` optional (elementFpage)
            `apply` optional (elementLpage)
            `apply` optional (elementPage'count)
            `apply` optional (elementSeries)
            `apply` many (elementComment)
            `apply` many (elementPub'id)
            `apply` optional (elementAnnotation)
    schemaTypeToXML s x@Nlm'citation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ nlm'citation_id x
                       , maybe [] (toXMLAttribute "publication-format") $ nlm'citation_publication'format x
                       , maybe [] (toXMLAttribute "publication-type") $ nlm'citation_publication'type x
                       , maybe [] (toXMLAttribute "publisher-type") $ nlm'citation_publisher'type x
                       , maybe [] (toXMLAttribute "specific-use") $ nlm'citation_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ nlm'citation_actuate x
                       , maybe [] (toXMLAttribute "href") $ nlm'citation_href x
                       , maybe [] (toXMLAttribute "role") $ nlm'citation_role x
                       , maybe [] (toXMLAttribute "show") $ nlm'citation_show x
                       , maybe [] (toXMLAttribute "title") $ nlm'citation_title x
                       , maybe [] (toXMLAttribute "type") $ nlm'citation_type x
                       , maybe [] (toXMLAttribute "base") $ nlm'citation_base x
                       , maybe [] (toXMLAttribute "lang") $ nlm'citation_lang x
                       ]
            [ concatMap (foldOneOf2  (elementToXMLPerson'group)
                                     (elementToXMLCollab)
                                    ) $ nlm'citation_choice0 x
            , concatMap (foldOneOf2  (elementToXMLArticle'title)
                                     (elementToXMLTrans'title)
                                    ) $ nlm'citation_choice1 x
            , maybe [] (elementToXMLSource) $ nlm'citation_source x
            , maybe [] (elementToXMLPatent) $ nlm'citation_patent x
            , maybe [] (elementToXMLTrans'source) $ nlm'citation_trans'source x
            , maybe [] (elementToXMLYear) $ nlm'citation_year x
            , maybe [] (foldOneOf2  (\ (a,b,c) -> concat [ maybe [] (elementToXMLMonth) a
                                                         , maybe [] (elementToXMLDay) b
                                                         , maybe [] (elementToXMLTime'stamp) c
                                                         ])
                                    (maybe [] (elementToXMLSeason))
                                   ) $ nlm'citation_choice6 x
            , maybe [] (elementToXMLAccess'date) $ nlm'citation_access'date x
            , maybe [] (elementToXMLVolume) $ nlm'citation_volume x
            , maybe [] (elementToXMLEdition) $ nlm'citation_edition x
            , maybe [] (elementToXMLConf'name) $ nlm'citation_conf'name x
            , maybe [] (elementToXMLConf'date) $ nlm'citation_conf'date x
            , maybe [] (elementToXMLConf'loc) $ nlm'citation_conf'loc x
            , concatMap (foldOneOf2  (elementToXMLIssue)
                                     (elementToXMLSupplement)
                                    ) $ nlm'citation_choice13 x
            , maybe [] (elementToXMLPublisher'loc) $ nlm'citation_publisher'loc x
            , maybe [] (elementToXMLPublisher'name) $ nlm'citation_publisher'name x
            , maybe [] (elementToXMLFpage) $ nlm'citation_fpage x
            , maybe [] (elementToXMLLpage) $ nlm'citation_lpage x
            , maybe [] (elementToXMLPage'count) $ nlm'citation_page'count x
            , maybe [] (elementToXMLSeries) $ nlm'citation_series x
            , concatMap (elementToXMLComment) $ nlm'citation_comment x
            , concatMap (elementToXMLPub'id) $ nlm'citation_pub'id x
            , maybe [] (elementToXMLAnnotation) $ nlm'citation_annotation x
            ]

elementNlm'citation :: XMLParser Nlm'citation
elementNlm'citation = parseSchemaType "nlm-citation"
elementToXMLNlm'citation :: Nlm'citation -> [Content ()]
elementToXMLNlm'citation = schemaTypeToXML "nlm-citation"

data Note = Note
        { note_content'type :: Maybe Xsd.XsdString
        , note_id           :: Maybe Xsd.ID
        , note_specific'use :: Maybe Xsd.XsdString
        , note_base         :: Maybe Xsd.AnyURI
        , note_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Note where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Note a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Note{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ note_content'type x
                       , maybe [] (toXMLAttribute "id") $ note_id x
                       , maybe [] (toXMLAttribute "specific-use") $ note_specific'use x
                       , maybe [] (toXMLAttribute "base") $ note_base x
                       , maybe [] (toXMLAttribute "lang") $ note_lang x
                       ]
            []

elementNote :: XMLParser Note
elementNote = parseSchemaType "note"
elementToXMLNote :: Note -> [Content ()]
elementToXMLNote = schemaTypeToXML "note"

data Notes = Notes
        { notes_id           :: Maybe Xsd.ID
        , notes_notes'type   :: Maybe Xsd.XsdString
        , notes_specific'use :: Maybe Xsd.XsdString
        , notes_base         :: Maybe Xsd.AnyURI
        , notes_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Notes where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "notes-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Notes a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Notes{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ notes_id x
                       , maybe [] (toXMLAttribute "notes-type") $ notes_notes'type x
                       , maybe [] (toXMLAttribute "specific-use") $ notes_specific'use x
                       , maybe [] (toXMLAttribute "base") $ notes_base x
                       , maybe [] (toXMLAttribute "lang") $ notes_lang x
                       ]
            []

elementNotes :: XMLParser Notes
elementNotes = parseSchemaType "notes"
elementToXMLNotes :: Notes -> [Content ()]
elementToXMLNotes = schemaTypeToXML "notes"

data Object'id = Object'id
        { object'id_content'type :: Maybe Xsd.XsdString
        , object'id_id           :: Maybe Xsd.ID
        , object'id_pub'id'type  :: Maybe Xsd.XsdString
        , object'id_specific'use :: Maybe Xsd.XsdString
        , object'id_base         :: Maybe Xsd.AnyURI
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Object'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "pub-id-type" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Object'id a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Object'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ object'id_content'type x
                       , maybe [] (toXMLAttribute "id") $ object'id_id x
                       , maybe [] (toXMLAttribute "pub-id-type") $ object'id_pub'id'type x
                       , maybe [] (toXMLAttribute "specific-use") $ object'id_specific'use x
                       , maybe [] (toXMLAttribute "base") $ object'id_base x
                       ]
            [
            ]

elementObject'id :: XMLParser Object'id
elementObject'id = parseSchemaType "object-id"
elementToXMLObject'id :: Object'id -> [Content ()]
elementToXMLObject'id = schemaTypeToXML "object-id"

data On'behalf'of = On'behalf'of
        { on'behalf'of_id           :: Maybe Xsd.ID
        , on'behalf'of_specific'use :: Maybe Xsd.XsdString
        , on'behalf'of_base         :: Maybe Xsd.AnyURI
        , on'behalf'of_lang         :: Maybe Xsd.XsdString
        , on'behalf'of_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType On'behalf'of where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (On'behalf'of a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@On'behalf'of{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ on'behalf'of_id x
                       , maybe [] (toXMLAttribute "specific-use") $ on'behalf'of_specific'use x
                       , maybe [] (toXMLAttribute "base") $ on'behalf'of_base x
                       , maybe [] (toXMLAttribute "lang") $ on'behalf'of_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ on'behalf'of_choice0 x
            ]

elementOn'behalf'of :: XMLParser On'behalf'of
elementOn'behalf'of = parseSchemaType "on-behalf-of"
elementToXMLOn'behalf'of :: On'behalf'of -> [Content ()]
elementToXMLOn'behalf'of = schemaTypeToXML "on-behalf-of"

data Open'access = Open'access
        { open'access_id           :: Maybe Xsd.ID
        , open'access_specific'use :: Maybe Xsd.XsdString
        , open'access_base         :: Maybe Xsd.AnyURI
        , open'access_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Open'access where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Open'access a0 a1 a2 a3)
    schemaTypeToXML s x@Open'access{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ open'access_id x
                       , maybe [] (toXMLAttribute "specific-use") $ open'access_specific'use x
                       , maybe [] (toXMLAttribute "base") $ open'access_base x
                       , maybe [] (toXMLAttribute "lang") $ open'access_lang x
                       ]
            []

elementOpen'access :: XMLParser Open'access
elementOpen'access = parseSchemaType "open-access"
elementToXMLOpen'access :: Open'access -> [Content ()]
elementToXMLOpen'access = schemaTypeToXML "open-access"

data Overline = Overline
        { overline_id           :: Maybe Xsd.ID
        , overline_specific'use :: Maybe Xsd.XsdString
        , overline_toggle       :: Maybe Xsd.XsdString
        , overline_base         :: Maybe Xsd.AnyURI
        , overline_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Overline where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "toggle" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Overline a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Overline{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ overline_id x
                       , maybe [] (toXMLAttribute "specific-use") $ overline_specific'use x
                       , maybe [] (toXMLAttribute "toggle") $ overline_toggle x
                       , maybe [] (toXMLAttribute "base") $ overline_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ overline_choice0 x
            ]

elementOverline :: XMLParser Overline
elementOverline = parseSchemaType "overline"
elementToXMLOverline :: Overline -> [Content ()]
elementToXMLOverline = schemaTypeToXML "overline"

data Overline'end = Overline'end
        { overline'end_id           :: Maybe Xsd.ID
        , overline'end_rid          :: Xsd.IDREF
        , overline'end_specific'use :: Maybe Xsd.XsdString
        , overline'end_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Overline'end where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- getAttribute "rid" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Overline'end a0 a1 a2 a3)
    schemaTypeToXML s x@Overline'end{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ overline'end_id x
                       , toXMLAttribute "rid" $ overline'end_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ overline'end_specific'use x
                       , maybe [] (toXMLAttribute "base") $ overline'end_base x
                       ]
            []

elementOverline'end :: XMLParser Overline'end
elementOverline'end = parseSchemaType "overline-end"
elementToXMLOverline'end :: Overline'end -> [Content ()]
elementToXMLOverline'end = schemaTypeToXML "overline-end"

data Overline'start = Overline'start
        { overline'start_id           :: Xsd.ID
        , overline'start_specific'use :: Maybe Xsd.XsdString
        , overline'start_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Overline'start where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Overline'start a0 a1 a2)
    schemaTypeToXML s x@Overline'start{} =
        toXMLElement s [ toXMLAttribute "id" $ overline'start_id x
                       , maybe [] (toXMLAttribute "specific-use") $ overline'start_specific'use x
                       , maybe [] (toXMLAttribute "base") $ overline'start_base x
                       ]
            []

elementOverline'start :: XMLParser Overline'start
elementOverline'start = parseSchemaType "overline-start"
elementToXMLOverline'start :: Overline'start -> [Content ()]
elementToXMLOverline'start = schemaTypeToXML "overline-start"

data P = P
        { p_content'type :: Maybe Xsd.XsdString
        , p_id           :: Maybe Xsd.ID
        , p_specific'use :: Maybe Xsd.XsdString
        , p_base         :: Maybe Xsd.AnyURI
        , p_lang         :: Maybe Xsd.XsdString
        , p_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType P where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Text.JaTex.JatsElements.P a0 a1 a2 a3 a4)
            `apply` many (oneOf' [
                                 ])
    schemaTypeToXML s x@Text.JaTex.JatsElements.P{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ p_content'type x
                       , maybe [] (toXMLAttribute "id") $ p_id x
                       , maybe [] (toXMLAttribute "specific-use") $ p_specific'use x
                       , maybe [] (toXMLAttribute "base") $ p_base x
                       , maybe [] (toXMLAttribute "lang") $ p_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ p_choice0 x
            ]

elementP :: XMLParser P
elementP = parseSchemaType "p"
elementToXMLP :: P -> [Content ()]
elementToXMLP = schemaTypeToXML "p"

data Page'count = Page'count
        { page'count_count :: Xsd.NMTOKEN
        , page'count_id    :: Maybe Xsd.ID
        , page'count_base  :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Page'count where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "count" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Page'count a0 a1 a2)
    schemaTypeToXML s x@Page'count{} =
        toXMLElement s [ toXMLAttribute "count" $ page'count_count x
                       , maybe [] (toXMLAttribute "id") $ page'count_id x
                       , maybe [] (toXMLAttribute "base") $ page'count_base x
                       ]
            []

elementPage'count :: XMLParser Page'count
elementPage'count = parseSchemaType "page-count"
elementToXMLPage'count :: Page'count -> [Content ()]
elementToXMLPage'count = schemaTypeToXML "page-count"

data Page'range = Page'range
        { page'range_content'type :: Maybe Xsd.XsdString
        , page'range_id           :: Maybe Xsd.ID
        , page'range_specific'use :: Maybe Xsd.XsdString
        , page'range_base         :: Maybe Xsd.AnyURI
        , page'range_lang         :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Page'range where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Page'range a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Page'range{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ page'range_content'type x
                       , maybe [] (toXMLAttribute "id") $ page'range_id x
                       , maybe [] (toXMLAttribute "specific-use") $ page'range_specific'use x
                       , maybe [] (toXMLAttribute "base") $ page'range_base x
                       , maybe [] (toXMLAttribute "lang") $ page'range_lang x
                       ]
            [
            ]

elementPage'range :: XMLParser Page'range
elementPage'range = parseSchemaType "page-range"
elementToXMLPage'range :: Page'range -> [Content ()]
elementToXMLPage'range = schemaTypeToXML "page-range"

data Part'title = Part'title
        { part'title_id           :: Maybe Xsd.ID
        , part'title_specific'use :: Maybe Xsd.XsdString
        , part'title_base         :: Maybe Xsd.AnyURI
        , part'title_lang         :: Maybe Xsd.XsdString
        , part'title_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Part'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Part'title a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Part'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ part'title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ part'title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ part'title_base x
                       , maybe [] (toXMLAttribute "lang") $ part'title_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ part'title_choice0 x
            ]

elementPart'title :: XMLParser Part'title
elementPart'title = parseSchemaType "part-title"
elementToXMLPart'title :: Part'title -> [Content ()]
elementToXMLPart'title = schemaTypeToXML "part-title"

data Patent = Patent
        { patent_content'type :: Maybe Xsd.XsdString
        , patent_country      :: Maybe Xsd.XsdString
        , patent_id           :: Maybe Xsd.ID
        , patent_specific'use :: Maybe Xsd.XsdString
        , patent_base         :: Maybe Xsd.AnyURI
        , patent_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Patent where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "country" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Patent a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Patent{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ patent_content'type x
                       , maybe [] (toXMLAttribute "country") $ patent_country x
                       , maybe [] (toXMLAttribute "id") $ patent_id x
                       , maybe [] (toXMLAttribute "specific-use") $ patent_specific'use x
                       , maybe [] (toXMLAttribute "base") $ patent_base x
                       , maybe [] (toXMLAttribute "lang") $ patent_lang x
                       ]
            []

elementPatent :: XMLParser Patent
elementPatent = parseSchemaType "patent"
elementToXMLPatent :: Patent -> [Content ()]
elementToXMLPatent = schemaTypeToXML "patent"

data Permissions = Permissions
        { permissions_id   :: Maybe Xsd.ID
        , permissions_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Permissions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Permissions a0 a1)
    schemaTypeToXML s x@Permissions{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ permissions_id x
                       , maybe [] (toXMLAttribute "base") $ permissions_base x
                       ]
            []

elementPermissions :: XMLParser Permissions
elementPermissions = parseSchemaType "permissions"
elementToXMLPermissions :: Permissions -> [Content ()]
elementToXMLPermissions = schemaTypeToXML "permissions"

data Person'group = Person'group
        { person'group_id                :: Maybe Xsd.ID
        , person'group_person'group'type :: Maybe Xsd.XsdString
        , person'group_specific'use      :: Maybe Xsd.XsdString
        , person'group_base              :: Maybe Xsd.AnyURI
        , person'group_lang              :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Person'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "person-group-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Person'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Person'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ person'group_id x
                       , maybe [] (toXMLAttribute "person-group-type") $ person'group_person'group'type x
                       , maybe [] (toXMLAttribute "specific-use") $ person'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ person'group_base x
                       , maybe [] (toXMLAttribute "lang") $ person'group_lang x
                       ]
            []

elementPerson'group :: XMLParser Person'group
elementPerson'group = parseSchemaType "person-group"
elementToXMLPerson'group :: Person'group -> [Content ()]
elementToXMLPerson'group = schemaTypeToXML "person-group"

data Phone = Phone
        { phone_content'type :: Maybe Xsd.XsdString
        , phone_id           :: Maybe Xsd.ID
        , phone_specific'use :: Maybe Xsd.XsdString
        , phone_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Phone where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Phone a0 a1 a2 a3)
    schemaTypeToXML s x@Phone{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ phone_content'type x
                       , maybe [] (toXMLAttribute "id") $ phone_id x
                       , maybe [] (toXMLAttribute "specific-use") $ phone_specific'use x
                       , maybe [] (toXMLAttribute "base") $ phone_base x
                       ]
            []

elementPhone :: XMLParser Phone
elementPhone = parseSchemaType "phone"
elementToXMLPhone :: Phone -> [Content ()]
elementToXMLPhone = schemaTypeToXML "phone"

data Postal'code = Postal'code
        { postal'code_content'type :: Maybe Xsd.XsdString
        , postal'code_id           :: Maybe Xsd.ID
        , postal'code_specific'use :: Maybe Xsd.XsdString
        , postal'code_base         :: Maybe Xsd.AnyURI
        , postal'code_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Postal'code where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Postal'code a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Postal'code{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ postal'code_content'type x
                       , maybe [] (toXMLAttribute "id") $ postal'code_id x
                       , maybe [] (toXMLAttribute "specific-use") $ postal'code_specific'use x
                       , maybe [] (toXMLAttribute "base") $ postal'code_base x
                       , maybe [] (toXMLAttribute "lang") $ postal'code_lang x
                       ]
            []

elementPostal'code :: XMLParser Postal'code
elementPostal'code = parseSchemaType "postal-code"
elementToXMLPostal'code :: Postal'code -> [Content ()]
elementToXMLPostal'code = schemaTypeToXML "postal-code"

data Prefix = Prefix
        { prefix_content'type :: Maybe Xsd.XsdString
        , prefix_id           :: Maybe Xsd.ID
        , prefix_specific'use :: Maybe Xsd.XsdString
        , prefix_base         :: Maybe Xsd.AnyURI
        , prefix_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Prefix where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Prefix a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Prefix{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ prefix_content'type x
                       , maybe [] (toXMLAttribute "id") $ prefix_id x
                       , maybe [] (toXMLAttribute "specific-use") $ prefix_specific'use x
                       , maybe [] (toXMLAttribute "base") $ prefix_base x
                       , maybe [] (toXMLAttribute "lang") $ prefix_lang x
                       ]
            []

elementPrefix :: XMLParser Prefix
elementPrefix = parseSchemaType "prefix"
elementToXMLPrefix :: Prefix -> [Content ()]
elementToXMLPrefix = schemaTypeToXML "prefix"

data Preformat = Preformat
        { preformat_id             :: Maybe Xsd.ID
        , preformat_orientation    :: Maybe Xsd.XsdString
        , preformat_position       :: Maybe Xsd.XsdString
        , preformat_preformat'type :: Maybe Xsd.XsdString
        , preformat_specific'use   :: Maybe Xsd.XsdString
        , preformat_base           :: Maybe Xsd.AnyURI
        , preformat_lang           :: Maybe Xsd.XsdString
        , preformat_space          :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Preformat where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "orientation" e pos
        a2 <- optional $ getAttribute "position" e pos
        a3 <- optional $ getAttribute "preformat-type" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        a7 <- optional $ getAttribute "space" e pos
        commit $ interior e $ return (Preformat a0 a1 a2 a3 a4 a5 a6 a7)
    schemaTypeToXML s x@Preformat{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ preformat_id x
                       , maybe [] (toXMLAttribute "orientation") $ preformat_orientation x
                       , maybe [] (toXMLAttribute "position") $ preformat_position x
                       , maybe [] (toXMLAttribute "preformat-type") $ preformat_preformat'type x
                       , maybe [] (toXMLAttribute "specific-use") $ preformat_specific'use x
                       , maybe [] (toXMLAttribute "base") $ preformat_base x
                       , maybe [] (toXMLAttribute "lang") $ preformat_lang x
                       , maybe [] (toXMLAttribute "space") $ preformat_space x
                       ]
            []

elementPreformat :: XMLParser Preformat
elementPreformat = parseSchemaType "preformat"
elementToXMLPreformat :: Preformat -> [Content ()]
elementToXMLPreformat = schemaTypeToXML "preformat"

data Price = Price
        { price_content'type :: Maybe Xsd.XsdString
        , price_currency     :: Maybe Xsd.XsdString
        , price_id           :: Maybe Xsd.ID
        , price_specific'use :: Maybe Xsd.XsdString
        , price_base         :: Maybe Xsd.AnyURI
        , price_lang         :: Maybe Xsd.XsdString
        , price_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "currency" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Price a0 a1 a2 a3 a4 a5)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Price{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ price_content'type x
                       , maybe [] (toXMLAttribute "currency") $ price_currency x
                       , maybe [] (toXMLAttribute "id") $ price_id x
                       , maybe [] (toXMLAttribute "specific-use") $ price_specific'use x
                       , maybe [] (toXMLAttribute "base") $ price_base x
                       , maybe [] (toXMLAttribute "lang") $ price_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ price_choice0 x
            ]

elementPrice :: XMLParser Price
elementPrice = parseSchemaType "price"
elementToXMLPrice :: Price -> [Content ()]
elementToXMLPrice = schemaTypeToXML "price"

data Principal'award'recipient = Principal'award'recipient
        { principal'award'recipient_id           :: Maybe Xsd.ID
        , principal'award'recipient_specific'use :: Maybe Xsd.XsdString
        , principal'award'recipient_base         :: Maybe Xsd.AnyURI
        , principal'award'recipient_lang         :: Maybe Xsd.XsdString
        , principal'award'recipient_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Principal'award'recipient where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Principal'award'recipient a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Principal'award'recipient{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ principal'award'recipient_id x
                       , maybe [] (toXMLAttribute "specific-use") $ principal'award'recipient_specific'use x
                       , maybe [] (toXMLAttribute "base") $ principal'award'recipient_base x
                       , maybe [] (toXMLAttribute "lang") $ principal'award'recipient_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ principal'award'recipient_choice0 x
            ]

elementPrincipal'award'recipient :: XMLParser Principal'award'recipient
elementPrincipal'award'recipient = parseSchemaType "principal-award-recipient"
elementToXMLPrincipal'award'recipient :: Principal'award'recipient -> [Content ()]
elementToXMLPrincipal'award'recipient = schemaTypeToXML "principal-award-recipient"

data Principal'investigator = Principal'investigator
        { principal'investigator_id           :: Maybe Xsd.ID
        , principal'investigator_specific'use :: Maybe Xsd.XsdString
        , principal'investigator_base         :: Maybe Xsd.AnyURI
        , principal'investigator_lang         :: Maybe Xsd.XsdString
        , principal'investigator_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Principal'investigator where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Principal'investigator a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Principal'investigator{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ principal'investigator_id x
                       , maybe [] (toXMLAttribute "specific-use") $ principal'investigator_specific'use x
                       , maybe [] (toXMLAttribute "base") $ principal'investigator_base x
                       , maybe [] (toXMLAttribute "lang") $ principal'investigator_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ principal'investigator_choice0 x
            ]

elementPrincipal'investigator :: XMLParser Principal'investigator
elementPrincipal'investigator = parseSchemaType "principal-investigator"
elementToXMLPrincipal'investigator :: Principal'investigator -> [Content ()]
elementToXMLPrincipal'investigator = schemaTypeToXML "principal-investigator"

data Private'char = Private'char
        { private'char_description  :: Maybe Xsd.XsdString
        , private'char_id           :: Maybe Xsd.ID
        , private'char_name         :: Maybe Xsd.XsdString
        , private'char_specific'use :: Maybe Xsd.XsdString
        , private'char_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Private'char where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "description" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "name" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Private'char a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Private'char{} =
        toXMLElement s [ maybe [] (toXMLAttribute "description") $ private'char_description x
                       , maybe [] (toXMLAttribute "id") $ private'char_id x
                       , maybe [] (toXMLAttribute "name") $ private'char_name x
                       , maybe [] (toXMLAttribute "specific-use") $ private'char_specific'use x
                       , maybe [] (toXMLAttribute "base") $ private'char_base x
                       ]
            []

elementPrivate'char :: XMLParser Private'char
elementPrivate'char = parseSchemaType "private-char"
elementToXMLPrivate'char :: Private'char -> [Content ()]
elementToXMLPrivate'char = schemaTypeToXML "private-char"

data Product = Product
        { product_id           :: Maybe Xsd.ID
        , product_product'type :: Maybe Xsd.XsdString
        , product_specific'use :: Maybe Xsd.XsdString
        , product_actuate      :: Maybe Xsd.XsdString
        , product_href         :: Maybe Xsd.AnyURI
        , product_role         :: Maybe Xsd.XsdString
        , product_show         :: Maybe Xsd.XsdString
        , product_title        :: Maybe Xsd.XsdString
        , product_type         :: Maybe Xsd.XsdString
        , product_base         :: Maybe Xsd.AnyURI
        , product_lang         :: Maybe Xsd.XsdString
        , product_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Product where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "product-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "actuate" e pos
        a4 <- optional $ getAttribute "href" e pos
        a5 <- optional $ getAttribute "role" e pos
        a6 <- optional $ getAttribute "show" e pos
        a7 <- optional $ getAttribute "title" e pos
        a8 <- optional $ getAttribute "type" e pos
        a9 <- optional $ getAttribute "base" e pos
        a10 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Product a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Product{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ product_id x
                       , maybe [] (toXMLAttribute "product-type") $ product_product'type x
                       , maybe [] (toXMLAttribute "specific-use") $ product_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ product_actuate x
                       , maybe [] (toXMLAttribute "href") $ product_href x
                       , maybe [] (toXMLAttribute "role") $ product_role x
                       , maybe [] (toXMLAttribute "show") $ product_show x
                       , maybe [] (toXMLAttribute "title") $ product_title x
                       , maybe [] (toXMLAttribute "type") $ product_type x
                       , maybe [] (toXMLAttribute "base") $ product_base x
                       , maybe [] (toXMLAttribute "lang") $ product_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ product_choice0 x
            ]

elementProduct :: XMLParser Product
elementProduct = parseSchemaType "product"
elementToXMLProduct :: Product -> [Content ()]
elementToXMLProduct = schemaTypeToXML "product"

data Pub'date = Pub'date
        { pub'date_calendar           :: Maybe Xsd.XsdString
        , pub'date_date'type          :: Maybe Xsd.XsdString
        , pub'date_id                 :: Maybe Xsd.ID
        , pub'date_iso'8601'date      :: Maybe Xsd.XsdString
        , pub'date_pub'type           :: Maybe Xsd.XsdString
        , pub'date_publication'format :: Maybe Xsd.XsdString
        , pub'date_base               :: Maybe Xsd.AnyURI
        , pub'date_lang               :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Pub'date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "calendar" e pos
        a1 <- optional $ getAttribute "date-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "iso-8601-date" e pos
        a4 <- optional $ getAttribute "pub-type" e pos
        a5 <- optional $ getAttribute "publication-format" e pos
        a6 <- optional $ getAttribute "base" e pos
        a7 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Pub'date a0 a1 a2 a3 a4 a5 a6 a7)
    schemaTypeToXML s x@Pub'date{} =
        toXMLElement s [ maybe [] (toXMLAttribute "calendar") $ pub'date_calendar x
                       , maybe [] (toXMLAttribute "date-type") $ pub'date_date'type x
                       , maybe [] (toXMLAttribute "id") $ pub'date_id x
                       , maybe [] (toXMLAttribute "iso-8601-date") $ pub'date_iso'8601'date x
                       , maybe [] (toXMLAttribute "pub-type") $ pub'date_pub'type x
                       , maybe [] (toXMLAttribute "publication-format") $ pub'date_publication'format x
                       , maybe [] (toXMLAttribute "base") $ pub'date_base x
                       , maybe [] (toXMLAttribute "lang") $ pub'date_lang x
                       ]
            []

elementPub'date :: XMLParser Pub'date
elementPub'date = parseSchemaType "pub-date"
elementToXMLPub'date :: Pub'date -> [Content ()]
elementToXMLPub'date = schemaTypeToXML "pub-date"

data Pub'id = Pub'id
        { pub'id_assigning'authority :: Maybe Xsd.XsdString
        , pub'id_id                  :: Maybe Xsd.ID
        , pub'id_pub'id'type         :: Maybe Xsd.XsdString
        , pub'id_specific'use        :: Maybe Xsd.XsdString
        , pub'id_actuate             :: Maybe Xsd.XsdString
        , pub'id_href                :: Maybe Xsd.AnyURI
        , pub'id_role                :: Maybe Xsd.XsdString
        , pub'id_show                :: Maybe Xsd.XsdString
        , pub'id_title               :: Maybe Xsd.XsdString
        , pub'id_type                :: Maybe Xsd.XsdString
        , pub'id_base                :: Maybe Xsd.AnyURI
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Pub'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "assigning-authority" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "pub-id-type" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "actuate" e pos
        a5 <- optional $ getAttribute "href" e pos
        a6 <- optional $ getAttribute "role" e pos
        a7 <- optional $ getAttribute "show" e pos
        a8 <- optional $ getAttribute "title" e pos
        a9 <- optional $ getAttribute "type" e pos
        a10 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Pub'id a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Pub'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "assigning-authority") $ pub'id_assigning'authority x
                       , maybe [] (toXMLAttribute "id") $ pub'id_id x
                       , maybe [] (toXMLAttribute "pub-id-type") $ pub'id_pub'id'type x
                       , maybe [] (toXMLAttribute "specific-use") $ pub'id_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ pub'id_actuate x
                       , maybe [] (toXMLAttribute "href") $ pub'id_href x
                       , maybe [] (toXMLAttribute "role") $ pub'id_role x
                       , maybe [] (toXMLAttribute "show") $ pub'id_show x
                       , maybe [] (toXMLAttribute "title") $ pub'id_title x
                       , maybe [] (toXMLAttribute "type") $ pub'id_type x
                       , maybe [] (toXMLAttribute "base") $ pub'id_base x
                       ]
            [
            ]

elementPub'id :: XMLParser Pub'id
elementPub'id = parseSchemaType "pub-id"
elementToXMLPub'id :: Pub'id -> [Content ()]
elementToXMLPub'id = schemaTypeToXML "pub-id"

data Publisher = Publisher
        { publisher_content'type :: Maybe Xsd.XsdString
        , publisher_id           :: Maybe Xsd.ID
        , publisher_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Publisher where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Publisher a0 a1 a2)
    schemaTypeToXML s x@Publisher{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ publisher_content'type x
                       , maybe [] (toXMLAttribute "id") $ publisher_id x
                       , maybe [] (toXMLAttribute "base") $ publisher_base x
                       ]
            []

elementPublisher :: XMLParser Publisher
elementPublisher = parseSchemaType "publisher"
elementToXMLPublisher :: Publisher -> [Content ()]
elementToXMLPublisher = schemaTypeToXML "publisher"

data Publisher'loc = Publisher'loc
        { publisher'loc_id           :: Maybe Xsd.ID
        , publisher'loc_specific'use :: Maybe Xsd.XsdString
        , publisher'loc_base         :: Maybe Xsd.AnyURI
        , publisher'loc_lang         :: Maybe Xsd.XsdString
        , publisher'loc_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Publisher'loc where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Publisher'loc a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Publisher'loc{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ publisher'loc_id x
                       , maybe [] (toXMLAttribute "specific-use") $ publisher'loc_specific'use x
                       , maybe [] (toXMLAttribute "base") $ publisher'loc_base x
                       , maybe [] (toXMLAttribute "lang") $ publisher'loc_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ publisher'loc_choice0 x
            ]

elementPublisher'loc :: XMLParser Publisher'loc
elementPublisher'loc = parseSchemaType "publisher-loc"
elementToXMLPublisher'loc :: Publisher'loc -> [Content ()]
elementToXMLPublisher'loc = schemaTypeToXML "publisher-loc"

data Publisher'name = Publisher'name
        { publisher'name_id           :: Maybe Xsd.ID
        , publisher'name_specific'use :: Maybe Xsd.XsdString
        , publisher'name_base         :: Maybe Xsd.AnyURI
        , publisher'name_lang         :: Maybe Xsd.XsdString
        , publisher'name_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Publisher'name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Publisher'name a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Publisher'name{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ publisher'name_id x
                       , maybe [] (toXMLAttribute "specific-use") $ publisher'name_specific'use x
                       , maybe [] (toXMLAttribute "base") $ publisher'name_base x
                       , maybe [] (toXMLAttribute "lang") $ publisher'name_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ publisher'name_choice0 x
            ]

elementPublisher'name :: XMLParser Publisher'name
elementPublisher'name = parseSchemaType "publisher-name"
elementToXMLPublisher'name :: Publisher'name -> [Content ()]
elementToXMLPublisher'name = schemaTypeToXML "publisher-name"

data Rb = Rb
        { rb_content'type :: Maybe Xsd.XsdString
        , rb_id           :: Maybe Xsd.ID
        , rb_specific'use :: Maybe Xsd.XsdString
        , rb_base         :: Maybe Xsd.AnyURI
        , rb_lang         :: Maybe Xsd.XsdString
        , rb_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Rb where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Rb a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Rb{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ rb_content'type x
                       , maybe [] (toXMLAttribute "id") $ rb_id x
                       , maybe [] (toXMLAttribute "specific-use") $ rb_specific'use x
                       , maybe [] (toXMLAttribute "base") $ rb_base x
                       , maybe [] (toXMLAttribute "lang") $ rb_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ rb_choice0 x
            ]

elementRb :: XMLParser Rb
elementRb = parseSchemaType "rb"
elementToXMLRb :: Rb -> [Content ()]
elementToXMLRb = schemaTypeToXML "rb"

data Ref = Ref
        { ref_content'type :: Maybe Xsd.XsdString
        , ref_id           :: Maybe Xsd.ID
        , ref_specific'use :: Maybe Xsd.XsdString
        , ref_base         :: Maybe Xsd.AnyURI
        , ref_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Ref where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Ref a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Ref{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ ref_content'type x
                       , maybe [] (toXMLAttribute "id") $ ref_id x
                       , maybe [] (toXMLAttribute "specific-use") $ ref_specific'use x
                       , maybe [] (toXMLAttribute "base") $ ref_base x
                       , maybe [] (toXMLAttribute "lang") $ ref_lang x
                       ]
            []

elementRef :: XMLParser Ref
elementRef = parseSchemaType "ref"
elementToXMLRef :: Ref -> [Content ()]
elementToXMLRef = schemaTypeToXML "ref"

data Ref'count = Ref'count
        { ref'count_count :: Xsd.NMTOKEN
        , ref'count_id    :: Maybe Xsd.ID
        , ref'count_base  :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Ref'count where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "count" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Ref'count a0 a1 a2)
    schemaTypeToXML s x@Ref'count{} =
        toXMLElement s [ toXMLAttribute "count" $ ref'count_count x
                       , maybe [] (toXMLAttribute "id") $ ref'count_id x
                       , maybe [] (toXMLAttribute "base") $ ref'count_base x
                       ]
            []

elementRef'count :: XMLParser Ref'count
elementRef'count = parseSchemaType "ref-count"
elementToXMLRef'count :: Ref'count -> [Content ()]
elementToXMLRef'count = schemaTypeToXML "ref-count"

data Ref'list = Ref'list
        { ref'list_content'type :: Maybe Xsd.XsdString
        , ref'list_id           :: Maybe Xsd.ID
        , ref'list_specific'use :: Maybe Xsd.XsdString
        , ref'list_base         :: Maybe Xsd.AnyURI
        , ref'list_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Ref'list where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Ref'list a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Ref'list{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ ref'list_content'type x
                       , maybe [] (toXMLAttribute "id") $ ref'list_id x
                       , maybe [] (toXMLAttribute "specific-use") $ ref'list_specific'use x
                       , maybe [] (toXMLAttribute "base") $ ref'list_base x
                       , maybe [] (toXMLAttribute "lang") $ ref'list_lang x
                       ]
            []

elementRef'list :: XMLParser Ref'list
elementRef'list = parseSchemaType "ref-list"
elementToXMLRef'list :: Ref'list -> [Content ()]
elementToXMLRef'list = schemaTypeToXML "ref-list"

data Related'article = Related'article
        { related'article_elocation'id         :: Maybe Xsd.XsdString
        , related'article_ext'link'type        :: Maybe Xsd.XsdString
        , related'article_id                   :: Maybe Xsd.ID
        , related'article_issue                :: Maybe Xsd.XsdString
        , related'article_journal'id           :: Maybe Xsd.XsdString
        , related'article_journal'id'type      :: Maybe Xsd.XsdString
        , related'article_page                 :: Maybe Xsd.XsdString
        , related'article_related'article'type :: Xsd.XsdString
        , related'article_specific'use         :: Maybe Xsd.XsdString
        , related'article_vol                  :: Maybe Xsd.XsdString
        , related'article_actuate              :: Maybe Xsd.XsdString
        , related'article_href                 :: Maybe Xsd.AnyURI
        , related'article_role                 :: Maybe Xsd.XsdString
        , related'article_show                 :: Maybe Xsd.XsdString
        , related'article_title                :: Maybe Xsd.XsdString
        , related'article_type                 :: Maybe Xsd.XsdString
        , related'article_base                 :: Maybe Xsd.AnyURI
        , related'article_lang                 :: Maybe Xsd.XsdString
        , related'article_choice0              :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Related'article where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "elocation-id" e pos
        a1 <- optional $ getAttribute "ext-link-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "issue" e pos
        a4 <- optional $ getAttribute "journal-id" e pos
        a5 <- optional $ getAttribute "journal-id-type" e pos
        a6 <- optional $ getAttribute "page" e pos
        a7 <- getAttribute "related-article-type" e pos
        a8 <- optional $ getAttribute "specific-use" e pos
        a9 <- optional $ getAttribute "vol" e pos
        a10 <- optional $ getAttribute "actuate" e pos
        a11 <- optional $ getAttribute "href" e pos
        a12 <- optional $ getAttribute "role" e pos
        a13 <- optional $ getAttribute "show" e pos
        a14 <- optional $ getAttribute "title" e pos
        a15 <- optional $ getAttribute "type" e pos
        a16 <- optional $ getAttribute "base" e pos
        a17 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Related'article a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Related'article{} =
        toXMLElement s [ maybe [] (toXMLAttribute "elocation-id") $ related'article_elocation'id x
                       , maybe [] (toXMLAttribute "ext-link-type") $ related'article_ext'link'type x
                       , maybe [] (toXMLAttribute "id") $ related'article_id x
                       , maybe [] (toXMLAttribute "issue") $ related'article_issue x
                       , maybe [] (toXMLAttribute "journal-id") $ related'article_journal'id x
                       , maybe [] (toXMLAttribute "journal-id-type") $ related'article_journal'id'type x
                       , maybe [] (toXMLAttribute "page") $ related'article_page x
                       , toXMLAttribute "related-article-type" $ related'article_related'article'type x
                       , maybe [] (toXMLAttribute "specific-use") $ related'article_specific'use x
                       , maybe [] (toXMLAttribute "vol") $ related'article_vol x
                       , maybe [] (toXMLAttribute "actuate") $ related'article_actuate x
                       , maybe [] (toXMLAttribute "href") $ related'article_href x
                       , maybe [] (toXMLAttribute "role") $ related'article_role x
                       , maybe [] (toXMLAttribute "show") $ related'article_show x
                       , maybe [] (toXMLAttribute "title") $ related'article_title x
                       , maybe [] (toXMLAttribute "type") $ related'article_type x
                       , maybe [] (toXMLAttribute "base") $ related'article_base x
                       , maybe [] (toXMLAttribute "lang") $ related'article_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ related'article_choice0 x
            ]

elementRelated'article :: XMLParser Related'article
elementRelated'article = parseSchemaType "related-article"
elementToXMLRelated'article :: Related'article -> [Content ()]
elementToXMLRelated'article = schemaTypeToXML "related-article"

data Related'object = Related'object
        { related'object_content'type     :: Maybe Xsd.XsdString
        , related'object_document'id      :: Maybe Xsd.XsdString
        , related'object_document'id'type :: Maybe Xsd.XsdString
        , related'object_document'type    :: Maybe Xsd.XsdString
        , related'object_ext'link'type    :: Maybe Xsd.XsdString
        , related'object_id               :: Maybe Xsd.ID
        , related'object_link'type        :: Maybe Xsd.XsdString
        , related'object_object'id        :: Maybe Xsd.XsdString
        , related'object_object'id'type   :: Maybe Xsd.XsdString
        , related'object_object'type      :: Maybe Xsd.XsdString
        , related'object_source'id        :: Maybe Xsd.XsdString
        , related'object_source'id'type   :: Maybe Xsd.XsdString
        , related'object_source'type      :: Maybe Xsd.XsdString
        , related'object_specific'use     :: Maybe Xsd.XsdString
        , related'object_actuate          :: Maybe Xsd.XsdString
        , related'object_href             :: Maybe Xsd.AnyURI
        , related'object_role             :: Maybe Xsd.XsdString
        , related'object_show             :: Maybe Xsd.XsdString
        , related'object_title            :: Maybe Xsd.XsdString
        , related'object_type             :: Maybe Xsd.XsdString
        , related'object_base             :: Maybe Xsd.AnyURI
        , related'object_lang             :: Maybe Xsd.XsdString
        , related'object_choice0          :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Related'object where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "document-id" e pos
        a2 <- optional $ getAttribute "document-id-type" e pos
        a3 <- optional $ getAttribute "document-type" e pos
        a4 <- optional $ getAttribute "ext-link-type" e pos
        a5 <- optional $ getAttribute "id" e pos
        a6 <- optional $ getAttribute "link-type" e pos
        a7 <- optional $ getAttribute "object-id" e pos
        a8 <- optional $ getAttribute "object-id-type" e pos
        a9 <- optional $ getAttribute "object-type" e pos
        a10 <- optional $ getAttribute "source-id" e pos
        a11 <- optional $ getAttribute "source-id-type" e pos
        a12 <- optional $ getAttribute "source-type" e pos
        a13 <- optional $ getAttribute "specific-use" e pos
        a14 <- optional $ getAttribute "actuate" e pos
        a15 <- optional $ getAttribute "href" e pos
        a16 <- optional $ getAttribute "role" e pos
        a17 <- optional $ getAttribute "show" e pos
        a18 <- optional $ getAttribute "title" e pos
        a19 <- optional $ getAttribute "type" e pos
        a20 <- optional $ getAttribute "base" e pos
        a21 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Related'object a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Related'object{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ related'object_content'type x
                       , maybe [] (toXMLAttribute "document-id") $ related'object_document'id x
                       , maybe [] (toXMLAttribute "document-id-type") $ related'object_document'id'type x
                       , maybe [] (toXMLAttribute "document-type") $ related'object_document'type x
                       , maybe [] (toXMLAttribute "ext-link-type") $ related'object_ext'link'type x
                       , maybe [] (toXMLAttribute "id") $ related'object_id x
                       , maybe [] (toXMLAttribute "link-type") $ related'object_link'type x
                       , maybe [] (toXMLAttribute "object-id") $ related'object_object'id x
                       , maybe [] (toXMLAttribute "object-id-type") $ related'object_object'id'type x
                       , maybe [] (toXMLAttribute "object-type") $ related'object_object'type x
                       , maybe [] (toXMLAttribute "source-id") $ related'object_source'id x
                       , maybe [] (toXMLAttribute "source-id-type") $ related'object_source'id'type x
                       , maybe [] (toXMLAttribute "source-type") $ related'object_source'type x
                       , maybe [] (toXMLAttribute "specific-use") $ related'object_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ related'object_actuate x
                       , maybe [] (toXMLAttribute "href") $ related'object_href x
                       , maybe [] (toXMLAttribute "role") $ related'object_role x
                       , maybe [] (toXMLAttribute "show") $ related'object_show x
                       , maybe [] (toXMLAttribute "title") $ related'object_title x
                       , maybe [] (toXMLAttribute "type") $ related'object_type x
                       , maybe [] (toXMLAttribute "base") $ related'object_base x
                       , maybe [] (toXMLAttribute "lang") $ related'object_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ related'object_choice0 x
            ]

elementRelated'object :: XMLParser Related'object
elementRelated'object = parseSchemaType "related-object"
elementToXMLRelated'object :: Related'object -> [Content ()]
elementToXMLRelated'object = schemaTypeToXML "related-object"

data Response = Response
        { response_id            :: Maybe Xsd.ID
        , response_response'type :: Maybe Xsd.XsdString
        , response_specific'use  :: Maybe Xsd.XsdString
        , response_base          :: Maybe Xsd.AnyURI
        , response_lang          :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Response where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "response-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Response a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Response{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ response_id x
                       , maybe [] (toXMLAttribute "response-type") $ response_response'type x
                       , maybe [] (toXMLAttribute "specific-use") $ response_specific'use x
                       , maybe [] (toXMLAttribute "base") $ response_base x
                       , maybe [] (toXMLAttribute "lang") $ response_lang x
                       ]
            []

elementResponse :: XMLParser Response
elementResponse = parseSchemaType "response"
elementToXMLResponse :: Response -> [Content ()]
elementToXMLResponse = schemaTypeToXML "response"

data Role = Role
        { role_content'type :: Maybe Xsd.XsdString
        , role_id           :: Maybe Xsd.ID
        , role_specific'use :: Maybe Xsd.XsdString
        , role_base         :: Maybe Xsd.AnyURI
        , role_lang         :: Maybe Xsd.XsdString
        , role_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Role where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Role a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Role{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ role_content'type x
                       , maybe [] (toXMLAttribute "id") $ role_id x
                       , maybe [] (toXMLAttribute "specific-use") $ role_specific'use x
                       , maybe [] (toXMLAttribute "base") $ role_base x
                       , maybe [] (toXMLAttribute "lang") $ role_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ role_choice0 x
            ]

elementRole :: XMLParser Role
elementRole = parseSchemaType "role"
elementToXMLRole :: Role -> [Content ()]
elementToXMLRole = schemaTypeToXML "role"

data Roman = Roman
        { roman_id           :: Maybe Xsd.ID
        , roman_specific'use :: Maybe Xsd.XsdString
        , roman_toggle       :: Maybe Xsd.XsdString
        , roman_base         :: Maybe Xsd.AnyURI
        , roman_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Roman where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "toggle" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Roman a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Roman{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ roman_id x
                       , maybe [] (toXMLAttribute "specific-use") $ roman_specific'use x
                       , maybe [] (toXMLAttribute "toggle") $ roman_toggle x
                       , maybe [] (toXMLAttribute "base") $ roman_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ roman_choice0 x
            ]

elementRoman :: XMLParser Roman
elementRoman = parseSchemaType "roman"
elementToXMLRoman :: Roman -> [Content ()]
elementToXMLRoman = schemaTypeToXML "roman"

data Rp = Rp
        { rp_id   :: Maybe Xsd.ID
        , rp_base :: Maybe Xsd.AnyURI
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Rp where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Rp a0 a1)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Rp{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ rp_id x
                       , maybe [] (toXMLAttribute "base") $ rp_base x
                       ]
            [
            ]

elementRp :: XMLParser Rp
elementRp = parseSchemaType "rp"
elementToXMLRp :: Rp -> [Content ()]
elementToXMLRp = schemaTypeToXML "rp"

data Rt = Rt
        { rt_content'type :: Maybe Xsd.XsdString
        , rt_id           :: Maybe Xsd.ID
        , rt_specific'use :: Maybe Xsd.XsdString
        , rt_base         :: Maybe Xsd.AnyURI
        , rt_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Rt where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Rt a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Rt{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ rt_content'type x
                       , maybe [] (toXMLAttribute "id") $ rt_id x
                       , maybe [] (toXMLAttribute "specific-use") $ rt_specific'use x
                       , maybe [] (toXMLAttribute "base") $ rt_base x
                       , maybe [] (toXMLAttribute "lang") $ rt_lang x
                       ]
            []

elementRt :: XMLParser Rt
elementRt = parseSchemaType "rt"
elementToXMLRt :: Rt -> [Content ()]
elementToXMLRt = schemaTypeToXML "rt"

data Ruby = Ruby
        { ruby_content'type :: Maybe Xsd.XsdString
        , ruby_id           :: Maybe Xsd.ID
        , ruby_specific'use :: Maybe Xsd.XsdString
        , ruby_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Ruby where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Ruby a0 a1 a2 a3)
    schemaTypeToXML s x@Ruby{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ ruby_content'type x
                       , maybe [] (toXMLAttribute "id") $ ruby_id x
                       , maybe [] (toXMLAttribute "specific-use") $ ruby_specific'use x
                       , maybe [] (toXMLAttribute "base") $ ruby_base x
                       ]
            []

elementRuby :: XMLParser Ruby
elementRuby = parseSchemaType "ruby"
elementToXMLRuby :: Ruby -> [Content ()]
elementToXMLRuby = schemaTypeToXML "ruby"

data Sans'serif = Sans'serif
        { sans'serif_id           :: Maybe Xsd.ID
        , sans'serif_specific'use :: Maybe Xsd.XsdString
        , sans'serif_toggle       :: Maybe Xsd.XsdString
        , sans'serif_base         :: Maybe Xsd.AnyURI
        , sans'serif_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Sans'serif where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "toggle" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Sans'serif a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Sans'serif{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ sans'serif_id x
                       , maybe [] (toXMLAttribute "specific-use") $ sans'serif_specific'use x
                       , maybe [] (toXMLAttribute "toggle") $ sans'serif_toggle x
                       , maybe [] (toXMLAttribute "base") $ sans'serif_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ sans'serif_choice0 x
            ]

elementSans'serif :: XMLParser Sans'serif
elementSans'serif = parseSchemaType "sans-serif"
elementToXMLSans'serif :: Sans'serif -> [Content ()]
elementToXMLSans'serif = schemaTypeToXML "sans-serif"

data Sc = Sc
        { sc_id           :: Maybe Xsd.ID
        , sc_specific'use :: Maybe Xsd.XsdString
        , sc_toggle       :: Maybe Xsd.XsdString
        , sc_base         :: Maybe Xsd.AnyURI
        , sc_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Sc where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "toggle" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Sc a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Sc{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ sc_id x
                       , maybe [] (toXMLAttribute "specific-use") $ sc_specific'use x
                       , maybe [] (toXMLAttribute "toggle") $ sc_toggle x
                       , maybe [] (toXMLAttribute "base") $ sc_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ sc_choice0 x
            ]

elementSc :: XMLParser Sc
elementSc = parseSchemaType "sc"
elementToXMLSc :: Sc -> [Content ()]
elementToXMLSc = schemaTypeToXML "sc"

data Season = Season
        { season_content'type :: Maybe Xsd.XsdString
        , season_id           :: Maybe Xsd.ID
        , season_specific'use :: Maybe Xsd.XsdString
        , season_base         :: Maybe Xsd.AnyURI
        , season_lang         :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Season where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Season a0 a1 a2 a3 a4)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Season{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ season_content'type x
                       , maybe [] (toXMLAttribute "id") $ season_id x
                       , maybe [] (toXMLAttribute "specific-use") $ season_specific'use x
                       , maybe [] (toXMLAttribute "base") $ season_base x
                       , maybe [] (toXMLAttribute "lang") $ season_lang x
                       ]
            [
            ]

elementSeason :: XMLParser Season
elementSeason = parseSchemaType "season"
elementToXMLSeason :: Season -> [Content ()]
elementToXMLSeason = schemaTypeToXML "season"

data Sec = Sec
        { sec_id           :: Maybe Xsd.ID
        , sec_sec'type     :: Maybe Xsd.XsdString
        , sec_specific'use :: Maybe Xsd.XsdString
        , sec_base         :: Maybe Xsd.AnyURI
        , sec_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Sec where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "sec-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Sec a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Sec{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ sec_id x
                       , maybe [] (toXMLAttribute "sec-type") $ sec_sec'type x
                       , maybe [] (toXMLAttribute "specific-use") $ sec_specific'use x
                       , maybe [] (toXMLAttribute "base") $ sec_base x
                       , maybe [] (toXMLAttribute "lang") $ sec_lang x
                       ]
            []

elementSec :: XMLParser Sec
elementSec = parseSchemaType "sec"
elementToXMLSec :: Sec -> [Content ()]
elementToXMLSec = schemaTypeToXML "sec"

data Sec'meta = Sec'meta
        { sec'meta_id   :: Maybe Xsd.ID
        , sec'meta_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Sec'meta where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Sec'meta a0 a1)
    schemaTypeToXML s x@Sec'meta{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ sec'meta_id x
                       , maybe [] (toXMLAttribute "base") $ sec'meta_base x
                       ]
            []

elementSec'meta :: XMLParser Sec'meta
elementSec'meta = parseSchemaType "sec-meta"
elementToXMLSec'meta :: Sec'meta -> [Content ()]
elementToXMLSec'meta = schemaTypeToXML "sec-meta"

data Self'uri = Self'uri
        { self'uri_content'type :: Maybe Xsd.XsdString
        , self'uri_id           :: Maybe Xsd.ID
        , self'uri_specific'use :: Maybe Xsd.XsdString
        , self'uri_actuate      :: Maybe Xsd.XsdString
        , self'uri_href         :: Maybe Xsd.AnyURI
        , self'uri_role         :: Maybe Xsd.XsdString
        , self'uri_show         :: Maybe Xsd.XsdString
        , self'uri_title        :: Maybe Xsd.XsdString
        , self'uri_type         :: Maybe Xsd.XsdString
        , self'uri_base         :: Maybe Xsd.AnyURI
        , self'uri_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Self'uri where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "actuate" e pos
        a4 <- optional $ getAttribute "href" e pos
        a5 <- optional $ getAttribute "role" e pos
        a6 <- optional $ getAttribute "show" e pos
        a7 <- optional $ getAttribute "title" e pos
        a8 <- optional $ getAttribute "type" e pos
        a9 <- optional $ getAttribute "base" e pos
        a10 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Self'uri a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    schemaTypeToXML s x@Self'uri{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ self'uri_content'type x
                       , maybe [] (toXMLAttribute "id") $ self'uri_id x
                       , maybe [] (toXMLAttribute "specific-use") $ self'uri_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ self'uri_actuate x
                       , maybe [] (toXMLAttribute "href") $ self'uri_href x
                       , maybe [] (toXMLAttribute "role") $ self'uri_role x
                       , maybe [] (toXMLAttribute "show") $ self'uri_show x
                       , maybe [] (toXMLAttribute "title") $ self'uri_title x
                       , maybe [] (toXMLAttribute "type") $ self'uri_type x
                       , maybe [] (toXMLAttribute "base") $ self'uri_base x
                       , maybe [] (toXMLAttribute "lang") $ self'uri_lang x
                       ]
            []

elementSelf'uri :: XMLParser Self'uri
elementSelf'uri = parseSchemaType "self-uri"
elementToXMLSelf'uri :: Self'uri -> [Content ()]
elementToXMLSelf'uri = schemaTypeToXML "self-uri"

data Series = Series
        { series_content'type :: Maybe Xsd.XsdString
        , series_id           :: Maybe Xsd.ID
        , series_specific'use :: Maybe Xsd.XsdString
        , series_base         :: Maybe Xsd.AnyURI
        , series_lang         :: Maybe Xsd.XsdString
        , series_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Series where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Series a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Series{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ series_content'type x
                       , maybe [] (toXMLAttribute "id") $ series_id x
                       , maybe [] (toXMLAttribute "specific-use") $ series_specific'use x
                       , maybe [] (toXMLAttribute "base") $ series_base x
                       , maybe [] (toXMLAttribute "lang") $ series_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ series_choice0 x
            ]

elementSeries :: XMLParser Series
elementSeries = parseSchemaType "series"
elementToXMLSeries :: Series -> [Content ()]
elementToXMLSeries = schemaTypeToXML "series"

data Series'text = Series'text
        { series'text_content'type :: Maybe Xsd.XsdString
        , series'text_id           :: Maybe Xsd.ID
        , series'text_specific'use :: Maybe Xsd.XsdString
        , series'text_base         :: Maybe Xsd.AnyURI
        , series'text_lang         :: Maybe Xsd.XsdString
        , series'text_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Series'text where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Series'text a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Series'text{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ series'text_content'type x
                       , maybe [] (toXMLAttribute "id") $ series'text_id x
                       , maybe [] (toXMLAttribute "specific-use") $ series'text_specific'use x
                       , maybe [] (toXMLAttribute "base") $ series'text_base x
                       , maybe [] (toXMLAttribute "lang") $ series'text_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ series'text_choice0 x
            ]

elementSeries'text :: XMLParser Series'text
elementSeries'text = parseSchemaType "series-text"
elementToXMLSeries'text :: Series'text -> [Content ()]
elementToXMLSeries'text = schemaTypeToXML "series-text"

data Series'title = Series'title
        { series'title_content'type :: Maybe Xsd.XsdString
        , series'title_id           :: Maybe Xsd.ID
        , series'title_specific'use :: Maybe Xsd.XsdString
        , series'title_base         :: Maybe Xsd.AnyURI
        , series'title_lang         :: Maybe Xsd.XsdString
        , series'title_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Series'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Series'title a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Series'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ series'title_content'type x
                       , maybe [] (toXMLAttribute "id") $ series'title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ series'title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ series'title_base x
                       , maybe [] (toXMLAttribute "lang") $ series'title_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ series'title_choice0 x
            ]

elementSeries'title :: XMLParser Series'title
elementSeries'title = parseSchemaType "series-title"
elementToXMLSeries'title :: Series'title -> [Content ()]
elementToXMLSeries'title = schemaTypeToXML "series-title"

data Sig = Sig
        { sig_content'type :: Maybe Xsd.XsdString
        , sig_id           :: Maybe Xsd.ID
        , sig_rid          :: Maybe Xsd.IDREFS
        , sig_specific'use :: Maybe Xsd.XsdString
        , sig_base         :: Maybe Xsd.AnyURI
        , sig_lang         :: Maybe Xsd.XsdString
        , sig_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Sig where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "rid" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Sig a0 a1 a2 a3 a4 a5)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Sig{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ sig_content'type x
                       , maybe [] (toXMLAttribute "id") $ sig_id x
                       , maybe [] (toXMLAttribute "rid") $ sig_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ sig_specific'use x
                       , maybe [] (toXMLAttribute "base") $ sig_base x
                       , maybe [] (toXMLAttribute "lang") $ sig_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ sig_choice0 x
            ]

elementSig :: XMLParser Sig
elementSig = parseSchemaType "sig"
elementToXMLSig :: Sig -> [Content ()]
elementToXMLSig = schemaTypeToXML "sig"

data Sig'block = Sig'block
        { sig'block_content'type :: Maybe Xsd.XsdString
        , sig'block_id           :: Maybe Xsd.ID
        , sig'block_rid          :: Maybe Xsd.IDREFS
        , sig'block_specific'use :: Maybe Xsd.XsdString
        , sig'block_base         :: Maybe Xsd.AnyURI
        , sig'block_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Sig'block where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "rid" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Sig'block a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Sig'block{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ sig'block_content'type x
                       , maybe [] (toXMLAttribute "id") $ sig'block_id x
                       , maybe [] (toXMLAttribute "rid") $ sig'block_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ sig'block_specific'use x
                       , maybe [] (toXMLAttribute "base") $ sig'block_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ sig'block_choice0 x
            ]

elementSig'block :: XMLParser Sig'block
elementSig'block = parseSchemaType "sig-block"
elementToXMLSig'block :: Sig'block -> [Content ()]
elementToXMLSig'block = schemaTypeToXML "sig-block"

data Size = Size
        { size_id           :: Maybe Xsd.ID
        , size_specific'use :: Maybe Xsd.XsdString
        , size_units        :: Xsd.XsdString
        , size_base         :: Maybe Xsd.AnyURI
        , size_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Size where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- getAttribute "units" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Size a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Size{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ size_id x
                       , maybe [] (toXMLAttribute "specific-use") $ size_specific'use x
                       , toXMLAttribute "units" $ size_units x
                       , maybe [] (toXMLAttribute "base") $ size_base x
                       , maybe [] (toXMLAttribute "lang") $ size_lang x
                       ]
            []

elementSize :: XMLParser Size
elementSize = parseSchemaType "size"
elementToXMLSize :: Size -> [Content ()]
elementToXMLSize = schemaTypeToXML "size"

data Source = Source
        { source_content'type :: Maybe Xsd.XsdString
        , source_id           :: Maybe Xsd.ID
        , source_specific'use :: Maybe Xsd.XsdString
        , source_base         :: Maybe Xsd.AnyURI
        , source_lang         :: Maybe Xsd.XsdString
        , source_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Source where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Source a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Source{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ source_content'type x
                       , maybe [] (toXMLAttribute "id") $ source_id x
                       , maybe [] (toXMLAttribute "specific-use") $ source_specific'use x
                       , maybe [] (toXMLAttribute "base") $ source_base x
                       , maybe [] (toXMLAttribute "lang") $ source_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ source_choice0 x
            ]

elementSource :: XMLParser Source
elementSource = parseSchemaType "source"
elementToXMLSource :: Source -> [Content ()]
elementToXMLSource = schemaTypeToXML "source"

data Speaker = Speaker
        { speaker_content'type :: Maybe Xsd.XsdString
        , speaker_id           :: Maybe Xsd.ID
        , speaker_specific'use :: Maybe Xsd.XsdString
        , speaker_base         :: Maybe Xsd.AnyURI
        , speaker_lang         :: Maybe Xsd.XsdString
        , speaker_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Speaker where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Speaker a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Speaker{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ speaker_content'type x
                       , maybe [] (toXMLAttribute "id") $ speaker_id x
                       , maybe [] (toXMLAttribute "specific-use") $ speaker_specific'use x
                       , maybe [] (toXMLAttribute "base") $ speaker_base x
                       , maybe [] (toXMLAttribute "lang") $ speaker_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ speaker_choice0 x
            ]

elementSpeaker :: XMLParser Speaker
elementSpeaker = parseSchemaType "speaker"
elementToXMLSpeaker :: Speaker -> [Content ()]
elementToXMLSpeaker = schemaTypeToXML "speaker"

data Speech = Speech
        { speech_content'type :: Maybe Xsd.XsdString
        , speech_id           :: Maybe Xsd.ID
        , speech_specific'use :: Maybe Xsd.XsdString
        , speech_base         :: Maybe Xsd.AnyURI
        , speech_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Speech where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Speech a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Speech{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ speech_content'type x
                       , maybe [] (toXMLAttribute "id") $ speech_id x
                       , maybe [] (toXMLAttribute "specific-use") $ speech_specific'use x
                       , maybe [] (toXMLAttribute "base") $ speech_base x
                       , maybe [] (toXMLAttribute "lang") $ speech_lang x
                       ]
            []

elementSpeech :: XMLParser Speech
elementSpeech = parseSchemaType "speech"
elementToXMLSpeech :: Speech -> [Content ()]
elementToXMLSpeech = schemaTypeToXML "speech"

data State = State
        { state_content'type :: Maybe Xsd.XsdString
        , state_id           :: Maybe Xsd.ID
        , state_specific'use :: Maybe Xsd.XsdString
        , state_base         :: Maybe Xsd.AnyURI
        , state_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType State where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (State a0 a1 a2 a3 a4)
    schemaTypeToXML s x@State{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ state_content'type x
                       , maybe [] (toXMLAttribute "id") $ state_id x
                       , maybe [] (toXMLAttribute "specific-use") $ state_specific'use x
                       , maybe [] (toXMLAttribute "base") $ state_base x
                       , maybe [] (toXMLAttribute "lang") $ state_lang x
                       ]
            []

elementState :: XMLParser State
elementState = parseSchemaType "state"
elementToXMLState :: State -> [Content ()]
elementToXMLState = schemaTypeToXML "state"

data Statement = Statement
        { statement_content'type :: Maybe Xsd.XsdString
        , statement_id           :: Maybe Xsd.ID
        , statement_specific'use :: Maybe Xsd.XsdString
        , statement_base         :: Maybe Xsd.AnyURI
        , statement_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Statement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Statement a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Statement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ statement_content'type x
                       , maybe [] (toXMLAttribute "id") $ statement_id x
                       , maybe [] (toXMLAttribute "specific-use") $ statement_specific'use x
                       , maybe [] (toXMLAttribute "base") $ statement_base x
                       , maybe [] (toXMLAttribute "lang") $ statement_lang x
                       ]
            []

elementStatement :: XMLParser Statement
elementStatement = parseSchemaType "statement"
elementToXMLStatement :: Statement -> [Content ()]
elementToXMLStatement = schemaTypeToXML "statement"

data Std = Std
        { std_content'type :: Maybe Xsd.XsdString
        , std_id           :: Maybe Xsd.ID
        , std_specific'use :: Maybe Xsd.XsdString
        , std_base         :: Maybe Xsd.AnyURI
        , std_lang         :: Maybe Xsd.XsdString
        , std_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Std where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Std a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Std{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ std_content'type x
                       , maybe [] (toXMLAttribute "id") $ std_id x
                       , maybe [] (toXMLAttribute "specific-use") $ std_specific'use x
                       , maybe [] (toXMLAttribute "base") $ std_base x
                       , maybe [] (toXMLAttribute "lang") $ std_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ std_choice0 x
            ]

elementStd :: XMLParser Std
elementStd = parseSchemaType "std"
elementToXMLStd :: Std -> [Content ()]
elementToXMLStd = schemaTypeToXML "std"

data Std'organization = Std'organization
        { std'organization_content'type :: Maybe Xsd.XsdString
        , std'organization_id           :: Maybe Xsd.ID
        , std'organization_specific'use :: Maybe Xsd.XsdString
        , std'organization_base         :: Maybe Xsd.AnyURI
        , std'organization_lang         :: Maybe Xsd.XsdString
        , std'organization_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Std'organization where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Std'organization a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Std'organization{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ std'organization_content'type x
                       , maybe [] (toXMLAttribute "id") $ std'organization_id x
                       , maybe [] (toXMLAttribute "specific-use") $ std'organization_specific'use x
                       , maybe [] (toXMLAttribute "base") $ std'organization_base x
                       , maybe [] (toXMLAttribute "lang") $ std'organization_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ std'organization_choice0 x
            ]

elementStd'organization :: XMLParser Std'organization
elementStd'organization = parseSchemaType "std-organization"
elementToXMLStd'organization :: Std'organization -> [Content ()]
elementToXMLStd'organization = schemaTypeToXML "std-organization"

data Strike = Strike
        { strike_id           :: Maybe Xsd.ID
        , strike_specific'use :: Maybe Xsd.XsdString
        , strike_toggle       :: Maybe Xsd.XsdString
        , strike_base         :: Maybe Xsd.AnyURI
        , strike_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Strike where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "toggle" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Strike a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Strike{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ strike_id x
                       , maybe [] (toXMLAttribute "specific-use") $ strike_specific'use x
                       , maybe [] (toXMLAttribute "toggle") $ strike_toggle x
                       , maybe [] (toXMLAttribute "base") $ strike_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ strike_choice0 x
            ]

elementStrike :: XMLParser Strike
elementStrike = parseSchemaType "strike"
elementToXMLStrike :: Strike -> [Content ()]
elementToXMLStrike = schemaTypeToXML "strike"

data String'conf = String'conf
        { string'conf_content'type :: Maybe Xsd.XsdString
        , string'conf_id           :: Maybe Xsd.ID
        , string'conf_specific'use :: Maybe Xsd.XsdString
        , string'conf_base         :: Maybe Xsd.AnyURI
        , string'conf_lang         :: Maybe Xsd.XsdString
        , string'conf_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType String'conf where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (String'conf a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@String'conf{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ string'conf_content'type x
                       , maybe [] (toXMLAttribute "id") $ string'conf_id x
                       , maybe [] (toXMLAttribute "specific-use") $ string'conf_specific'use x
                       , maybe [] (toXMLAttribute "base") $ string'conf_base x
                       , maybe [] (toXMLAttribute "lang") $ string'conf_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ string'conf_choice0 x
            ]

elementString'conf :: XMLParser String'conf
elementString'conf = parseSchemaType "string-conf"
elementToXMLString'conf :: String'conf -> [Content ()]
elementToXMLString'conf = schemaTypeToXML "string-conf"

data String'date = String'date
        { string'date_calendar      :: Maybe Xsd.XsdString
        , string'date_content'type  :: Maybe Xsd.XsdString
        , string'date_id            :: Maybe Xsd.ID
        , string'date_iso'8601'date :: Maybe Xsd.XsdString
        , string'date_specific'use  :: Maybe Xsd.XsdString
        , string'date_base          :: Maybe Xsd.AnyURI
        , string'date_lang          :: Maybe Xsd.XsdString
        , string'date_choice0       :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType String'date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "calendar" e pos
        a1 <- optional $ getAttribute "content-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "iso-8601-date" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (String'date a0 a1 a2 a3 a4 a5 a6)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@String'date{} =
        toXMLElement s [ maybe [] (toXMLAttribute "calendar") $ string'date_calendar x
                       , maybe [] (toXMLAttribute "content-type") $ string'date_content'type x
                       , maybe [] (toXMLAttribute "id") $ string'date_id x
                       , maybe [] (toXMLAttribute "iso-8601-date") $ string'date_iso'8601'date x
                       , maybe [] (toXMLAttribute "specific-use") $ string'date_specific'use x
                       , maybe [] (toXMLAttribute "base") $ string'date_base x
                       , maybe [] (toXMLAttribute "lang") $ string'date_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ string'date_choice0 x
            ]

elementString'date :: XMLParser String'date
elementString'date = parseSchemaType "string-date"
elementToXMLString'date :: String'date -> [Content ()]
elementToXMLString'date = schemaTypeToXML "string-date"

data String'name = String'name
        { string'name_content'type :: Maybe Xsd.XsdString
        , string'name_id           :: Maybe Xsd.ID
        , string'name_name'style   :: Maybe Xsd.XsdString
        , string'name_specific'use :: Maybe Xsd.XsdString
        , string'name_base         :: Maybe Xsd.AnyURI
        , string'name_lang         :: Maybe Xsd.XsdString
        , string'name_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType String'name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "name-style" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (String'name a0 a1 a2 a3 a4 a5)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@String'name{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ string'name_content'type x
                       , maybe [] (toXMLAttribute "id") $ string'name_id x
                       , maybe [] (toXMLAttribute "name-style") $ string'name_name'style x
                       , maybe [] (toXMLAttribute "specific-use") $ string'name_specific'use x
                       , maybe [] (toXMLAttribute "base") $ string'name_base x
                       , maybe [] (toXMLAttribute "lang") $ string'name_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ string'name_choice0 x
            ]

elementString'name :: XMLParser String'name
elementString'name = parseSchemaType "string-name"
elementToXMLString'name :: String'name -> [Content ()]
elementToXMLString'name = schemaTypeToXML "string-name"

data Styled'content = Styled'content
        { styled'content_alt          :: Maybe Xsd.XsdString
        , styled'content_id           :: Maybe Xsd.ID
        , styled'content_specific'use :: Maybe Xsd.XsdString
        , styled'content_style        :: Maybe Xsd.XsdString
        , styled'content_style'type   :: Maybe Xsd.XsdString
        , styled'content_toggle       :: Maybe Xsd.XsdString
        , styled'content_base         :: Maybe Xsd.AnyURI
        , styled'content_lang         :: Maybe Xsd.XsdString
        , styled'content_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Styled'content where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "alt" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "style" e pos
        a4 <- optional $ getAttribute "style-type" e pos
        a5 <- optional $ getAttribute "toggle" e pos
        a6 <- optional $ getAttribute "base" e pos
        a7 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Styled'content a0 a1 a2 a3 a4 a5 a6 a7)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Styled'content{} =
        toXMLElement s [ maybe [] (toXMLAttribute "alt") $ styled'content_alt x
                       , maybe [] (toXMLAttribute "id") $ styled'content_id x
                       , maybe [] (toXMLAttribute "specific-use") $ styled'content_specific'use x
                       , maybe [] (toXMLAttribute "style") $ styled'content_style x
                       , maybe [] (toXMLAttribute "style-type") $ styled'content_style'type x
                       , maybe [] (toXMLAttribute "toggle") $ styled'content_toggle x
                       , maybe [] (toXMLAttribute "base") $ styled'content_base x
                       , maybe [] (toXMLAttribute "lang") $ styled'content_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ styled'content_choice0 x
            ]

elementStyled'content :: XMLParser Styled'content
elementStyled'content = parseSchemaType "styled-content"
elementToXMLStyled'content :: Styled'content -> [Content ()]
elementToXMLStyled'content = schemaTypeToXML "styled-content"

data Sub = Sub
        { sub_arrange      :: Maybe Xsd.XsdString
        , sub_id           :: Maybe Xsd.ID
        , sub_specific'use :: Maybe Xsd.XsdString
        , sub_base         :: Maybe Xsd.AnyURI
        , sub_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Sub where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "arrange" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Sub a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Sub{} =
        toXMLElement s [ maybe [] (toXMLAttribute "arrange") $ sub_arrange x
                       , maybe [] (toXMLAttribute "id") $ sub_id x
                       , maybe [] (toXMLAttribute "specific-use") $ sub_specific'use x
                       , maybe [] (toXMLAttribute "base") $ sub_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ sub_choice0 x
            ]

elementSub :: XMLParser Sub
elementSub = parseSchemaType "sub"
elementToXMLSub :: Sub -> [Content ()]
elementToXMLSub = schemaTypeToXML "sub"

data Sub'article = Sub'article
        { sub'article_article'type :: Maybe Xsd.XsdString
        , sub'article_id           :: Maybe Xsd.ID
        , sub'article_specific'use :: Maybe Xsd.XsdString
        , sub'article_base         :: Maybe Xsd.AnyURI
        , sub'article_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Sub'article where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "article-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Sub'article a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Sub'article{} =
        toXMLElement s [ maybe [] (toXMLAttribute "article-type") $ sub'article_article'type x
                       , maybe [] (toXMLAttribute "id") $ sub'article_id x
                       , maybe [] (toXMLAttribute "specific-use") $ sub'article_specific'use x
                       , maybe [] (toXMLAttribute "base") $ sub'article_base x
                       , maybe [] (toXMLAttribute "lang") $ sub'article_lang x
                       ]
            []

elementSub'article :: XMLParser Sub'article
elementSub'article = parseSchemaType "sub-article"
elementToXMLSub'article :: Sub'article -> [Content ()]
elementToXMLSub'article = schemaTypeToXML "sub-article"

data Subj'group = Subj'group
        { subj'group_id              :: Maybe Xsd.ID
        , subj'group_specific'use    :: Maybe Xsd.XsdString
        , subj'group_subj'group'type :: Maybe Xsd.XsdString
        , subj'group_base            :: Maybe Xsd.AnyURI
        , subj'group_lang            :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Subj'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "subj-group-type" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Subj'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Subj'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ subj'group_id x
                       , maybe [] (toXMLAttribute "specific-use") $ subj'group_specific'use x
                       , maybe [] (toXMLAttribute "subj-group-type") $ subj'group_subj'group'type x
                       , maybe [] (toXMLAttribute "base") $ subj'group_base x
                       , maybe [] (toXMLAttribute "lang") $ subj'group_lang x
                       ]
            []

elementSubj'group :: XMLParser Subj'group
elementSubj'group = parseSchemaType "subj-group"
elementToXMLSubj'group :: Subj'group -> [Content ()]
elementToXMLSubj'group = schemaTypeToXML "subj-group"

data Subject = Subject
        { subject_content'type :: Maybe Xsd.XsdString
        , subject_id           :: Maybe Xsd.ID
        , subject_base         :: Maybe Xsd.AnyURI
        , subject_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Subject where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Subject a0 a1 a2)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Subject{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ subject_content'type x
                       , maybe [] (toXMLAttribute "id") $ subject_id x
                       , maybe [] (toXMLAttribute "base") $ subject_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ subject_choice0 x
            ]

elementSubject :: XMLParser Subject
elementSubject = parseSchemaType "subject"
elementToXMLSubject :: Subject -> [Content ()]
elementToXMLSubject = schemaTypeToXML "subject"

data Subtitle = Subtitle
        { subtitle_content'type :: Maybe Xsd.XsdString
        , subtitle_id           :: Maybe Xsd.ID
        , subtitle_specific'use :: Maybe Xsd.XsdString
        , subtitle_base         :: Maybe Xsd.AnyURI
        , subtitle_lang         :: Maybe Xsd.XsdString
        , subtitle_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Subtitle where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Subtitle a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Subtitle{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ subtitle_content'type x
                       , maybe [] (toXMLAttribute "id") $ subtitle_id x
                       , maybe [] (toXMLAttribute "specific-use") $ subtitle_specific'use x
                       , maybe [] (toXMLAttribute "base") $ subtitle_base x
                       , maybe [] (toXMLAttribute "lang") $ subtitle_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ subtitle_choice0 x
            ]

elementSubtitle :: XMLParser Subtitle
elementSubtitle = parseSchemaType "subtitle"
elementToXMLSubtitle :: Subtitle -> [Content ()]
elementToXMLSubtitle = schemaTypeToXML "subtitle"

data Suffix = Suffix
        { suffix_content'type :: Maybe Xsd.XsdString
        , suffix_id           :: Maybe Xsd.ID
        , suffix_specific'use :: Maybe Xsd.XsdString
        , suffix_base         :: Maybe Xsd.AnyURI
        , suffix_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Suffix where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Suffix a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Suffix{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ suffix_content'type x
                       , maybe [] (toXMLAttribute "id") $ suffix_id x
                       , maybe [] (toXMLAttribute "specific-use") $ suffix_specific'use x
                       , maybe [] (toXMLAttribute "base") $ suffix_base x
                       , maybe [] (toXMLAttribute "lang") $ suffix_lang x
                       ]
            []

elementSuffix :: XMLParser Suffix
elementSuffix = parseSchemaType "suffix"
elementToXMLSuffix :: Suffix -> [Content ()]
elementToXMLSuffix = schemaTypeToXML "suffix"

data Sup = Sup
        { sup_arrange      :: Maybe Xsd.XsdString
        , sup_id           :: Maybe Xsd.ID
        , sup_specific'use :: Maybe Xsd.XsdString
        , sup_base         :: Maybe Xsd.AnyURI
        , sup_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Sup where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "arrange" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Sup a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Sup{} =
        toXMLElement s [ maybe [] (toXMLAttribute "arrange") $ sup_arrange x
                       , maybe [] (toXMLAttribute "id") $ sup_id x
                       , maybe [] (toXMLAttribute "specific-use") $ sup_specific'use x
                       , maybe [] (toXMLAttribute "base") $ sup_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ sup_choice0 x
            ]

elementSup :: XMLParser Sup
elementSup = parseSchemaType "sup"
elementToXMLSup :: Sup -> [Content ()]
elementToXMLSup = schemaTypeToXML "sup"

data Supplement = Supplement
        { supplement_id              :: Maybe Xsd.ID
        , supplement_specific'use    :: Maybe Xsd.XsdString
        , supplement_supplement'type :: Maybe Xsd.XsdString
        , supplement_base            :: Maybe Xsd.AnyURI
        , supplement_lang            :: Maybe Xsd.XsdString
        , supplement_choice0         :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Supplement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "supplement-type" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Supplement a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Supplement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ supplement_id x
                       , maybe [] (toXMLAttribute "specific-use") $ supplement_specific'use x
                       , maybe [] (toXMLAttribute "supplement-type") $ supplement_supplement'type x
                       , maybe [] (toXMLAttribute "base") $ supplement_base x
                       , maybe [] (toXMLAttribute "lang") $ supplement_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ supplement_choice0 x
            ]

elementSupplement :: XMLParser Supplement
elementSupplement = parseSchemaType "supplement"
elementToXMLSupplement :: Supplement -> [Content ()]
elementToXMLSupplement = schemaTypeToXML "supplement"

data Supplementary'material = Supplementary'material
        { supplementary'material_content'type :: Maybe Xsd.XsdString
        , supplementary'material_id           :: Maybe Xsd.ID
        , supplementary'material_mime'subtype :: Maybe Xsd.XsdString
        , supplementary'material_mimetype     :: Maybe Xsd.XsdString
        , supplementary'material_orientation  :: Maybe Xsd.XsdString
        , supplementary'material_position     :: Maybe Xsd.XsdString
        , supplementary'material_specific'use :: Maybe Xsd.XsdString
        , supplementary'material_actuate      :: Maybe Xsd.XsdString
        , supplementary'material_href         :: Maybe Xsd.AnyURI
        , supplementary'material_role         :: Maybe Xsd.XsdString
        , supplementary'material_show         :: Maybe Xsd.XsdString
        , supplementary'material_title        :: Maybe Xsd.XsdString
        , supplementary'material_type         :: Maybe Xsd.XsdString
        , supplementary'material_base         :: Maybe Xsd.AnyURI
        , supplementary'material_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Supplementary'material where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "mime-subtype" e pos
        a3 <- optional $ getAttribute "mimetype" e pos
        a4 <- optional $ getAttribute "orientation" e pos
        a5 <- optional $ getAttribute "position" e pos
        a6 <- optional $ getAttribute "specific-use" e pos
        a7 <- optional $ getAttribute "actuate" e pos
        a8 <- optional $ getAttribute "href" e pos
        a9 <- optional $ getAttribute "role" e pos
        a10 <- optional $ getAttribute "show" e pos
        a11 <- optional $ getAttribute "title" e pos
        a12 <- optional $ getAttribute "type" e pos
        a13 <- optional $ getAttribute "base" e pos
        a14 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Supplementary'material a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
    schemaTypeToXML s x@Supplementary'material{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ supplementary'material_content'type x
                       , maybe [] (toXMLAttribute "id") $ supplementary'material_id x
                       , maybe [] (toXMLAttribute "mime-subtype") $ supplementary'material_mime'subtype x
                       , maybe [] (toXMLAttribute "mimetype") $ supplementary'material_mimetype x
                       , maybe [] (toXMLAttribute "orientation") $ supplementary'material_orientation x
                       , maybe [] (toXMLAttribute "position") $ supplementary'material_position x
                       , maybe [] (toXMLAttribute "specific-use") $ supplementary'material_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ supplementary'material_actuate x
                       , maybe [] (toXMLAttribute "href") $ supplementary'material_href x
                       , maybe [] (toXMLAttribute "role") $ supplementary'material_role x
                       , maybe [] (toXMLAttribute "show") $ supplementary'material_show x
                       , maybe [] (toXMLAttribute "title") $ supplementary'material_title x
                       , maybe [] (toXMLAttribute "type") $ supplementary'material_type x
                       , maybe [] (toXMLAttribute "base") $ supplementary'material_base x
                       , maybe [] (toXMLAttribute "lang") $ supplementary'material_lang x
                       ]
            []

elementSupplementary'material :: XMLParser Supplementary'material
elementSupplementary'material = parseSchemaType "supplementary-material"
elementToXMLSupplementary'material :: Supplementary'material -> [Content ()]
elementToXMLSupplementary'material = schemaTypeToXML "supplementary-material"

data Surname = Surname
        { surname_id       :: Maybe Xsd.ID
        , surname_initials :: Maybe Xsd.XsdString
        , surname_base     :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Surname where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "initials" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Surname a0 a1 a2)
    schemaTypeToXML s x@Surname{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ surname_id x
                       , maybe [] (toXMLAttribute "initials") $ surname_initials x
                       , maybe [] (toXMLAttribute "base") $ surname_base x
                       ]
            []

elementSurname :: XMLParser Surname
elementSurname = parseSchemaType "surname"
elementToXMLSurname :: Surname -> [Content ()]
elementToXMLSurname = schemaTypeToXML "surname"

data Table = Table
        { table_border       :: Maybe Xsd.XsdString
        , table_cellpadding  :: Maybe Xsd.XsdString
        , table_cellspacing  :: Maybe Xsd.XsdString
        , table_content'type :: Maybe Xsd.XsdString
        , table_frame        :: Maybe Xsd.XsdString
        , table_id           :: Maybe Xsd.ID
        , table_rules        :: Maybe Xsd.XsdString
        , table_specific'use :: Maybe Xsd.XsdString
        , table_style        :: Maybe Xsd.XsdString
        , table_summary      :: Maybe Xsd.XsdString
        , table_width        :: Maybe Xsd.XsdString
        , table_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Table where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "border" e pos
        a1 <- optional $ getAttribute "cellpadding" e pos
        a2 <- optional $ getAttribute "cellspacing" e pos
        a3 <- optional $ getAttribute "content-type" e pos
        a4 <- optional $ getAttribute "frame" e pos
        a5 <- optional $ getAttribute "id" e pos
        a6 <- optional $ getAttribute "rules" e pos
        a7 <- optional $ getAttribute "specific-use" e pos
        a8 <- optional $ getAttribute "style" e pos
        a9 <- optional $ getAttribute "summary" e pos
        a10 <- optional $ getAttribute "width" e pos
        a11 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Table a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
    schemaTypeToXML s x@Table{} =
        toXMLElement s [ maybe [] (toXMLAttribute "border") $ table_border x
                       , maybe [] (toXMLAttribute "cellpadding") $ table_cellpadding x
                       , maybe [] (toXMLAttribute "cellspacing") $ table_cellspacing x
                       , maybe [] (toXMLAttribute "content-type") $ table_content'type x
                       , maybe [] (toXMLAttribute "frame") $ table_frame x
                       , maybe [] (toXMLAttribute "id") $ table_id x
                       , maybe [] (toXMLAttribute "rules") $ table_rules x
                       , maybe [] (toXMLAttribute "specific-use") $ table_specific'use x
                       , maybe [] (toXMLAttribute "style") $ table_style x
                       , maybe [] (toXMLAttribute "summary") $ table_summary x
                       , maybe [] (toXMLAttribute "width") $ table_width x
                       , maybe [] (toXMLAttribute "base") $ table_base x
                       ]
            []

elementTable :: XMLParser Table
elementTable = parseSchemaType "table"
elementToXMLTable :: Table -> [Content ()]
elementToXMLTable = schemaTypeToXML "table"

data Table'count = Table'count
        { table'count_count :: Xsd.NMTOKEN
        , table'count_id    :: Maybe Xsd.ID
        , table'count_base  :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Table'count where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "count" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Table'count a0 a1 a2)
    schemaTypeToXML s x@Table'count{} =
        toXMLElement s [ toXMLAttribute "count" $ table'count_count x
                       , maybe [] (toXMLAttribute "id") $ table'count_id x
                       , maybe [] (toXMLAttribute "base") $ table'count_base x
                       ]
            []

elementTable'count :: XMLParser Table'count
elementTable'count = parseSchemaType "table-count"
elementToXMLTable'count :: Table'count -> [Content ()]
elementToXMLTable'count = schemaTypeToXML "table-count"

data Table'wrap = Table'wrap
        { table'wrap_content'type :: Maybe Xsd.XsdString
        , table'wrap_id           :: Maybe Xsd.ID
        , table'wrap_orientation  :: Maybe Xsd.XsdString
        , table'wrap_position     :: Maybe Xsd.XsdString
        , table'wrap_specific'use :: Maybe Xsd.XsdString
        , table'wrap_base         :: Maybe Xsd.AnyURI
        , table'wrap_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Table'wrap where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "orientation" e pos
        a3 <- optional $ getAttribute "position" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Table'wrap a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Table'wrap{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ table'wrap_content'type x
                       , maybe [] (toXMLAttribute "id") $ table'wrap_id x
                       , maybe [] (toXMLAttribute "orientation") $ table'wrap_orientation x
                       , maybe [] (toXMLAttribute "position") $ table'wrap_position x
                       , maybe [] (toXMLAttribute "specific-use") $ table'wrap_specific'use x
                       , maybe [] (toXMLAttribute "base") $ table'wrap_base x
                       , maybe [] (toXMLAttribute "lang") $ table'wrap_lang x
                       ]
            []

elementTable'wrap :: XMLParser Table'wrap
elementTable'wrap = parseSchemaType "table-wrap"
elementToXMLTable'wrap :: Table'wrap -> [Content ()]
elementToXMLTable'wrap = schemaTypeToXML "table-wrap"

data Table'wrap'foot = Table'wrap'foot
        { table'wrap'foot_id   :: Maybe Xsd.ID
        , table'wrap'foot_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Table'wrap'foot where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Table'wrap'foot a0 a1)
    schemaTypeToXML s x@Table'wrap'foot{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ table'wrap'foot_id x
                       , maybe [] (toXMLAttribute "base") $ table'wrap'foot_base x
                       ]
            []

elementTable'wrap'foot :: XMLParser Table'wrap'foot
elementTable'wrap'foot = parseSchemaType "table-wrap-foot"
elementToXMLTable'wrap'foot :: Table'wrap'foot -> [Content ()]
elementToXMLTable'wrap'foot = schemaTypeToXML "table-wrap-foot"

data Table'wrap'group = Table'wrap'group
        { table'wrap'group_content'type :: Maybe Xsd.XsdString
        , table'wrap'group_id           :: Maybe Xsd.ID
        , table'wrap'group_orientation  :: Maybe Xsd.XsdString
        , table'wrap'group_position     :: Maybe Xsd.XsdString
        , table'wrap'group_specific'use :: Maybe Xsd.XsdString
        , table'wrap'group_base         :: Maybe Xsd.AnyURI
        , table'wrap'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Table'wrap'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "orientation" e pos
        a3 <- optional $ getAttribute "position" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Table'wrap'group a0 a1 a2 a3 a4 a5 a6)
    schemaTypeToXML s x@Table'wrap'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ table'wrap'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ table'wrap'group_id x
                       , maybe [] (toXMLAttribute "orientation") $ table'wrap'group_orientation x
                       , maybe [] (toXMLAttribute "position") $ table'wrap'group_position x
                       , maybe [] (toXMLAttribute "specific-use") $ table'wrap'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ table'wrap'group_base x
                       , maybe [] (toXMLAttribute "lang") $ table'wrap'group_lang x
                       ]
            []

elementTable'wrap'group :: XMLParser Table'wrap'group
elementTable'wrap'group = parseSchemaType "table-wrap-group"
elementToXMLTable'wrap'group :: Table'wrap'group -> [Content ()]
elementToXMLTable'wrap'group = schemaTypeToXML "table-wrap-group"

data Target = Target
        { target_id           :: Xsd.ID
        , target_specific'use :: Maybe Xsd.XsdString
        , target_target'type  :: Maybe Xsd.XsdString
        , target_base         :: Maybe Xsd.AnyURI
        , target_lang         :: Maybe Xsd.XsdString
        , target_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Target where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "target-type" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Target a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Target{} =
        toXMLElement s [ toXMLAttribute "id" $ target_id x
                       , maybe [] (toXMLAttribute "specific-use") $ target_specific'use x
                       , maybe [] (toXMLAttribute "target-type") $ target_target'type x
                       , maybe [] (toXMLAttribute "base") $ target_base x
                       , maybe [] (toXMLAttribute "lang") $ target_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ target_choice0 x
            ]

elementTarget :: XMLParser Target
elementTarget = parseSchemaType "target"
elementToXMLTarget :: Target -> [Content ()]
elementToXMLTarget = schemaTypeToXML "target"

data Tbody = Tbody
        { tbody_align        :: Maybe Xsd.XsdString
        , tbody_char         :: Maybe Xsd.XsdString
        , tbody_charoff      :: Maybe Xsd.XsdString
        , tbody_content'type :: Maybe Xsd.XsdString
        , tbody_id           :: Maybe Xsd.ID
        , tbody_style        :: Maybe Xsd.XsdString
        , tbody_valign       :: Maybe Xsd.XsdString
        , tbody_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Tbody where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "align" e pos
        a1 <- optional $ getAttribute "char" e pos
        a2 <- optional $ getAttribute "charoff" e pos
        a3 <- optional $ getAttribute "content-type" e pos
        a4 <- optional $ getAttribute "id" e pos
        a5 <- optional $ getAttribute "style" e pos
        a6 <- optional $ getAttribute "valign" e pos
        a7 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Tbody a0 a1 a2 a3 a4 a5 a6 a7)
    schemaTypeToXML s x@Tbody{} =
        toXMLElement s [ maybe [] (toXMLAttribute "align") $ tbody_align x
                       , maybe [] (toXMLAttribute "char") $ tbody_char x
                       , maybe [] (toXMLAttribute "charoff") $ tbody_charoff x
                       , maybe [] (toXMLAttribute "content-type") $ tbody_content'type x
                       , maybe [] (toXMLAttribute "id") $ tbody_id x
                       , maybe [] (toXMLAttribute "style") $ tbody_style x
                       , maybe [] (toXMLAttribute "valign") $ tbody_valign x
                       , maybe [] (toXMLAttribute "base") $ tbody_base x
                       ]
            []

elementTbody :: XMLParser Tbody
elementTbody = parseSchemaType "tbody"
elementToXMLTbody :: Tbody -> [Content ()]
elementToXMLTbody = schemaTypeToXML "tbody"

data Td = Td
        { td_abbr         :: Maybe Xsd.XsdString
        , td_align        :: Maybe Xsd.XsdString
        , td_axis         :: Maybe Xsd.XsdString
        , td_char         :: Maybe Xsd.XsdString
        , td_charoff      :: Maybe Xsd.XsdString
        , td_colspan      :: Maybe Xsd.XsdString
        , td_content'type :: Maybe Xsd.XsdString
        , td_headers      :: Maybe Xsd.IDREFS
        , td_id           :: Maybe Xsd.ID
        , td_rowspan      :: Maybe Xsd.XsdString
        , td_scope        :: Maybe Xsd.XsdString
        , td_style        :: Maybe Xsd.XsdString
        , td_valign       :: Maybe Xsd.XsdString
        , td_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Td where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "abbr" e pos
        a1 <- optional $ getAttribute "align" e pos
        a2 <- optional $ getAttribute "axis" e pos
        a3 <- optional $ getAttribute "char" e pos
        a4 <- optional $ getAttribute "charoff" e pos
        a5 <- optional $ getAttribute "colspan" e pos
        a6 <- optional $ getAttribute "content-type" e pos
        a7 <- optional $ getAttribute "headers" e pos
        a8 <- optional $ getAttribute "id" e pos
        a9 <- optional $ getAttribute "rowspan" e pos
        a10 <- optional $ getAttribute "scope" e pos
        a11 <- optional $ getAttribute "style" e pos
        a12 <- optional $ getAttribute "valign" e pos
        a13 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Td a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13)
    schemaTypeToXML s x@Td{} =
        toXMLElement s [ maybe [] (toXMLAttribute "abbr") $ td_abbr x
                       , maybe [] (toXMLAttribute "align") $ td_align x
                       , maybe [] (toXMLAttribute "axis") $ td_axis x
                       , maybe [] (toXMLAttribute "char") $ td_char x
                       , maybe [] (toXMLAttribute "charoff") $ td_charoff x
                       , maybe [] (toXMLAttribute "colspan") $ td_colspan x
                       , maybe [] (toXMLAttribute "content-type") $ td_content'type x
                       , maybe [] (toXMLAttribute "headers") $ td_headers x
                       , maybe [] (toXMLAttribute "id") $ td_id x
                       , maybe [] (toXMLAttribute "rowspan") $ td_rowspan x
                       , maybe [] (toXMLAttribute "scope") $ td_scope x
                       , maybe [] (toXMLAttribute "style") $ td_style x
                       , maybe [] (toXMLAttribute "valign") $ td_valign x
                       , maybe [] (toXMLAttribute "base") $ td_base x
                       ]
            []

elementTd :: XMLParser Td
elementTd = parseSchemaType "td"
elementToXMLTd :: Td -> [Content ()]
elementToXMLTd = schemaTypeToXML "td"

data Term = Term
        { term_id           :: Maybe Xsd.ID
        , term_rid          :: Maybe Xsd.IDREFS
        , term_specific'use :: Maybe Xsd.XsdString
        , term_base         :: Maybe Xsd.AnyURI
        , term_lang         :: Maybe Xsd.XsdString
        , term_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Term where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "rid" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Term a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Term{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ term_id x
                       , maybe [] (toXMLAttribute "rid") $ term_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ term_specific'use x
                       , maybe [] (toXMLAttribute "base") $ term_base x
                       , maybe [] (toXMLAttribute "lang") $ term_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ term_choice0 x
            ]

elementTerm :: XMLParser Term
elementTerm = parseSchemaType "term"
elementToXMLTerm :: Term -> [Content ()]
elementToXMLTerm = schemaTypeToXML "term"

data Term'head = Term'head
        { term'head_id      :: Maybe Xsd.ID
        , term'head_base    :: Maybe Xsd.AnyURI
        , term'head_choice0 :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Term'head where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Term'head a0 a1)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Term'head{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ term'head_id x
                       , maybe [] (toXMLAttribute "base") $ term'head_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ term'head_choice0 x
            ]

elementTerm'head :: XMLParser Term'head
elementTerm'head = parseSchemaType "term-head"
elementToXMLTerm'head :: Term'head -> [Content ()]
elementToXMLTerm'head = schemaTypeToXML "term-head"

data Tex'math = Tex'math
        { tex'math_content'type :: Maybe Xsd.XsdString
        , tex'math_id           :: Maybe Xsd.ID
        , tex'math_notation     :: Maybe Xsd.XsdString
        , tex'math_specific'use :: Maybe Xsd.XsdString
        , tex'math_version      :: Maybe Xsd.XsdString
        , tex'math_base         :: Maybe Xsd.AnyURI
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Tex'math where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "notation" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "version" e pos
        a5 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Tex'math a0 a1 a2 a3 a4 a5)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Tex'math{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ tex'math_content'type x
                       , maybe [] (toXMLAttribute "id") $ tex'math_id x
                       , maybe [] (toXMLAttribute "notation") $ tex'math_notation x
                       , maybe [] (toXMLAttribute "specific-use") $ tex'math_specific'use x
                       , maybe [] (toXMLAttribute "version") $ tex'math_version x
                       , maybe [] (toXMLAttribute "base") $ tex'math_base x
                       ]
            [
            ]

elementTex'math :: XMLParser Tex'math
elementTex'math = parseSchemaType "tex-math"
elementToXMLTex'math :: Tex'math -> [Content ()]
elementToXMLTex'math = schemaTypeToXML "tex-math"

data Textual'form = Textual'form
        { textual'form_id           :: Maybe Xsd.ID
        , textual'form_specific'use :: Maybe Xsd.XsdString
        , textual'form_base         :: Maybe Xsd.AnyURI
        , textual'form_lang         :: Maybe Xsd.XsdString
        , textual'form_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Textual'form where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Textual'form a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Textual'form{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ textual'form_id x
                       , maybe [] (toXMLAttribute "specific-use") $ textual'form_specific'use x
                       , maybe [] (toXMLAttribute "base") $ textual'form_base x
                       , maybe [] (toXMLAttribute "lang") $ textual'form_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ textual'form_choice0 x
            ]

elementTextual'form :: XMLParser Textual'form
elementTextual'form = parseSchemaType "textual-form"
elementToXMLTextual'form :: Textual'form -> [Content ()]
elementToXMLTextual'form = schemaTypeToXML "textual-form"

data Tfoot = Tfoot
        { tfoot_align        :: Maybe Xsd.XsdString
        , tfoot_char         :: Maybe Xsd.XsdString
        , tfoot_charoff      :: Maybe Xsd.XsdString
        , tfoot_content'type :: Maybe Xsd.XsdString
        , tfoot_id           :: Maybe Xsd.ID
        , tfoot_style        :: Maybe Xsd.XsdString
        , tfoot_valign       :: Maybe Xsd.XsdString
        , tfoot_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Tfoot where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "align" e pos
        a1 <- optional $ getAttribute "char" e pos
        a2 <- optional $ getAttribute "charoff" e pos
        a3 <- optional $ getAttribute "content-type" e pos
        a4 <- optional $ getAttribute "id" e pos
        a5 <- optional $ getAttribute "style" e pos
        a6 <- optional $ getAttribute "valign" e pos
        a7 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Tfoot a0 a1 a2 a3 a4 a5 a6 a7)
    schemaTypeToXML s x@Tfoot{} =
        toXMLElement s [ maybe [] (toXMLAttribute "align") $ tfoot_align x
                       , maybe [] (toXMLAttribute "char") $ tfoot_char x
                       , maybe [] (toXMLAttribute "charoff") $ tfoot_charoff x
                       , maybe [] (toXMLAttribute "content-type") $ tfoot_content'type x
                       , maybe [] (toXMLAttribute "id") $ tfoot_id x
                       , maybe [] (toXMLAttribute "style") $ tfoot_style x
                       , maybe [] (toXMLAttribute "valign") $ tfoot_valign x
                       , maybe [] (toXMLAttribute "base") $ tfoot_base x
                       ]
            []

elementTfoot :: XMLParser Tfoot
elementTfoot = parseSchemaType "tfoot"
elementToXMLTfoot :: Tfoot -> [Content ()]
elementToXMLTfoot = schemaTypeToXML "tfoot"

data Th = Th
        { th_abbr         :: Maybe Xsd.XsdString
        , th_align        :: Maybe Xsd.XsdString
        , th_axis         :: Maybe Xsd.XsdString
        , th_char         :: Maybe Xsd.XsdString
        , th_charoff      :: Maybe Xsd.XsdString
        , th_colspan      :: Maybe Xsd.XsdString
        , th_content'type :: Maybe Xsd.XsdString
        , th_headers      :: Maybe Xsd.IDREFS
        , th_id           :: Maybe Xsd.ID
        , th_rowspan      :: Maybe Xsd.XsdString
        , th_scope        :: Maybe Xsd.XsdString
        , th_style        :: Maybe Xsd.XsdString
        , th_valign       :: Maybe Xsd.XsdString
        , th_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Th where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "abbr" e pos
        a1 <- optional $ getAttribute "align" e pos
        a2 <- optional $ getAttribute "axis" e pos
        a3 <- optional $ getAttribute "char" e pos
        a4 <- optional $ getAttribute "charoff" e pos
        a5 <- optional $ getAttribute "colspan" e pos
        a6 <- optional $ getAttribute "content-type" e pos
        a7 <- optional $ getAttribute "headers" e pos
        a8 <- optional $ getAttribute "id" e pos
        a9 <- optional $ getAttribute "rowspan" e pos
        a10 <- optional $ getAttribute "scope" e pos
        a11 <- optional $ getAttribute "style" e pos
        a12 <- optional $ getAttribute "valign" e pos
        a13 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Th a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13)
    schemaTypeToXML s x@Th{} =
        toXMLElement s [ maybe [] (toXMLAttribute "abbr") $ th_abbr x
                       , maybe [] (toXMLAttribute "align") $ th_align x
                       , maybe [] (toXMLAttribute "axis") $ th_axis x
                       , maybe [] (toXMLAttribute "char") $ th_char x
                       , maybe [] (toXMLAttribute "charoff") $ th_charoff x
                       , maybe [] (toXMLAttribute "colspan") $ th_colspan x
                       , maybe [] (toXMLAttribute "content-type") $ th_content'type x
                       , maybe [] (toXMLAttribute "headers") $ th_headers x
                       , maybe [] (toXMLAttribute "id") $ th_id x
                       , maybe [] (toXMLAttribute "rowspan") $ th_rowspan x
                       , maybe [] (toXMLAttribute "scope") $ th_scope x
                       , maybe [] (toXMLAttribute "style") $ th_style x
                       , maybe [] (toXMLAttribute "valign") $ th_valign x
                       , maybe [] (toXMLAttribute "base") $ th_base x
                       ]
            []

elementTh :: XMLParser Th
elementTh = parseSchemaType "th"
elementToXMLTh :: Th -> [Content ()]
elementToXMLTh = schemaTypeToXML "th"

data Thead = Thead
        { thead_align        :: Maybe Xsd.XsdString
        , thead_char         :: Maybe Xsd.XsdString
        , thead_charoff      :: Maybe Xsd.XsdString
        , thead_content'type :: Maybe Xsd.XsdString
        , thead_id           :: Maybe Xsd.ID
        , thead_style        :: Maybe Xsd.XsdString
        , thead_valign       :: Maybe Xsd.XsdString
        , thead_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Thead where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "align" e pos
        a1 <- optional $ getAttribute "char" e pos
        a2 <- optional $ getAttribute "charoff" e pos
        a3 <- optional $ getAttribute "content-type" e pos
        a4 <- optional $ getAttribute "id" e pos
        a5 <- optional $ getAttribute "style" e pos
        a6 <- optional $ getAttribute "valign" e pos
        a7 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Thead a0 a1 a2 a3 a4 a5 a6 a7)
    schemaTypeToXML s x@Thead{} =
        toXMLElement s [ maybe [] (toXMLAttribute "align") $ thead_align x
                       , maybe [] (toXMLAttribute "char") $ thead_char x
                       , maybe [] (toXMLAttribute "charoff") $ thead_charoff x
                       , maybe [] (toXMLAttribute "content-type") $ thead_content'type x
                       , maybe [] (toXMLAttribute "id") $ thead_id x
                       , maybe [] (toXMLAttribute "style") $ thead_style x
                       , maybe [] (toXMLAttribute "valign") $ thead_valign x
                       , maybe [] (toXMLAttribute "base") $ thead_base x
                       ]
            []

elementThead :: XMLParser Thead
elementThead = parseSchemaType "thead"
elementToXMLThead :: Thead -> [Content ()]
elementToXMLThead = schemaTypeToXML "thead"

data Time'stamp = Time'stamp
        { time'stamp_content'type :: Maybe Xsd.XsdString
        , time'stamp_id           :: Maybe Xsd.ID
        , time'stamp_specific'use :: Maybe Xsd.XsdString
        , time'stamp_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Time'stamp where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Time'stamp a0 a1 a2 a3)
    schemaTypeToXML s x@Time'stamp{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ time'stamp_content'type x
                       , maybe [] (toXMLAttribute "id") $ time'stamp_id x
                       , maybe [] (toXMLAttribute "specific-use") $ time'stamp_specific'use x
                       , maybe [] (toXMLAttribute "base") $ time'stamp_base x
                       ]
            []

elementTime'stamp :: XMLParser Time'stamp
elementTime'stamp = parseSchemaType "time-stamp"
elementToXMLTime'stamp :: Time'stamp -> [Content ()]
elementToXMLTime'stamp = schemaTypeToXML "time-stamp"

data Title = Title
        { title_content'type :: Maybe Xsd.XsdString
        , title_id           :: Maybe Xsd.ID
        , title_specific'use :: Maybe Xsd.XsdString
        , title_base         :: Maybe Xsd.AnyURI
        , title_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Title a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ title_content'type x
                       , maybe [] (toXMLAttribute "id") $ title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ title_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ title_choice0 x
            ]

elementTitle :: XMLParser Title
elementTitle = parseSchemaType "title"
elementToXMLTitle :: Title -> [Content ()]
elementToXMLTitle = schemaTypeToXML "title"

data Title'group = Title'group
        { title'group_id   :: Maybe Xsd.ID
        , title'group_base :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Title'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Title'group a0 a1)
    schemaTypeToXML s x@Title'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ title'group_id x
                       , maybe [] (toXMLAttribute "base") $ title'group_base x
                       ]
            []

elementTitle'group :: XMLParser Title'group
elementTitle'group = parseSchemaType "title-group"
elementToXMLTitle'group :: Title'group -> [Content ()]
elementToXMLTitle'group = schemaTypeToXML "title-group"

data Tr = Tr
        { tr_align        :: Maybe Xsd.XsdString
        , tr_char         :: Maybe Xsd.XsdString
        , tr_charoff      :: Maybe Xsd.XsdString
        , tr_content'type :: Maybe Xsd.XsdString
        , tr_id           :: Maybe Xsd.ID
        , tr_style        :: Maybe Xsd.XsdString
        , tr_valign       :: Maybe Xsd.XsdString
        , tr_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Tr where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "align" e pos
        a1 <- optional $ getAttribute "char" e pos
        a2 <- optional $ getAttribute "charoff" e pos
        a3 <- optional $ getAttribute "content-type" e pos
        a4 <- optional $ getAttribute "id" e pos
        a5 <- optional $ getAttribute "style" e pos
        a6 <- optional $ getAttribute "valign" e pos
        a7 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Tr a0 a1 a2 a3 a4 a5 a6 a7)
    schemaTypeToXML s x@Tr{} =
        toXMLElement s [ maybe [] (toXMLAttribute "align") $ tr_align x
                       , maybe [] (toXMLAttribute "char") $ tr_char x
                       , maybe [] (toXMLAttribute "charoff") $ tr_charoff x
                       , maybe [] (toXMLAttribute "content-type") $ tr_content'type x
                       , maybe [] (toXMLAttribute "id") $ tr_id x
                       , maybe [] (toXMLAttribute "style") $ tr_style x
                       , maybe [] (toXMLAttribute "valign") $ tr_valign x
                       , maybe [] (toXMLAttribute "base") $ tr_base x
                       ]
            []

elementTr :: XMLParser Tr
elementTr = parseSchemaType "tr"
elementToXMLTr :: Tr -> [Content ()]
elementToXMLTr = schemaTypeToXML "tr"

data Trans'abstract = Trans'abstract
        { trans'abstract_abstract'type :: Maybe Xsd.XsdString
        , trans'abstract_id            :: Maybe Xsd.ID
        , trans'abstract_specific'use  :: Maybe Xsd.XsdString
        , trans'abstract_base          :: Maybe Xsd.AnyURI
        , trans'abstract_lang          :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Trans'abstract where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "abstract-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Trans'abstract a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Trans'abstract{} =
        toXMLElement s [ maybe [] (toXMLAttribute "abstract-type") $ trans'abstract_abstract'type x
                       , maybe [] (toXMLAttribute "id") $ trans'abstract_id x
                       , maybe [] (toXMLAttribute "specific-use") $ trans'abstract_specific'use x
                       , maybe [] (toXMLAttribute "base") $ trans'abstract_base x
                       , maybe [] (toXMLAttribute "lang") $ trans'abstract_lang x
                       ]
            []

elementTrans'abstract :: XMLParser Trans'abstract
elementTrans'abstract = parseSchemaType "trans-abstract"
elementToXMLTrans'abstract :: Trans'abstract -> [Content ()]
elementToXMLTrans'abstract = schemaTypeToXML "trans-abstract"

data Trans'source = Trans'source
        { trans'source_content'type :: Maybe Xsd.XsdString
        , trans'source_id           :: Maybe Xsd.ID
        , trans'source_specific'use :: Maybe Xsd.XsdString
        , trans'source_base         :: Maybe Xsd.AnyURI
        , trans'source_lang         :: Maybe Xsd.XsdString
        , trans'source_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Trans'source where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Trans'source a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Trans'source{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ trans'source_content'type x
                       , maybe [] (toXMLAttribute "id") $ trans'source_id x
                       , maybe [] (toXMLAttribute "specific-use") $ trans'source_specific'use x
                       , maybe [] (toXMLAttribute "base") $ trans'source_base x
                       , maybe [] (toXMLAttribute "lang") $ trans'source_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ trans'source_choice0 x
            ]

elementTrans'source :: XMLParser Trans'source
elementTrans'source = parseSchemaType "trans-source"
elementToXMLTrans'source :: Trans'source -> [Content ()]
elementToXMLTrans'source = schemaTypeToXML "trans-source"

data Trans'subtitle = Trans'subtitle
        { trans'subtitle_id           :: Maybe Xsd.ID
        , trans'subtitle_specific'use :: Maybe Xsd.XsdString
        , trans'subtitle_base         :: Maybe Xsd.AnyURI
        , trans'subtitle_lang         :: Maybe Xsd.XsdString
        , trans'subtitle_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Trans'subtitle where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        a3 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Trans'subtitle a0 a1 a2 a3)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Trans'subtitle{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ trans'subtitle_id x
                       , maybe [] (toXMLAttribute "specific-use") $ trans'subtitle_specific'use x
                       , maybe [] (toXMLAttribute "base") $ trans'subtitle_base x
                       , maybe [] (toXMLAttribute "lang") $ trans'subtitle_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ trans'subtitle_choice0 x
            ]

elementTrans'subtitle :: XMLParser Trans'subtitle
elementTrans'subtitle = parseSchemaType "trans-subtitle"
elementToXMLTrans'subtitle :: Trans'subtitle -> [Content ()]
elementToXMLTrans'subtitle = schemaTypeToXML "trans-subtitle"

data Trans'title = Trans'title
        { trans'title_content'type :: Maybe Xsd.XsdString
        , trans'title_id           :: Maybe Xsd.ID
        , trans'title_specific'use :: Maybe Xsd.XsdString
        , trans'title_base         :: Maybe Xsd.AnyURI
        , trans'title_lang         :: Maybe Xsd.XsdString
        , trans'title_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Trans'title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Trans'title a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Trans'title{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ trans'title_content'type x
                       , maybe [] (toXMLAttribute "id") $ trans'title_id x
                       , maybe [] (toXMLAttribute "specific-use") $ trans'title_specific'use x
                       , maybe [] (toXMLAttribute "base") $ trans'title_base x
                       , maybe [] (toXMLAttribute "lang") $ trans'title_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ trans'title_choice0 x
            ]

elementTrans'title :: XMLParser Trans'title
elementTrans'title = parseSchemaType "trans-title"
elementToXMLTrans'title :: Trans'title -> [Content ()]
elementToXMLTrans'title = schemaTypeToXML "trans-title"

data Trans'title'group = Trans'title'group
        { trans'title'group_content'type :: Maybe Xsd.XsdString
        , trans'title'group_id           :: Maybe Xsd.ID
        , trans'title'group_specific'use :: Maybe Xsd.XsdString
        , trans'title'group_base         :: Maybe Xsd.AnyURI
        , trans'title'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Trans'title'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Trans'title'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Trans'title'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ trans'title'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ trans'title'group_id x
                       , maybe [] (toXMLAttribute "specific-use") $ trans'title'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ trans'title'group_base x
                       , maybe [] (toXMLAttribute "lang") $ trans'title'group_lang x
                       ]
            []

elementTrans'title'group :: XMLParser Trans'title'group
elementTrans'title'group = parseSchemaType "trans-title-group"
elementToXMLTrans'title'group :: Trans'title'group -> [Content ()]
elementToXMLTrans'title'group = schemaTypeToXML "trans-title-group"

data Underline = Underline
        { underline_id              :: Maybe Xsd.ID
        , underline_specific'use    :: Maybe Xsd.XsdString
        , underline_toggle          :: Maybe Xsd.XsdString
        , underline_underline'style :: Maybe Xsd.XsdString
        , underline_base            :: Maybe Xsd.AnyURI
        , underline_choice0         :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Underline where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "toggle" e pos
        a3 <- optional $ getAttribute "underline-style" e pos
        a4 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Underline a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Underline{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ underline_id x
                       , maybe [] (toXMLAttribute "specific-use") $ underline_specific'use x
                       , maybe [] (toXMLAttribute "toggle") $ underline_toggle x
                       , maybe [] (toXMLAttribute "underline-style") $ underline_underline'style x
                       , maybe [] (toXMLAttribute "base") $ underline_base x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ underline_choice0 x
            ]

elementUnderline :: XMLParser Underline
elementUnderline = parseSchemaType "underline"
elementToXMLUnderline :: Underline -> [Content ()]
elementToXMLUnderline = schemaTypeToXML "underline"

data Underline'end = Underline'end
        { underline'end_id           :: Maybe Xsd.ID
        , underline'end_rid          :: Xsd.IDREF
        , underline'end_specific'use :: Maybe Xsd.XsdString
        , underline'end_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Underline'end where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- getAttribute "rid" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Underline'end a0 a1 a2 a3)
    schemaTypeToXML s x@Underline'end{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ underline'end_id x
                       , toXMLAttribute "rid" $ underline'end_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ underline'end_specific'use x
                       , maybe [] (toXMLAttribute "base") $ underline'end_base x
                       ]
            []

elementUnderline'end :: XMLParser Underline'end
elementUnderline'end = parseSchemaType "underline-end"
elementToXMLUnderline'end :: Underline'end -> [Content ()]
elementToXMLUnderline'end = schemaTypeToXML "underline-end"

data Underline'start = Underline'start
        { underline'start_id           :: Xsd.ID
        , underline'start_specific'use :: Maybe Xsd.XsdString
        , underline'start_base         :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Underline'start where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "id" e pos
        a1 <- optional $ getAttribute "specific-use" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Underline'start a0 a1 a2)
    schemaTypeToXML s x@Underline'start{} =
        toXMLElement s [ toXMLAttribute "id" $ underline'start_id x
                       , maybe [] (toXMLAttribute "specific-use") $ underline'start_specific'use x
                       , maybe [] (toXMLAttribute "base") $ underline'start_base x
                       ]
            []

elementUnderline'start :: XMLParser Underline'start
elementUnderline'start = parseSchemaType "underline-start"
elementToXMLUnderline'start :: Underline'start -> [Content ()]
elementToXMLUnderline'start = schemaTypeToXML "underline-start"

data Unstructured'kwd'group = Unstructured'kwd'group
        { unstructured'kwd'group_id             :: Maybe Xsd.ID
        , unstructured'kwd'group_kwd'group'type :: Maybe Xsd.XsdString
        , unstructured'kwd'group_specific'use   :: Maybe Xsd.XsdString
        , unstructured'kwd'group_base           :: Maybe Xsd.AnyURI
        , unstructured'kwd'group_lang           :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Unstructured'kwd'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "kwd-group-type" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Unstructured'kwd'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Unstructured'kwd'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ unstructured'kwd'group_id x
                       , maybe [] (toXMLAttribute "kwd-group-type") $ unstructured'kwd'group_kwd'group'type x
                       , maybe [] (toXMLAttribute "specific-use") $ unstructured'kwd'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ unstructured'kwd'group_base x
                       , maybe [] (toXMLAttribute "lang") $ unstructured'kwd'group_lang x
                       ]
            []

elementUnstructured'kwd'group :: XMLParser Unstructured'kwd'group
elementUnstructured'kwd'group = parseSchemaType "unstructured-kwd-group"
elementToXMLUnstructured'kwd'group :: Unstructured'kwd'group -> [Content ()]
elementToXMLUnstructured'kwd'group = schemaTypeToXML "unstructured-kwd-group"

data Uri = Uri
        { uri_content'type :: Maybe Xsd.XsdString
        , uri_id           :: Maybe Xsd.ID
        , uri_specific'use :: Maybe Xsd.XsdString
        , uri_actuate      :: Maybe Xsd.XsdString
        , uri_href         :: Maybe Xsd.AnyURI
        , uri_role         :: Maybe Xsd.XsdString
        , uri_show         :: Maybe Xsd.XsdString
        , uri_title        :: Maybe Xsd.XsdString
        , uri_type         :: Maybe Xsd.XsdString
        , uri_base         :: Maybe Xsd.AnyURI
        , uri_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Uri where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "actuate" e pos
        a4 <- optional $ getAttribute "href" e pos
        a5 <- optional $ getAttribute "role" e pos
        a6 <- optional $ getAttribute "show" e pos
        a7 <- optional $ getAttribute "title" e pos
        a8 <- optional $ getAttribute "type" e pos
        a9 <- optional $ getAttribute "base" e pos
        a10 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Uri a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    schemaTypeToXML s x@Uri{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ uri_content'type x
                       , maybe [] (toXMLAttribute "id") $ uri_id x
                       , maybe [] (toXMLAttribute "specific-use") $ uri_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ uri_actuate x
                       , maybe [] (toXMLAttribute "href") $ uri_href x
                       , maybe [] (toXMLAttribute "role") $ uri_role x
                       , maybe [] (toXMLAttribute "show") $ uri_show x
                       , maybe [] (toXMLAttribute "title") $ uri_title x
                       , maybe [] (toXMLAttribute "type") $ uri_type x
                       , maybe [] (toXMLAttribute "base") $ uri_base x
                       , maybe [] (toXMLAttribute "lang") $ uri_lang x
                       ]
            []

elementUri :: XMLParser Uri
elementUri = parseSchemaType "uri"
elementToXMLUri :: Uri -> [Content ()]
elementToXMLUri = schemaTypeToXML "uri"

data Verse'group = Verse'group
        { verse'group_content'type :: Maybe Xsd.XsdString
        , verse'group_id           :: Maybe Xsd.ID
        , verse'group_specific'use :: Maybe Xsd.XsdString
        , verse'group_base         :: Maybe Xsd.AnyURI
        , verse'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Verse'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Verse'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Verse'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ verse'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ verse'group_id x
                       , maybe [] (toXMLAttribute "specific-use") $ verse'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ verse'group_base x
                       , maybe [] (toXMLAttribute "lang") $ verse'group_lang x
                       ]
            []

elementVerse'group :: XMLParser Verse'group
elementVerse'group = parseSchemaType "verse-group"
elementToXMLVerse'group :: Verse'group -> [Content ()]
elementToXMLVerse'group = schemaTypeToXML "verse-group"

data Verse'line = Verse'line
        { verse'line_content'type :: Maybe Xsd.XsdString
        , verse'line_id           :: Maybe Xsd.ID
        , verse'line_specific'use :: Maybe Xsd.XsdString
        , verse'line_base         :: Maybe Xsd.AnyURI
        , verse'line_lang         :: Maybe Xsd.XsdString
        , verse'line_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Verse'line where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Verse'line a0 a1 a2 a3 a4)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Verse'line{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ verse'line_content'type x
                       , maybe [] (toXMLAttribute "id") $ verse'line_id x
                       , maybe [] (toXMLAttribute "specific-use") $ verse'line_specific'use x
                       , maybe [] (toXMLAttribute "base") $ verse'line_base x
                       , maybe [] (toXMLAttribute "lang") $ verse'line_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ verse'line_choice0 x
            ]

elementVerse'line :: XMLParser Verse'line
elementVerse'line = parseSchemaType "verse-line"
elementToXMLVerse'line :: Verse'line -> [Content ()]
elementToXMLVerse'line = schemaTypeToXML "verse-line"

data Version = Version
        { version_content'type :: Maybe Xsd.XsdString
        , version_designator   :: Maybe Xsd.XsdString
        , version_id           :: Maybe Xsd.ID
        , version_specific'use :: Maybe Xsd.XsdString
        , version_base         :: Maybe Xsd.AnyURI
        , version_lang         :: Maybe Xsd.XsdString
        , version_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Version where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "designator" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Version a0 a1 a2 a3 a4 a5)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Version{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ version_content'type x
                       , maybe [] (toXMLAttribute "designator") $ version_designator x
                       , maybe [] (toXMLAttribute "id") $ version_id x
                       , maybe [] (toXMLAttribute "specific-use") $ version_specific'use x
                       , maybe [] (toXMLAttribute "base") $ version_base x
                       , maybe [] (toXMLAttribute "lang") $ version_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ version_choice0 x
            ]

elementVersion :: XMLParser Version
elementVersion = parseSchemaType "version"
elementToXMLVersion :: Version -> [Content ()]
elementToXMLVersion = schemaTypeToXML "version"

data Volume = Volume
        { volume_content'type :: Maybe Xsd.XsdString
        , volume_id           :: Maybe Xsd.ID
        , volume_seq          :: Maybe Xsd.XsdString
        , volume_specific'use :: Maybe Xsd.XsdString
        , volume_base         :: Maybe Xsd.AnyURI
        , volume_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Volume where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "seq" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "base" e pos
        a5 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Volume a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@Volume{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ volume_content'type x
                       , maybe [] (toXMLAttribute "id") $ volume_id x
                       , maybe [] (toXMLAttribute "seq") $ volume_seq x
                       , maybe [] (toXMLAttribute "specific-use") $ volume_specific'use x
                       , maybe [] (toXMLAttribute "base") $ volume_base x
                       , maybe [] (toXMLAttribute "lang") $ volume_lang x
                       ]
            []

elementVolume :: XMLParser Volume
elementVolume = parseSchemaType "volume"
elementToXMLVolume :: Volume -> [Content ()]
elementToXMLVolume = schemaTypeToXML "volume"

data Volume'id = Volume'id
        { volume'id_content'type :: Maybe Xsd.XsdString
        , volume'id_id           :: Maybe Xsd.ID
        , volume'id_pub'id'type  :: Maybe Xsd.XsdString
        , volume'id_specific'use :: Maybe Xsd.XsdString
        , volume'id_actuate      :: Maybe Xsd.XsdString
        , volume'id_href         :: Maybe Xsd.AnyURI
        , volume'id_role         :: Maybe Xsd.XsdString
        , volume'id_show         :: Maybe Xsd.XsdString
        , volume'id_title        :: Maybe Xsd.XsdString
        , volume'id_type         :: Maybe Xsd.XsdString
        , volume'id_base         :: Maybe Xsd.AnyURI
        , volume'id_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Volume'id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "pub-id-type" e pos
        a3 <- optional $ getAttribute "specific-use" e pos
        a4 <- optional $ getAttribute "actuate" e pos
        a5 <- optional $ getAttribute "href" e pos
        a6 <- optional $ getAttribute "role" e pos
        a7 <- optional $ getAttribute "show" e pos
        a8 <- optional $ getAttribute "title" e pos
        a9 <- optional $ getAttribute "type" e pos
        a10 <- optional $ getAttribute "base" e pos
        a11 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Volume'id a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
    schemaTypeToXML s x@Volume'id{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ volume'id_content'type x
                       , maybe [] (toXMLAttribute "id") $ volume'id_id x
                       , maybe [] (toXMLAttribute "pub-id-type") $ volume'id_pub'id'type x
                       , maybe [] (toXMLAttribute "specific-use") $ volume'id_specific'use x
                       , maybe [] (toXMLAttribute "actuate") $ volume'id_actuate x
                       , maybe [] (toXMLAttribute "href") $ volume'id_href x
                       , maybe [] (toXMLAttribute "role") $ volume'id_role x
                       , maybe [] (toXMLAttribute "show") $ volume'id_show x
                       , maybe [] (toXMLAttribute "title") $ volume'id_title x
                       , maybe [] (toXMLAttribute "type") $ volume'id_type x
                       , maybe [] (toXMLAttribute "base") $ volume'id_base x
                       , maybe [] (toXMLAttribute "lang") $ volume'id_lang x
                       ]
            []

elementVolume'id :: XMLParser Volume'id
elementVolume'id = parseSchemaType "volume-id"
elementToXMLVolume'id :: Volume'id -> [Content ()]
elementToXMLVolume'id = schemaTypeToXML "volume-id"

data Volume'issue'group = Volume'issue'group
        { volume'issue'group_content'type :: Maybe Xsd.XsdString
        , volume'issue'group_id           :: Maybe Xsd.ID
        , volume'issue'group_specific'use :: Maybe Xsd.XsdString
        , volume'issue'group_base         :: Maybe Xsd.AnyURI
        , volume'issue'group_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Volume'issue'group where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Volume'issue'group a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Volume'issue'group{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ volume'issue'group_content'type x
                       , maybe [] (toXMLAttribute "id") $ volume'issue'group_id x
                       , maybe [] (toXMLAttribute "specific-use") $ volume'issue'group_specific'use x
                       , maybe [] (toXMLAttribute "base") $ volume'issue'group_base x
                       , maybe [] (toXMLAttribute "lang") $ volume'issue'group_lang x
                       ]
            []

elementVolume'issue'group :: XMLParser Volume'issue'group
elementVolume'issue'group = parseSchemaType "volume-issue-group"
elementToXMLVolume'issue'group :: Volume'issue'group -> [Content ()]
elementToXMLVolume'issue'group = schemaTypeToXML "volume-issue-group"

data Volume'series = Volume'series
        { volume'series_content'type :: Maybe Xsd.XsdString
        , volume'series_id           :: Maybe Xsd.ID
        , volume'series_specific'use :: Maybe Xsd.XsdString
        , volume'series_base         :: Maybe Xsd.AnyURI
        , volume'series_lang         :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Volume'series where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Volume'series a0 a1 a2 a3 a4)
    schemaTypeToXML s x@Volume'series{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ volume'series_content'type x
                       , maybe [] (toXMLAttribute "id") $ volume'series_id x
                       , maybe [] (toXMLAttribute "specific-use") $ volume'series_specific'use x
                       , maybe [] (toXMLAttribute "base") $ volume'series_base x
                       , maybe [] (toXMLAttribute "lang") $ volume'series_lang x
                       ]
            []

elementVolume'series :: XMLParser Volume'series
elementVolume'series = parseSchemaType "volume-series"
elementToXMLVolume'series :: Volume'series -> [Content ()]
elementToXMLVolume'series = schemaTypeToXML "volume-series"

data Word'count = Word'count
        { word'count_count :: Xsd.NMTOKEN
        , word'count_id    :: Maybe Xsd.ID
        , word'count_base  :: Maybe Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType Word'count where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "count" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "base" e pos
        commit $ interior e $ return (Word'count a0 a1 a2)
    schemaTypeToXML s x@Word'count{} =
        toXMLElement s [ toXMLAttribute "count" $ word'count_count x
                       , maybe [] (toXMLAttribute "id") $ word'count_id x
                       , maybe [] (toXMLAttribute "base") $ word'count_base x
                       ]
            []

elementWord'count :: XMLParser Word'count
elementWord'count = parseSchemaType "word-count"
elementToXMLWord'count :: Word'count -> [Content ()]
elementToXMLWord'count = schemaTypeToXML "word-count"

data X = X
        { x_content'type :: Maybe Xsd.XsdString
        , x_id           :: Maybe Xsd.ID
        , x_specific'use :: Maybe Xsd.XsdString
        , x_base         :: Maybe Xsd.AnyURI
        , x_lang         :: Maybe Xsd.XsdString
        , x_space        :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType X where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "content-type" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "specific-use" e pos
        a3 <- optional $ getAttribute "base" e pos
        a4 <- optional $ getAttribute "lang" e pos
        a5 <- optional $ getAttribute "space" e pos
        commit $ interior e $ return (X a0 a1 a2 a3 a4 a5)
    schemaTypeToXML s x@X{} =
        toXMLElement s [ maybe [] (toXMLAttribute "content-type") $ x_content'type x
                       , maybe [] (toXMLAttribute "id") $ x_id x
                       , maybe [] (toXMLAttribute "specific-use") $ x_specific'use x
                       , maybe [] (toXMLAttribute "base") $ x_base x
                       , maybe [] (toXMLAttribute "lang") $ x_lang x
                       , maybe [] (toXMLAttribute "space") $ x_space x
                       ]
            []

elementX :: XMLParser X
elementX = parseSchemaType "x"
elementToXMLX :: X -> [Content ()]
elementToXMLX = schemaTypeToXML "x"

data Xref = Xref
        { xref_alt          :: Maybe Xsd.XsdString
        , xref_id           :: Maybe Xsd.ID
        , xref_ref'type     :: Maybe Xsd.XsdString
        , xref_rid          :: Maybe Xsd.IDREFS
        , xref_specific'use :: Maybe Xsd.XsdString
        , xref_base         :: Maybe Xsd.AnyURI
        , xref_lang         :: Maybe Xsd.XsdString
        , xref_choice0      :: [OneOf1 ()]
          -- ^ Choice between:
          --
          --   (1) Sequence of:
        }
        deriving (Eq,Show)
instance SchemaType Xref where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "alt" e pos
        a1 <- optional $ getAttribute "id" e pos
        a2 <- optional $ getAttribute "ref-type" e pos
        a3 <- optional $ getAttribute "rid" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Xref a0 a1 a2 a3 a4 a5 a6)
            `apply` many (oneOf' [ -- ("", fmap OneOf1 ())
                                 ])
    schemaTypeToXML s x@Xref{} =
        toXMLElement s [ maybe [] (toXMLAttribute "alt") $ xref_alt x
                       , maybe [] (toXMLAttribute "id") $ xref_id x
                       , maybe [] (toXMLAttribute "ref-type") $ xref_ref'type x
                       , maybe [] (toXMLAttribute "rid") $ xref_rid x
                       , maybe [] (toXMLAttribute "specific-use") $ xref_specific'use x
                       , maybe [] (toXMLAttribute "base") $ xref_base x
                       , maybe [] (toXMLAttribute "lang") $ xref_lang x
                       ]
            [ -- concatMap (foldOneOf1  ()
              --                       ) $ xref_choice0 x
            ]

elementXref :: XMLParser Xref
elementXref = parseSchemaType "xref"
elementToXMLXref :: Xref -> [Content ()]
elementToXMLXref = schemaTypeToXML "xref"

data Year = Year
        { year_calendar      :: Maybe Xsd.XsdString
        , year_content'type  :: Maybe Xsd.XsdString
        , year_id            :: Maybe Xsd.ID
        , year_iso'8601'date :: Maybe Xsd.XsdString
        , year_specific'use  :: Maybe Xsd.XsdString
        , year_base          :: Maybe Xsd.AnyURI
        , year_lang          :: Maybe Xsd.XsdString
          -- ^ Choice between:
        }
        deriving (Eq,Show)
instance SchemaType Year where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "calendar" e pos
        a1 <- optional $ getAttribute "content-type" e pos
        a2 <- optional $ getAttribute "id" e pos
        a3 <- optional $ getAttribute "iso-8601-date" e pos
        a4 <- optional $ getAttribute "specific-use" e pos
        a5 <- optional $ getAttribute "base" e pos
        a6 <- optional $ getAttribute "lang" e pos
        commit $ interior e $ return (Year a0 a1 a2 a3 a4 a5 a6)
            -- `apply` many (oneOf' [])
    schemaTypeToXML s x@Year{} =
        toXMLElement s [ maybe [] (toXMLAttribute "calendar") $ year_calendar x
                       , maybe [] (toXMLAttribute "content-type") $ year_content'type x
                       , maybe [] (toXMLAttribute "id") $ year_id x
                       , maybe [] (toXMLAttribute "iso-8601-date") $ year_iso'8601'date x
                       , maybe [] (toXMLAttribute "specific-use") $ year_specific'use x
                       , maybe [] (toXMLAttribute "base") $ year_base x
                       , maybe [] (toXMLAttribute "lang") $ year_lang x
                       ]
            [
            ]

elementYear :: XMLParser Year
elementYear = parseSchemaType "year"
elementToXMLYear :: Year -> [Content ()]
elementToXMLYear = schemaTypeToXML "year"
