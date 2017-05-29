{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module JATSXML.Types where

import           Data.Maybe
import           JATSXML.Class
import qualified Text.XML.HaXml.XmlContent as HaXml.XmlContent
import           Text.XML.Light            hiding (Parser)

data JatsxmlAbbrev = JatsxmlAbbrev
    { jatsxmlAbbrevElement  :: Element
    , jatsxmlAbbrevChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAbbrev where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "abbrev" = Just $
         JatsxmlAbbrev e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAbbrevJournalTitle = JatsxmlAbbrevJournalTitle
    { jatsxmlAbbrevJournalTitleElement  :: Element
    , jatsxmlAbbrevJournalTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAbbrevJournalTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "abbrev-journal-title" = Just $
         JatsxmlAbbrevJournalTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAbstract = JatsxmlAbstract
    { jatsxmlAbstractElement  :: Element
    , jatsxmlAbstractChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAbstract where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "abstract" = Just $
         JatsxmlAbstract e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAccessDate = JatsxmlAccessDate
    { jatsxmlAccessDateElement  :: Element
    , jatsxmlAccessDateChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAccessDate where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "access-date" = Just $
         JatsxmlAccessDate e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAck = JatsxmlAck
    { jatsxmlAckElement  :: Element
    , jatsxmlAckChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAck where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "ack" = Just $
         JatsxmlAck e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAddrLine = JatsxmlAddrLine
    { jatsxmlAddrLineElement  :: Element
    , jatsxmlAddrLineChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAddrLine where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "addr-line" = Just $
         JatsxmlAddrLine e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAddress = JatsxmlAddress
    { jatsxmlAddressElement  :: Element
    , jatsxmlAddressChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAddress where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "address" = Just $
         JatsxmlAddress e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAffAlternatives = JatsxmlAffAlternatives
    { jatsxmlAffAlternativesElement  :: Element
    , jatsxmlAffAlternativesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAffAlternatives where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "aff-alternatives" = Just $
         JatsxmlAffAlternatives e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAff = JatsxmlAff
    { jatsxmlAffElement  :: Element
    , jatsxmlAffChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAff where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "aff" = Just $
         JatsxmlAff e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAliFree_to_read = JatsxmlAliFree_to_read
    { jatsxmlAliFree_to_readElement  :: Element
    , jatsxmlAliFree_to_readChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAliFree_to_read where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "ali:free_to_read" = Just $
         JatsxmlAliFree_to_read e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAliLicense_ref = JatsxmlAliLicense_ref
    { jatsxmlAliLicense_refElement  :: Element
    , jatsxmlAliLicense_refChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAliLicense_ref where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "ali:license_ref" = Just $
         JatsxmlAliLicense_ref e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAltText = JatsxmlAltText
    { jatsxmlAltTextElement  :: Element
    , jatsxmlAltTextChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAltText where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "alt-text" = Just $
         JatsxmlAltText e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAltTitle = JatsxmlAltTitle
    { jatsxmlAltTitleElement  :: Element
    , jatsxmlAltTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAltTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "alt-title" = Just $
         JatsxmlAltTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAlternatives = JatsxmlAlternatives
    { jatsxmlAlternativesElement  :: Element
    , jatsxmlAlternativesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAlternatives where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "alternatives" = Just $
         JatsxmlAlternatives e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAnnotation = JatsxmlAnnotation
    { jatsxmlAnnotationElement  :: Element
    , jatsxmlAnnotationChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAnnotation where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "annotation" = Just $
         JatsxmlAnnotation e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAnonymous = JatsxmlAnonymous
    { jatsxmlAnonymousElement  :: Element
    , jatsxmlAnonymousChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAnonymous where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "anonymous" = Just $
         JatsxmlAnonymous e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAppGroup = JatsxmlAppGroup
    { jatsxmlAppGroupElement  :: Element
    , jatsxmlAppGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAppGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "app-group" = Just $
         JatsxmlAppGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlApp = JatsxmlApp
    { jatsxmlAppElement  :: Element
    , jatsxmlAppChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlApp where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "app" = Just $
         JatsxmlApp e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlArray = JatsxmlArray
    { jatsxmlArrayElement  :: Element
    , jatsxmlArrayChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlArray where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "array" = Just $
         JatsxmlArray e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlArticle = JatsxmlArticle
    { jatsxmlArticleElement  :: Element
    , jatsxmlArticleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlArticle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "article" = Just $
         JatsxmlArticle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlArticleCategories = JatsxmlArticleCategories
    { jatsxmlArticleCategoriesElement  :: Element
    , jatsxmlArticleCategoriesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlArticleCategories where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "article-categories" = Just $
         JatsxmlArticleCategories e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlArticleId = JatsxmlArticleId
    { jatsxmlArticleIdElement  :: Element
    , jatsxmlArticleIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlArticleId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "article-id" = Just $
         JatsxmlArticleId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlArticleMeta = JatsxmlArticleMeta
    { jatsxmlArticleMetaElement  :: Element
    , jatsxmlArticleMetaChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlArticleMeta where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "article-meta" = Just $
         JatsxmlArticleMeta e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlArticleTitle = JatsxmlArticleTitle
    { jatsxmlArticleTitleElement  :: Element
    , jatsxmlArticleTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlArticleTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "article-title" = Just $
         JatsxmlArticleTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAttrib = JatsxmlAttrib
    { jatsxmlAttribElement  :: Element
    , jatsxmlAttribChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAttrib where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "attrib" = Just $
         JatsxmlAttrib e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAuthorComment = JatsxmlAuthorComment
    { jatsxmlAuthorCommentElement  :: Element
    , jatsxmlAuthorCommentChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAuthorComment where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "author-comment" = Just $
         JatsxmlAuthorComment e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAuthorNotes = JatsxmlAuthorNotes
    { jatsxmlAuthorNotesElement  :: Element
    , jatsxmlAuthorNotesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAuthorNotes where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "author-notes" = Just $
         JatsxmlAuthorNotes e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAwardGroup = JatsxmlAwardGroup
    { jatsxmlAwardGroupElement  :: Element
    , jatsxmlAwardGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAwardGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "award-group" = Just $
         JatsxmlAwardGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlAwardId = JatsxmlAwardId
    { jatsxmlAwardIdElement  :: Element
    , jatsxmlAwardIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlAwardId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "award-id" = Just $
         JatsxmlAwardId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlBack = JatsxmlBack
    { jatsxmlBackElement  :: Element
    , jatsxmlBackChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlBack where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "back" = Just $
         JatsxmlBack e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlBio = JatsxmlBio
    { jatsxmlBioElement  :: Element
    , jatsxmlBioChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlBio where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "bio" = Just $
         JatsxmlBio e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlBody = JatsxmlBody
    { jatsxmlBodyElement  :: Element
    , jatsxmlBodyChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlBody where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "body" = Just $
         JatsxmlBody e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlBold = JatsxmlBold
    { jatsxmlBoldElement  :: Element
    , jatsxmlBoldChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlBold where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "bold" = Just $
         JatsxmlBold e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlBoxedText = JatsxmlBoxedText
    { jatsxmlBoxedTextElement  :: Element
    , jatsxmlBoxedTextChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlBoxedText where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "boxed-text" = Just $
         JatsxmlBoxedText e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlBreak = JatsxmlBreak
    { jatsxmlBreakElement  :: Element
    , jatsxmlBreakChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlBreak where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "break" = Just $
         JatsxmlBreak e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCaption = JatsxmlCaption
    { jatsxmlCaptionElement  :: Element
    , jatsxmlCaptionChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCaption where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "caption" = Just $
         JatsxmlCaption e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlChapterTitle = JatsxmlChapterTitle
    { jatsxmlChapterTitleElement  :: Element
    , jatsxmlChapterTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlChapterTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "chapter-title" = Just $
         JatsxmlChapterTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlChemStructWrap = JatsxmlChemStructWrap
    { jatsxmlChemStructWrapElement  :: Element
    , jatsxmlChemStructWrapChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlChemStructWrap where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "chem-struct-wrap" = Just $
         JatsxmlChemStructWrap e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlChemStruct = JatsxmlChemStruct
    { jatsxmlChemStructElement  :: Element
    , jatsxmlChemStructChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlChemStruct where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "chem-struct" = Just $
         JatsxmlChemStruct e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCitationAlternatives = JatsxmlCitationAlternatives
    { jatsxmlCitationAlternativesElement  :: Element
    , jatsxmlCitationAlternativesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCitationAlternatives where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "citation-alternatives" = Just $
         JatsxmlCitationAlternatives e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCity = JatsxmlCity
    { jatsxmlCityElement  :: Element
    , jatsxmlCityChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCity where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "city" = Just $
         JatsxmlCity e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCode = JatsxmlCode
    { jatsxmlCodeElement  :: Element
    , jatsxmlCodeChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCode where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "code" = Just $
         JatsxmlCode e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCol = JatsxmlCol
    { jatsxmlColElement  :: Element
    , jatsxmlColChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCol where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "col" = Just $
         JatsxmlCol e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlColgroup = JatsxmlColgroup
    { jatsxmlColgroupElement  :: Element
    , jatsxmlColgroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlColgroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "colgroup" = Just $
         JatsxmlColgroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCollabAlternatives = JatsxmlCollabAlternatives
    { jatsxmlCollabAlternativesElement  :: Element
    , jatsxmlCollabAlternativesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCollabAlternatives where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "collab-alternatives" = Just $
         JatsxmlCollabAlternatives e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCollab = JatsxmlCollab
    { jatsxmlCollabElement  :: Element
    , jatsxmlCollabChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCollab where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "collab" = Just $
         JatsxmlCollab e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlComment = JatsxmlComment
    { jatsxmlCommentElement  :: Element
    , jatsxmlCommentChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlComment where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "comment" = Just $
         JatsxmlComment e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCompoundKwdPart = JatsxmlCompoundKwdPart
    { jatsxmlCompoundKwdPartElement  :: Element
    , jatsxmlCompoundKwdPartChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCompoundKwdPart where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "compound-kwd-part" = Just $
         JatsxmlCompoundKwdPart e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCompoundKwd = JatsxmlCompoundKwd
    { jatsxmlCompoundKwdElement  :: Element
    , jatsxmlCompoundKwdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCompoundKwd where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "compound-kwd" = Just $
         JatsxmlCompoundKwd e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCompoundSubjectPart = JatsxmlCompoundSubjectPart
    { jatsxmlCompoundSubjectPartElement  :: Element
    , jatsxmlCompoundSubjectPartChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCompoundSubjectPart where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "compound-subject-part" = Just $
         JatsxmlCompoundSubjectPart e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCompoundSubject = JatsxmlCompoundSubject
    { jatsxmlCompoundSubjectElement  :: Element
    , jatsxmlCompoundSubjectChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCompoundSubject where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "compound-subject" = Just $
         JatsxmlCompoundSubject e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlConfAcronym = JatsxmlConfAcronym
    { jatsxmlConfAcronymElement  :: Element
    , jatsxmlConfAcronymChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlConfAcronym where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "conf-acronym" = Just $
         JatsxmlConfAcronym e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlConfDate = JatsxmlConfDate
    { jatsxmlConfDateElement  :: Element
    , jatsxmlConfDateChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlConfDate where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "conf-date" = Just $
         JatsxmlConfDate e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlConfLoc = JatsxmlConfLoc
    { jatsxmlConfLocElement  :: Element
    , jatsxmlConfLocChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlConfLoc where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "conf-loc" = Just $
         JatsxmlConfLoc e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlConfName = JatsxmlConfName
    { jatsxmlConfNameElement  :: Element
    , jatsxmlConfNameChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlConfName where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "conf-name" = Just $
         JatsxmlConfName e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlConfNum = JatsxmlConfNum
    { jatsxmlConfNumElement  :: Element
    , jatsxmlConfNumChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlConfNum where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "conf-num" = Just $
         JatsxmlConfNum e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlConfSponsor = JatsxmlConfSponsor
    { jatsxmlConfSponsorElement  :: Element
    , jatsxmlConfSponsorChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlConfSponsor where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "conf-sponsor" = Just $
         JatsxmlConfSponsor e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlConfTheme = JatsxmlConfTheme
    { jatsxmlConfThemeElement  :: Element
    , jatsxmlConfThemeChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlConfTheme where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "conf-theme" = Just $
         JatsxmlConfTheme e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlConference = JatsxmlConference
    { jatsxmlConferenceElement  :: Element
    , jatsxmlConferenceChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlConference where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "conference" = Just $
         JatsxmlConference e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlContribGroup = JatsxmlContribGroup
    { jatsxmlContribGroupElement  :: Element
    , jatsxmlContribGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlContribGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "contrib-group" = Just $
         JatsxmlContribGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlContribId = JatsxmlContribId
    { jatsxmlContribIdElement  :: Element
    , jatsxmlContribIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlContribId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "contrib-id" = Just $
         JatsxmlContribId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlContrib = JatsxmlContrib
    { jatsxmlContribElement  :: Element
    , jatsxmlContribChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlContrib where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "contrib" = Just $
         JatsxmlContrib e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCopyrightHolder = JatsxmlCopyrightHolder
    { jatsxmlCopyrightHolderElement  :: Element
    , jatsxmlCopyrightHolderChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCopyrightHolder where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "copyright-holder" = Just $
         JatsxmlCopyrightHolder e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCopyrightStatement = JatsxmlCopyrightStatement
    { jatsxmlCopyrightStatementElement  :: Element
    , jatsxmlCopyrightStatementChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCopyrightStatement where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "copyright-statement" = Just $
         JatsxmlCopyrightStatement e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCopyrightYear = JatsxmlCopyrightYear
    { jatsxmlCopyrightYearElement  :: Element
    , jatsxmlCopyrightYearChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCopyrightYear where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "copyright-year" = Just $
         JatsxmlCopyrightYear e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCorresp = JatsxmlCorresp
    { jatsxmlCorrespElement  :: Element
    , jatsxmlCorrespChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCorresp where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "corresp" = Just $
         JatsxmlCorresp e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCount = JatsxmlCount
    { jatsxmlCountElement  :: Element
    , jatsxmlCountChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCount where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "count" = Just $
         JatsxmlCount e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCountry = JatsxmlCountry
    { jatsxmlCountryElement  :: Element
    , jatsxmlCountryChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCountry where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "country" = Just $
         JatsxmlCountry e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCounts = JatsxmlCounts
    { jatsxmlCountsElement  :: Element
    , jatsxmlCountsChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCounts where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "counts" = Just $
         JatsxmlCounts e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCustomMetaGroup = JatsxmlCustomMetaGroup
    { jatsxmlCustomMetaGroupElement  :: Element
    , jatsxmlCustomMetaGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCustomMetaGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "custom-meta-group" = Just $
         JatsxmlCustomMetaGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlCustomMeta = JatsxmlCustomMeta
    { jatsxmlCustomMetaElement  :: Element
    , jatsxmlCustomMetaChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlCustomMeta where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "custom-meta" = Just $
         JatsxmlCustomMeta e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDateInCitation = JatsxmlDateInCitation
    { jatsxmlDateInCitationElement  :: Element
    , jatsxmlDateInCitationChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDateInCitation where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "date-in-citation" = Just $
         JatsxmlDateInCitation e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDataTitle = JatsxmlDataTitle
    { jatsxmlDataTitleElement  :: Element
    , jatsxmlDataTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDataTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "data-title" = Just $
         JatsxmlDataTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDate = JatsxmlDate
    { jatsxmlDateElement  :: Element
    , jatsxmlDateChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDate where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "date" = Just $
         JatsxmlDate e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDay = JatsxmlDay
    { jatsxmlDayElement  :: Element
    , jatsxmlDayChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDay where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "day" = Just $
         JatsxmlDay e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDefHead = JatsxmlDefHead
    { jatsxmlDefHeadElement  :: Element
    , jatsxmlDefHeadChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDefHead where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "def-head" = Just $
         JatsxmlDefHead e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDefItem = JatsxmlDefItem
    { jatsxmlDefItemElement  :: Element
    , jatsxmlDefItemChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDefItem where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "def-item" = Just $
         JatsxmlDefItem e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDefList = JatsxmlDefList
    { jatsxmlDefListElement  :: Element
    , jatsxmlDefListChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDefList where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "def-list" = Just $
         JatsxmlDefList e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDef = JatsxmlDef
    { jatsxmlDefElement  :: Element
    , jatsxmlDefChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDef where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "def" = Just $
         JatsxmlDef e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDegrees = JatsxmlDegrees
    { jatsxmlDegreesElement  :: Element
    , jatsxmlDegreesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDegrees where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "degrees" = Just $
         JatsxmlDegrees e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDispFormulaGroup = JatsxmlDispFormulaGroup
    { jatsxmlDispFormulaGroupElement  :: Element
    , jatsxmlDispFormulaGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDispFormulaGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "disp-formula-group" = Just $
         JatsxmlDispFormulaGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDispFormula = JatsxmlDispFormula
    { jatsxmlDispFormulaElement  :: Element
    , jatsxmlDispFormulaChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDispFormula where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "disp-formula" = Just $
         JatsxmlDispFormula e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlDispQuote = JatsxmlDispQuote
    { jatsxmlDispQuoteElement  :: Element
    , jatsxmlDispQuoteChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlDispQuote where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "disp-quote" = Just $
         JatsxmlDispQuote e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlEdition = JatsxmlEdition
    { jatsxmlEditionElement  :: Element
    , jatsxmlEditionChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlEdition where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "edition" = Just $
         JatsxmlEdition e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlElementCitation = JatsxmlElementCitation
    { jatsxmlElementCitationElement  :: Element
    , jatsxmlElementCitationChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlElementCitation where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "element-citation" = Just $
         JatsxmlElementCitation e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlElocationId = JatsxmlElocationId
    { jatsxmlElocationIdElement  :: Element
    , jatsxmlElocationIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlElocationId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "elocation-id" = Just $
         JatsxmlElocationId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlEmail = JatsxmlEmail
    { jatsxmlEmailElement  :: Element
    , jatsxmlEmailChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlEmail where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "email" = Just $
         JatsxmlEmail e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlEquationCount = JatsxmlEquationCount
    { jatsxmlEquationCountElement  :: Element
    , jatsxmlEquationCountChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlEquationCount where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "equation-count" = Just $
         JatsxmlEquationCount e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlEra = JatsxmlEra
    { jatsxmlEraElement  :: Element
    , jatsxmlEraChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlEra where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "era" = Just $
         JatsxmlEra e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlEtal = JatsxmlEtal
    { jatsxmlEtalElement  :: Element
    , jatsxmlEtalChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlEtal where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "etal" = Just $
         JatsxmlEtal e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlExtLink = JatsxmlExtLink
    { jatsxmlExtLinkElement  :: Element
    , jatsxmlExtLinkChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlExtLink where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "ext-link" = Just $
         JatsxmlExtLink e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFax = JatsxmlFax
    { jatsxmlFaxElement  :: Element
    , jatsxmlFaxChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFax where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "fax" = Just $
         JatsxmlFax e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFigCount = JatsxmlFigCount
    { jatsxmlFigCountElement  :: Element
    , jatsxmlFigCountChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFigCount where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "fig-count" = Just $
         JatsxmlFigCount e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFigGroup = JatsxmlFigGroup
    { jatsxmlFigGroupElement  :: Element
    , jatsxmlFigGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFigGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "fig-group" = Just $
         JatsxmlFigGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFig = JatsxmlFig
    { jatsxmlFigElement  :: Element
    , jatsxmlFigChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFig where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "fig" = Just $
         JatsxmlFig e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFixedCase = JatsxmlFixedCase
    { jatsxmlFixedCaseElement  :: Element
    , jatsxmlFixedCaseChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFixedCase where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "fixed-case" = Just $
         JatsxmlFixedCase e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFloatsGroup = JatsxmlFloatsGroup
    { jatsxmlFloatsGroupElement  :: Element
    , jatsxmlFloatsGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFloatsGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "floats-group" = Just $
         JatsxmlFloatsGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFnGroup = JatsxmlFnGroup
    { jatsxmlFnGroupElement  :: Element
    , jatsxmlFnGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFnGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "fn-group" = Just $
         JatsxmlFnGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFn = JatsxmlFn
    { jatsxmlFnElement  :: Element
    , jatsxmlFnChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFn where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "fn" = Just $
         JatsxmlFn e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFpage = JatsxmlFpage
    { jatsxmlFpageElement  :: Element
    , jatsxmlFpageChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFpage where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "fpage" = Just $
         JatsxmlFpage e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFront = JatsxmlFront
    { jatsxmlFrontElement  :: Element
    , jatsxmlFrontChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFront where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "front" = Just $
         JatsxmlFront e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFrontStub = JatsxmlFrontStub
    { jatsxmlFrontStubElement  :: Element
    , jatsxmlFrontStubChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFrontStub where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "front-stub" = Just $
         JatsxmlFrontStub e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFundingGroup = JatsxmlFundingGroup
    { jatsxmlFundingGroupElement  :: Element
    , jatsxmlFundingGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFundingGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "funding-group" = Just $
         JatsxmlFundingGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFundingSource = JatsxmlFundingSource
    { jatsxmlFundingSourceElement  :: Element
    , jatsxmlFundingSourceChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFundingSource where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "funding-source" = Just $
         JatsxmlFundingSource e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlFundingStatement = JatsxmlFundingStatement
    { jatsxmlFundingStatementElement  :: Element
    , jatsxmlFundingStatementChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlFundingStatement where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "funding-statement" = Just $
         JatsxmlFundingStatement e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlGivenNames = JatsxmlGivenNames
    { jatsxmlGivenNamesElement  :: Element
    , jatsxmlGivenNamesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlGivenNames where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "given-names" = Just $
         JatsxmlGivenNames e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlGlossary = JatsxmlGlossary
    { jatsxmlGlossaryElement  :: Element
    , jatsxmlGlossaryChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlGlossary where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "glossary" = Just $
         JatsxmlGlossary e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlGlyphData = JatsxmlGlyphData
    { jatsxmlGlyphDataElement  :: Element
    , jatsxmlGlyphDataChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlGlyphData where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "glyph-data" = Just $
         JatsxmlGlyphData e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlGlyphRef = JatsxmlGlyphRef
    { jatsxmlGlyphRefElement  :: Element
    , jatsxmlGlyphRefChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlGlyphRef where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "glyph-ref" = Just $
         JatsxmlGlyphRef e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlGov = JatsxmlGov
    { jatsxmlGovElement  :: Element
    , jatsxmlGovChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlGov where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "gov" = Just $
         JatsxmlGov e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlGraphic = JatsxmlGraphic
    { jatsxmlGraphicElement  :: Element
    , jatsxmlGraphicChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlGraphic where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "graphic" = Just $
         JatsxmlGraphic e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlHistory = JatsxmlHistory
    { jatsxmlHistoryElement  :: Element
    , jatsxmlHistoryChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlHistory where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "history" = Just $
         JatsxmlHistory e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlHr = JatsxmlHr
    { jatsxmlHrElement  :: Element
    , jatsxmlHrChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlHr where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "hr" = Just $
         JatsxmlHr e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlInlineFormula = JatsxmlInlineFormula
    { jatsxmlInlineFormulaElement  :: Element
    , jatsxmlInlineFormulaChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlInlineFormula where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "inline-formula" = Just $
         JatsxmlInlineFormula e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlInlineGraphic = JatsxmlInlineGraphic
    { jatsxmlInlineGraphicElement  :: Element
    , jatsxmlInlineGraphicChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlInlineGraphic where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "inline-graphic" = Just $
         JatsxmlInlineGraphic e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlInlineSupplementaryMaterial = JatsxmlInlineSupplementaryMaterial
    { jatsxmlInlineSupplementaryMaterialElement  :: Element
    , jatsxmlInlineSupplementaryMaterialChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlInlineSupplementaryMaterial where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "inline-supplementary-material" = Just $
         JatsxmlInlineSupplementaryMaterial e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlInstitutionId = JatsxmlInstitutionId
    { jatsxmlInstitutionIdElement  :: Element
    , jatsxmlInstitutionIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlInstitutionId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "institution-id" = Just $
         JatsxmlInstitutionId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlInstitutionWrap = JatsxmlInstitutionWrap
    { jatsxmlInstitutionWrapElement  :: Element
    , jatsxmlInstitutionWrapChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlInstitutionWrap where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "institution-wrap" = Just $
         JatsxmlInstitutionWrap e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlInstitution = JatsxmlInstitution
    { jatsxmlInstitutionElement  :: Element
    , jatsxmlInstitutionChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlInstitution where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "institution" = Just $
         JatsxmlInstitution e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlIsbn = JatsxmlIsbn
    { jatsxmlIsbnElement  :: Element
    , jatsxmlIsbnChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlIsbn where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "isbn" = Just $
         JatsxmlIsbn e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlIssnL = JatsxmlIssnL
    { jatsxmlIssnLElement  :: Element
    , jatsxmlIssnLChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlIssnL where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "issn-l" = Just $
         JatsxmlIssnL e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlIssn = JatsxmlIssn
    { jatsxmlIssnElement  :: Element
    , jatsxmlIssnChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlIssn where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "issn" = Just $
         JatsxmlIssn e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlIssueId = JatsxmlIssueId
    { jatsxmlIssueIdElement  :: Element
    , jatsxmlIssueIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlIssueId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "issue-id" = Just $
         JatsxmlIssueId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlIssuePart = JatsxmlIssuePart
    { jatsxmlIssuePartElement  :: Element
    , jatsxmlIssuePartChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlIssuePart where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "issue-part" = Just $
         JatsxmlIssuePart e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlIssueSponsor = JatsxmlIssueSponsor
    { jatsxmlIssueSponsorElement  :: Element
    , jatsxmlIssueSponsorChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlIssueSponsor where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "issue-sponsor" = Just $
         JatsxmlIssueSponsor e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlIssueTitle = JatsxmlIssueTitle
    { jatsxmlIssueTitleElement  :: Element
    , jatsxmlIssueTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlIssueTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "issue-title" = Just $
         JatsxmlIssueTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlIssue = JatsxmlIssue
    { jatsxmlIssueElement  :: Element
    , jatsxmlIssueChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlIssue where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "issue" = Just $
         JatsxmlIssue e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlItalic = JatsxmlItalic
    { jatsxmlItalicElement  :: Element
    , jatsxmlItalicChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlItalic where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "italic" = Just $
         JatsxmlItalic e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlJournalId = JatsxmlJournalId
    { jatsxmlJournalIdElement  :: Element
    , jatsxmlJournalIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlJournalId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "journal-id" = Just $
         JatsxmlJournalId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlJournalMeta = JatsxmlJournalMeta
    { jatsxmlJournalMetaElement  :: Element
    , jatsxmlJournalMetaChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlJournalMeta where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "journal-meta" = Just $
         JatsxmlJournalMeta e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlJournalSubtitle = JatsxmlJournalSubtitle
    { jatsxmlJournalSubtitleElement  :: Element
    , jatsxmlJournalSubtitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlJournalSubtitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "journal-subtitle" = Just $
         JatsxmlJournalSubtitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlJournalTitle = JatsxmlJournalTitle
    { jatsxmlJournalTitleElement  :: Element
    , jatsxmlJournalTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlJournalTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "journal-title" = Just $
         JatsxmlJournalTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlJournalTitleGroup = JatsxmlJournalTitleGroup
    { jatsxmlJournalTitleGroupElement  :: Element
    , jatsxmlJournalTitleGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlJournalTitleGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "journal-title-group" = Just $
         JatsxmlJournalTitleGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlKwdGroup = JatsxmlKwdGroup
    { jatsxmlKwdGroupElement  :: Element
    , jatsxmlKwdGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlKwdGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "kwd-group" = Just $
         JatsxmlKwdGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlKwd = JatsxmlKwd
    { jatsxmlKwdElement  :: Element
    , jatsxmlKwdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlKwd where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "kwd" = Just $
         JatsxmlKwd e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlLabel = JatsxmlLabel
    { jatsxmlLabelElement  :: Element
    , jatsxmlLabelChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlLabel where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "label" = Just $
         JatsxmlLabel e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlLicenseP = JatsxmlLicenseP
    { jatsxmlLicensePElement  :: Element
    , jatsxmlLicensePChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlLicenseP where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "license-p" = Just $
         JatsxmlLicenseP e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlLicense = JatsxmlLicense
    { jatsxmlLicenseElement  :: Element
    , jatsxmlLicenseChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlLicense where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "license" = Just $
         JatsxmlLicense e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlListItem = JatsxmlListItem
    { jatsxmlListItemElement  :: Element
    , jatsxmlListItemChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlListItem where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "list-item" = Just $
         JatsxmlListItem e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlList = JatsxmlList
    { jatsxmlListElement  :: Element
    , jatsxmlListChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlList where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "list" = Just $
         JatsxmlList e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlLongDesc = JatsxmlLongDesc
    { jatsxmlLongDescElement  :: Element
    , jatsxmlLongDescChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlLongDesc where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "long-desc" = Just $
         JatsxmlLongDesc e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlLpage = JatsxmlLpage
    { jatsxmlLpageElement  :: Element
    , jatsxmlLpageChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlLpage where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "lpage" = Just $
         JatsxmlLpage e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlMedia = JatsxmlMedia
    { jatsxmlMediaElement  :: Element
    , jatsxmlMediaChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlMedia where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "media" = Just $
         JatsxmlMedia e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlMetaName = JatsxmlMetaName
    { jatsxmlMetaNameElement  :: Element
    , jatsxmlMetaNameChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlMetaName where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "meta-name" = Just $
         JatsxmlMetaName e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlMetaValue = JatsxmlMetaValue
    { jatsxmlMetaValueElement  :: Element
    , jatsxmlMetaValueChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlMetaValue where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "meta-value" = Just $
         JatsxmlMetaValue e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlMilestoneEnd = JatsxmlMilestoneEnd
    { jatsxmlMilestoneEndElement  :: Element
    , jatsxmlMilestoneEndChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlMilestoneEnd where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "milestone-end" = Just $
         JatsxmlMilestoneEnd e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlMilestoneStart = JatsxmlMilestoneStart
    { jatsxmlMilestoneStartElement  :: Element
    , jatsxmlMilestoneStartChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlMilestoneStart where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "milestone-start" = Just $
         JatsxmlMilestoneStart e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlMixedCitation = JatsxmlMixedCitation
    { jatsxmlMixedCitationElement  :: Element
    , jatsxmlMixedCitationChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlMixedCitation where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "mixed-citation" = Just $
         JatsxmlMixedCitation e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlMmlMath = JatsxmlMmlMath
    { jatsxmlMmlMathElement  :: Element
    , jatsxmlMmlMathChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlMmlMath where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "mml:math" = Just $
         JatsxmlMmlMath e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlMonospace = JatsxmlMonospace
    { jatsxmlMonospaceElement  :: Element
    , jatsxmlMonospaceChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlMonospace where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "monospace" = Just $
         JatsxmlMonospace e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlMonth = JatsxmlMonth
    { jatsxmlMonthElement  :: Element
    , jatsxmlMonthChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlMonth where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "month" = Just $
         JatsxmlMonth e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlNameAlternatives = JatsxmlNameAlternatives
    { jatsxmlNameAlternativesElement  :: Element
    , jatsxmlNameAlternativesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlNameAlternatives where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "name-alternatives" = Just $
         JatsxmlNameAlternatives e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlName = JatsxmlName
    { jatsxmlNameElement  :: Element
    , jatsxmlNameChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlName where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "name" = Just $
         JatsxmlName e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlNamedContent = JatsxmlNamedContent
    { jatsxmlNamedContentElement  :: Element
    , jatsxmlNamedContentChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlNamedContent where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "named-content" = Just $
         JatsxmlNamedContent e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlNlmCitation = JatsxmlNlmCitation
    { jatsxmlNlmCitationElement  :: Element
    , jatsxmlNlmCitationChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlNlmCitation where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "nlm-citation" = Just $
         JatsxmlNlmCitation e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlNestedKwd = JatsxmlNestedKwd
    { jatsxmlNestedKwdElement  :: Element
    , jatsxmlNestedKwdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlNestedKwd where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "nested-kwd" = Just $
         JatsxmlNestedKwd e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlNote = JatsxmlNote
    { jatsxmlNoteElement  :: Element
    , jatsxmlNoteChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlNote where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "note" = Just $
         JatsxmlNote e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlNotes = JatsxmlNotes
    { jatsxmlNotesElement  :: Element
    , jatsxmlNotesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlNotes where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "notes" = Just $
         JatsxmlNotes e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlObjectId = JatsxmlObjectId
    { jatsxmlObjectIdElement  :: Element
    , jatsxmlObjectIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlObjectId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "object-id" = Just $
         JatsxmlObjectId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlOnBehalfOf = JatsxmlOnBehalfOf
    { jatsxmlOnBehalfOfElement  :: Element
    , jatsxmlOnBehalfOfChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlOnBehalfOf where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "on-behalf-of" = Just $
         JatsxmlOnBehalfOf e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlOpenAccess = JatsxmlOpenAccess
    { jatsxmlOpenAccessElement  :: Element
    , jatsxmlOpenAccessChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlOpenAccess where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "open-access" = Just $
         JatsxmlOpenAccess e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlOverline = JatsxmlOverline
    { jatsxmlOverlineElement  :: Element
    , jatsxmlOverlineChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlOverline where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "overline" = Just $
         JatsxmlOverline e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlP = JatsxmlP
    { jatsxmlPElement  :: Element
    , jatsxmlPChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlP where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "p" = Just $
         JatsxmlP e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPageCount = JatsxmlPageCount
    { jatsxmlPageCountElement  :: Element
    , jatsxmlPageCountChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPageCount where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "page-count" = Just $
         JatsxmlPageCount e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPageRange = JatsxmlPageRange
    { jatsxmlPageRangeElement  :: Element
    , jatsxmlPageRangeChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPageRange where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "page-range" = Just $
         JatsxmlPageRange e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPartTitle = JatsxmlPartTitle
    { jatsxmlPartTitleElement  :: Element
    , jatsxmlPartTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPartTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "part-title" = Just $
         JatsxmlPartTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPatent = JatsxmlPatent
    { jatsxmlPatentElement  :: Element
    , jatsxmlPatentChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPatent where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "patent" = Just $
         JatsxmlPatent e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPermissions = JatsxmlPermissions
    { jatsxmlPermissionsElement  :: Element
    , jatsxmlPermissionsChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPermissions where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "permissions" = Just $
         JatsxmlPermissions e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPersonGroup = JatsxmlPersonGroup
    { jatsxmlPersonGroupElement  :: Element
    , jatsxmlPersonGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPersonGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "person-group" = Just $
         JatsxmlPersonGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPhone = JatsxmlPhone
    { jatsxmlPhoneElement  :: Element
    , jatsxmlPhoneChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPhone where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "phone" = Just $
         JatsxmlPhone e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPostalCode = JatsxmlPostalCode
    { jatsxmlPostalCodeElement  :: Element
    , jatsxmlPostalCodeChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPostalCode where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "postal-code" = Just $
         JatsxmlPostalCode e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPrefix = JatsxmlPrefix
    { jatsxmlPrefixElement  :: Element
    , jatsxmlPrefixChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPrefix where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "prefix" = Just $
         JatsxmlPrefix e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPreformat = JatsxmlPreformat
    { jatsxmlPreformatElement  :: Element
    , jatsxmlPreformatChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPreformat where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "preformat" = Just $
         JatsxmlPreformat e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPrice = JatsxmlPrice
    { jatsxmlPriceElement  :: Element
    , jatsxmlPriceChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPrice where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "price" = Just $
         JatsxmlPrice e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPrincipalAwardRecipient = JatsxmlPrincipalAwardRecipient
    { jatsxmlPrincipalAwardRecipientElement  :: Element
    , jatsxmlPrincipalAwardRecipientChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPrincipalAwardRecipient where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "principal-award-recipient" = Just $
         JatsxmlPrincipalAwardRecipient e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPrincipalInvestigator = JatsxmlPrincipalInvestigator
    { jatsxmlPrincipalInvestigatorElement  :: Element
    , jatsxmlPrincipalInvestigatorChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPrincipalInvestigator where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "principal-investigator" = Just $
         JatsxmlPrincipalInvestigator e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPrivateChar = JatsxmlPrivateChar
    { jatsxmlPrivateCharElement  :: Element
    , jatsxmlPrivateCharChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPrivateChar where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "private-char" = Just $
         JatsxmlPrivateChar e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlProduct = JatsxmlProduct
    { jatsxmlProductElement  :: Element
    , jatsxmlProductChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlProduct where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "product" = Just $
         JatsxmlProduct e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPubDate = JatsxmlPubDate
    { jatsxmlPubDateElement  :: Element
    , jatsxmlPubDateChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPubDate where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "pub-date" = Just $
         JatsxmlPubDate e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPubId = JatsxmlPubId
    { jatsxmlPubIdElement  :: Element
    , jatsxmlPubIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPubId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "pub-id" = Just $
         JatsxmlPubId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPublisherLoc = JatsxmlPublisherLoc
    { jatsxmlPublisherLocElement  :: Element
    , jatsxmlPublisherLocChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPublisherLoc where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "publisher-loc" = Just $
         JatsxmlPublisherLoc e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPublisherName = JatsxmlPublisherName
    { jatsxmlPublisherNameElement  :: Element
    , jatsxmlPublisherNameChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPublisherName where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "publisher-name" = Just $
         JatsxmlPublisherName e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlPublisher = JatsxmlPublisher
    { jatsxmlPublisherElement  :: Element
    , jatsxmlPublisherChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlPublisher where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "publisher" = Just $
         JatsxmlPublisher e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRb = JatsxmlRb
    { jatsxmlRbElement  :: Element
    , jatsxmlRbChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRb where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "rb" = Just $
         JatsxmlRb e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRefCount = JatsxmlRefCount
    { jatsxmlRefCountElement  :: Element
    , jatsxmlRefCountChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRefCount where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "ref-count" = Just $
         JatsxmlRefCount e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRefList = JatsxmlRefList
    { jatsxmlRefListElement  :: Element
    , jatsxmlRefListChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRefList where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "ref-list" = Just $
         JatsxmlRefList e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRef = JatsxmlRef
    { jatsxmlRefElement  :: Element
    , jatsxmlRefChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRef where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "ref" = Just $
         JatsxmlRef e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRelatedArticle = JatsxmlRelatedArticle
    { jatsxmlRelatedArticleElement  :: Element
    , jatsxmlRelatedArticleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRelatedArticle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "related-article" = Just $
         JatsxmlRelatedArticle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRelatedObject = JatsxmlRelatedObject
    { jatsxmlRelatedObjectElement  :: Element
    , jatsxmlRelatedObjectChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRelatedObject where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "related-object" = Just $
         JatsxmlRelatedObject e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlResponse = JatsxmlResponse
    { jatsxmlResponseElement  :: Element
    , jatsxmlResponseChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlResponse where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "response" = Just $
         JatsxmlResponse e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRole = JatsxmlRole
    { jatsxmlRoleElement  :: Element
    , jatsxmlRoleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRole where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "role" = Just $
         JatsxmlRole e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRoman = JatsxmlRoman
    { jatsxmlRomanElement  :: Element
    , jatsxmlRomanChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRoman where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "roman" = Just $
         JatsxmlRoman e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRt = JatsxmlRt
    { jatsxmlRtElement  :: Element
    , jatsxmlRtChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRt where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "rt" = Just $
         JatsxmlRt e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlRuby = JatsxmlRuby
    { jatsxmlRubyElement  :: Element
    , jatsxmlRubyChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlRuby where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "ruby" = Just $
         JatsxmlRuby e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSansSerif = JatsxmlSansSerif
    { jatsxmlSansSerifElement  :: Element
    , jatsxmlSansSerifChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSansSerif where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "sans-serif" = Just $
         JatsxmlSansSerif e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSc = JatsxmlSc
    { jatsxmlScElement  :: Element
    , jatsxmlScChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSc where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "sc" = Just $
         JatsxmlSc e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSeason = JatsxmlSeason
    { jatsxmlSeasonElement  :: Element
    , jatsxmlSeasonChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSeason where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "season" = Just $
         JatsxmlSeason e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSecMeta = JatsxmlSecMeta
    { jatsxmlSecMetaElement  :: Element
    , jatsxmlSecMetaChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSecMeta where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "sec-meta" = Just $
         JatsxmlSecMeta e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSec = JatsxmlSec
    { jatsxmlSecElement  :: Element
    , jatsxmlSecChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSec where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "sec" = Just $
         JatsxmlSec e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSelfUri = JatsxmlSelfUri
    { jatsxmlSelfUriElement  :: Element
    , jatsxmlSelfUriChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSelfUri where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "self-uri" = Just $
         JatsxmlSelfUri e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSeries = JatsxmlSeries
    { jatsxmlSeriesElement  :: Element
    , jatsxmlSeriesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSeries where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "series" = Just $
         JatsxmlSeries e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSeriesText = JatsxmlSeriesText
    { jatsxmlSeriesTextElement  :: Element
    , jatsxmlSeriesTextChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSeriesText where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "series-text" = Just $
         JatsxmlSeriesText e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSeriesTitle = JatsxmlSeriesTitle
    { jatsxmlSeriesTitleElement  :: Element
    , jatsxmlSeriesTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSeriesTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "series-title" = Just $
         JatsxmlSeriesTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSigBlock = JatsxmlSigBlock
    { jatsxmlSigBlockElement  :: Element
    , jatsxmlSigBlockChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSigBlock where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "sig-block" = Just $
         JatsxmlSigBlock e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSig = JatsxmlSig
    { jatsxmlSigElement  :: Element
    , jatsxmlSigChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSig where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "sig" = Just $
         JatsxmlSig e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSize = JatsxmlSize
    { jatsxmlSizeElement  :: Element
    , jatsxmlSizeChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSize where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "size" = Just $
         JatsxmlSize e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSource = JatsxmlSource
    { jatsxmlSourceElement  :: Element
    , jatsxmlSourceChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSource where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "source" = Just $
         JatsxmlSource e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSpeaker = JatsxmlSpeaker
    { jatsxmlSpeakerElement  :: Element
    , jatsxmlSpeakerChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSpeaker where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "speaker" = Just $
         JatsxmlSpeaker e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSpeech = JatsxmlSpeech
    { jatsxmlSpeechElement  :: Element
    , jatsxmlSpeechChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSpeech where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "speech" = Just $
         JatsxmlSpeech e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlState = JatsxmlState
    { jatsxmlStateElement  :: Element
    , jatsxmlStateChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlState where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "state" = Just $
         JatsxmlState e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlStatement = JatsxmlStatement
    { jatsxmlStatementElement  :: Element
    , jatsxmlStatementChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlStatement where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "statement" = Just $
         JatsxmlStatement e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlStdOrganization = JatsxmlStdOrganization
    { jatsxmlStdOrganizationElement  :: Element
    , jatsxmlStdOrganizationChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlStdOrganization where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "std-organization" = Just $
         JatsxmlStdOrganization e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlStd = JatsxmlStd
    { jatsxmlStdElement  :: Element
    , jatsxmlStdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlStd where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "std" = Just $
         JatsxmlStd e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlStrike = JatsxmlStrike
    { jatsxmlStrikeElement  :: Element
    , jatsxmlStrikeChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlStrike where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "strike" = Just $
         JatsxmlStrike e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlStringDate = JatsxmlStringDate
    { jatsxmlStringDateElement  :: Element
    , jatsxmlStringDateChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlStringDate where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "string-date" = Just $
         JatsxmlStringDate e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlStringName = JatsxmlStringName
    { jatsxmlStringNameElement  :: Element
    , jatsxmlStringNameChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlStringName where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "string-name" = Just $
         JatsxmlStringName e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlStyledContent = JatsxmlStyledContent
    { jatsxmlStyledContentElement  :: Element
    , jatsxmlStyledContentChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlStyledContent where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "styled-content" = Just $
         JatsxmlStyledContent e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSub = JatsxmlSub
    { jatsxmlSubElement  :: Element
    , jatsxmlSubChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSub where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "sub" = Just $
         JatsxmlSub e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSubArticle = JatsxmlSubArticle
    { jatsxmlSubArticleElement  :: Element
    , jatsxmlSubArticleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSubArticle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "sub-article" = Just $
         JatsxmlSubArticle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSubjGroup = JatsxmlSubjGroup
    { jatsxmlSubjGroupElement  :: Element
    , jatsxmlSubjGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSubjGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "subj-group" = Just $
         JatsxmlSubjGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSubject = JatsxmlSubject
    { jatsxmlSubjectElement  :: Element
    , jatsxmlSubjectChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSubject where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "subject" = Just $
         JatsxmlSubject e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSubtitle = JatsxmlSubtitle
    { jatsxmlSubtitleElement  :: Element
    , jatsxmlSubtitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSubtitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "subtitle" = Just $
         JatsxmlSubtitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSuffix = JatsxmlSuffix
    { jatsxmlSuffixElement  :: Element
    , jatsxmlSuffixChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSuffix where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "suffix" = Just $
         JatsxmlSuffix e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSup = JatsxmlSup
    { jatsxmlSupElement  :: Element
    , jatsxmlSupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "sup" = Just $
         JatsxmlSup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSupplement = JatsxmlSupplement
    { jatsxmlSupplementElement  :: Element
    , jatsxmlSupplementChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSupplement where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "supplement" = Just $
         JatsxmlSupplement e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSupplementaryMaterial = JatsxmlSupplementaryMaterial
    { jatsxmlSupplementaryMaterialElement  :: Element
    , jatsxmlSupplementaryMaterialChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSupplementaryMaterial where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "supplementary-material" = Just $
         JatsxmlSupplementaryMaterial e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlSurname = JatsxmlSurname
    { jatsxmlSurnameElement  :: Element
    , jatsxmlSurnameChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlSurname where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "surname" = Just $
         JatsxmlSurname e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTableCount = JatsxmlTableCount
    { jatsxmlTableCountElement  :: Element
    , jatsxmlTableCountChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTableCount where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "table-count" = Just $
         JatsxmlTableCount e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTableWrapFoot = JatsxmlTableWrapFoot
    { jatsxmlTableWrapFootElement  :: Element
    , jatsxmlTableWrapFootChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTableWrapFoot where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "table-wrap-foot" = Just $
         JatsxmlTableWrapFoot e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTableWrapGroup = JatsxmlTableWrapGroup
    { jatsxmlTableWrapGroupElement  :: Element
    , jatsxmlTableWrapGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTableWrapGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "table-wrap-group" = Just $
         JatsxmlTableWrapGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTableWrap = JatsxmlTableWrap
    { jatsxmlTableWrapElement  :: Element
    , jatsxmlTableWrapChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTableWrap where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "table-wrap" = Just $
         JatsxmlTableWrap e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTable = JatsxmlTable
    { jatsxmlTableElement  :: Element
    , jatsxmlTableChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTable where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "table" = Just $
         JatsxmlTable e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTarget = JatsxmlTarget
    { jatsxmlTargetElement  :: Element
    , jatsxmlTargetChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTarget where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "target" = Just $
         JatsxmlTarget e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTbody = JatsxmlTbody
    { jatsxmlTbodyElement  :: Element
    , jatsxmlTbodyChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTbody where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "tbody" = Just $
         JatsxmlTbody e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTd = JatsxmlTd
    { jatsxmlTdElement  :: Element
    , jatsxmlTdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTd where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "td" = Just $
         JatsxmlTd e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTermHead = JatsxmlTermHead
    { jatsxmlTermHeadElement  :: Element
    , jatsxmlTermHeadChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTermHead where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "term-head" = Just $
         JatsxmlTermHead e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTerm = JatsxmlTerm
    { jatsxmlTermElement  :: Element
    , jatsxmlTermChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTerm where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "term" = Just $
         JatsxmlTerm e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTexMath = JatsxmlTexMath
    { jatsxmlTexMathElement  :: Element
    , jatsxmlTexMathChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTexMath where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "tex-math" = Just $
         JatsxmlTexMath e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTextualForm = JatsxmlTextualForm
    { jatsxmlTextualFormElement  :: Element
    , jatsxmlTextualFormChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTextualForm where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "textual-form" = Just $
         JatsxmlTextualForm e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTfoot = JatsxmlTfoot
    { jatsxmlTfootElement  :: Element
    , jatsxmlTfootChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTfoot where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "tfoot" = Just $
         JatsxmlTfoot e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTh = JatsxmlTh
    { jatsxmlThElement  :: Element
    , jatsxmlThChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTh where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "th" = Just $
         JatsxmlTh e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlThead = JatsxmlThead
    { jatsxmlTheadElement  :: Element
    , jatsxmlTheadChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlThead where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "thead" = Just $
         JatsxmlThead e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTimeStamp = JatsxmlTimeStamp
    { jatsxmlTimeStampElement  :: Element
    , jatsxmlTimeStampChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTimeStamp where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "time-stamp" = Just $
         JatsxmlTimeStamp e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTitleGroup = JatsxmlTitleGroup
    { jatsxmlTitleGroupElement  :: Element
    , jatsxmlTitleGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTitleGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "title-group" = Just $
         JatsxmlTitleGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTitle = JatsxmlTitle
    { jatsxmlTitleElement  :: Element
    , jatsxmlTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "title" = Just $
         JatsxmlTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTr = JatsxmlTr
    { jatsxmlTrElement  :: Element
    , jatsxmlTrChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTr where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "tr" = Just $
         JatsxmlTr e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTransAbstract = JatsxmlTransAbstract
    { jatsxmlTransAbstractElement  :: Element
    , jatsxmlTransAbstractChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTransAbstract where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "trans-abstract" = Just $
         JatsxmlTransAbstract e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTransSource = JatsxmlTransSource
    { jatsxmlTransSourceElement  :: Element
    , jatsxmlTransSourceChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTransSource where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "trans-source" = Just $
         JatsxmlTransSource e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTransSubtitle = JatsxmlTransSubtitle
    { jatsxmlTransSubtitleElement  :: Element
    , jatsxmlTransSubtitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTransSubtitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "trans-subtitle" = Just $
         JatsxmlTransSubtitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTransTitleGroup = JatsxmlTransTitleGroup
    { jatsxmlTransTitleGroupElement  :: Element
    , jatsxmlTransTitleGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTransTitleGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "trans-title-group" = Just $
         JatsxmlTransTitleGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlTransTitle = JatsxmlTransTitle
    { jatsxmlTransTitleElement  :: Element
    , jatsxmlTransTitleChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlTransTitle where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "trans-title" = Just $
         JatsxmlTransTitle e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlUnderline = JatsxmlUnderline
    { jatsxmlUnderlineElement  :: Element
    , jatsxmlUnderlineChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlUnderline where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "underline" = Just $
         JatsxmlUnderline e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlUri = JatsxmlUri
    { jatsxmlUriElement  :: Element
    , jatsxmlUriChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlUri where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "uri" = Just $
         JatsxmlUri e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlVersion = JatsxmlVersion
    { jatsxmlVersionElement  :: Element
    , jatsxmlVersionChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlVersion where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "version" = Just $
         JatsxmlVersion e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlVerseGroup = JatsxmlVerseGroup
    { jatsxmlVerseGroupElement  :: Element
    , jatsxmlVerseGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlVerseGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "verse-group" = Just $
         JatsxmlVerseGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlVerseLine = JatsxmlVerseLine
    { jatsxmlVerseLineElement  :: Element
    , jatsxmlVerseLineChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlVerseLine where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "verse-line" = Just $
         JatsxmlVerseLine e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlVolumeId = JatsxmlVolumeId
    { jatsxmlVolumeIdElement  :: Element
    , jatsxmlVolumeIdChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlVolumeId where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "volume-id" = Just $
         JatsxmlVolumeId e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlVolumeIssueGroup = JatsxmlVolumeIssueGroup
    { jatsxmlVolumeIssueGroupElement  :: Element
    , jatsxmlVolumeIssueGroupChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlVolumeIssueGroup where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "volume-issue-group" = Just $
         JatsxmlVolumeIssueGroup e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlVolumeSeries = JatsxmlVolumeSeries
    { jatsxmlVolumeSeriesElement  :: Element
    , jatsxmlVolumeSeriesChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlVolumeSeries where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "volume-series" = Just $
         JatsxmlVolumeSeries e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlVolume = JatsxmlVolume
    { jatsxmlVolumeElement  :: Element
    , jatsxmlVolumeChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlVolume where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "volume" = Just $
         JatsxmlVolume e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlWordCount = JatsxmlWordCount
    { jatsxmlWordCountElement  :: Element
    , jatsxmlWordCountChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlWordCount where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "word-count" = Just $
         JatsxmlWordCount e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlXref = JatsxmlXref
    { jatsxmlXrefElement  :: Element
    , jatsxmlXrefChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlXref where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "xref" = Just $
         JatsxmlXref e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JatsxmlYear = JatsxmlYear
    { jatsxmlYearElement  :: Element
    , jatsxmlYearChildren :: [JATSElement]
    }
  deriving(Show)

instance FromXMLNode JatsxmlYear where
    fromXMLNode (Elem e@(Element{..})) | qName elName == "year" = Just $
         JatsxmlYear e (catMaybes $ map fromXMLNode elContent)
    fromXMLNode _ = Nothing

data JATSElement =
    NodeJatsxmlAbbrev JatsxmlAbbrev
  | NodeJatsxmlAbbrevJournalTitle JatsxmlAbbrevJournalTitle
  | NodeJatsxmlAbstract JatsxmlAbstract
  | NodeJatsxmlAccessDate JatsxmlAccessDate
  | NodeJatsxmlAck JatsxmlAck
  | NodeJatsxmlAddrLine JatsxmlAddrLine
  | NodeJatsxmlAddress JatsxmlAddress
  | NodeJatsxmlAffAlternatives JatsxmlAffAlternatives
  | NodeJatsxmlAff JatsxmlAff
  | NodeJatsxmlAliFree_to_read JatsxmlAliFree_to_read
  | NodeJatsxmlAliLicense_ref JatsxmlAliLicense_ref
  | NodeJatsxmlAltText JatsxmlAltText
  | NodeJatsxmlAltTitle JatsxmlAltTitle
  | NodeJatsxmlAlternatives JatsxmlAlternatives
  | NodeJatsxmlAnnotation JatsxmlAnnotation
  | NodeJatsxmlAnonymous JatsxmlAnonymous
  | NodeJatsxmlAppGroup JatsxmlAppGroup
  | NodeJatsxmlApp JatsxmlApp
  | NodeJatsxmlArray JatsxmlArray
  | NodeJatsxmlArticle JatsxmlArticle
  | NodeJatsxmlArticleCategories JatsxmlArticleCategories
  | NodeJatsxmlArticleId JatsxmlArticleId
  | NodeJatsxmlArticleMeta JatsxmlArticleMeta
  | NodeJatsxmlArticleTitle JatsxmlArticleTitle
  | NodeJatsxmlAttrib JatsxmlAttrib
  | NodeJatsxmlAuthorComment JatsxmlAuthorComment
  | NodeJatsxmlAuthorNotes JatsxmlAuthorNotes
  | NodeJatsxmlAwardGroup JatsxmlAwardGroup
  | NodeJatsxmlAwardId JatsxmlAwardId
  | NodeJatsxmlBack JatsxmlBack
  | NodeJatsxmlBio JatsxmlBio
  | NodeJatsxmlBody JatsxmlBody
  | NodeJatsxmlBold JatsxmlBold
  | NodeJatsxmlBoxedText JatsxmlBoxedText
  | NodeJatsxmlBreak JatsxmlBreak
  | NodeJatsxmlCaption JatsxmlCaption
  | NodeJatsxmlChapterTitle JatsxmlChapterTitle
  | NodeJatsxmlChemStructWrap JatsxmlChemStructWrap
  | NodeJatsxmlChemStruct JatsxmlChemStruct
  | NodeJatsxmlCitationAlternatives JatsxmlCitationAlternatives
  | NodeJatsxmlCity JatsxmlCity
  | NodeJatsxmlCode JatsxmlCode
  | NodeJatsxmlCol JatsxmlCol
  | NodeJatsxmlColgroup JatsxmlColgroup
  | NodeJatsxmlCollabAlternatives JatsxmlCollabAlternatives
  | NodeJatsxmlCollab JatsxmlCollab
  | NodeJatsxmlComment JatsxmlComment
  | NodeJatsxmlCompoundKwdPart JatsxmlCompoundKwdPart
  | NodeJatsxmlCompoundKwd JatsxmlCompoundKwd
  | NodeJatsxmlCompoundSubjectPart JatsxmlCompoundSubjectPart
  | NodeJatsxmlCompoundSubject JatsxmlCompoundSubject
  | NodeJatsxmlConfAcronym JatsxmlConfAcronym
  | NodeJatsxmlConfDate JatsxmlConfDate
  | NodeJatsxmlConfLoc JatsxmlConfLoc
  | NodeJatsxmlConfName JatsxmlConfName
  | NodeJatsxmlConfNum JatsxmlConfNum
  | NodeJatsxmlConfSponsor JatsxmlConfSponsor
  | NodeJatsxmlConfTheme JatsxmlConfTheme
  | NodeJatsxmlConference JatsxmlConference
  | NodeJatsxmlContribGroup JatsxmlContribGroup
  | NodeJatsxmlContribId JatsxmlContribId
  | NodeJatsxmlContrib JatsxmlContrib
  | NodeJatsxmlCopyrightHolder JatsxmlCopyrightHolder
  | NodeJatsxmlCopyrightStatement JatsxmlCopyrightStatement
  | NodeJatsxmlCopyrightYear JatsxmlCopyrightYear
  | NodeJatsxmlCorresp JatsxmlCorresp
  | NodeJatsxmlCount JatsxmlCount
  | NodeJatsxmlCountry JatsxmlCountry
  | NodeJatsxmlCounts JatsxmlCounts
  | NodeJatsxmlCustomMetaGroup JatsxmlCustomMetaGroup
  | NodeJatsxmlCustomMeta JatsxmlCustomMeta
  | NodeJatsxmlDateInCitation JatsxmlDateInCitation
  | NodeJatsxmlDataTitle JatsxmlDataTitle
  | NodeJatsxmlDate JatsxmlDate
  | NodeJatsxmlDay JatsxmlDay
  | NodeJatsxmlDefHead JatsxmlDefHead
  | NodeJatsxmlDefItem JatsxmlDefItem
  | NodeJatsxmlDefList JatsxmlDefList
  | NodeJatsxmlDef JatsxmlDef
  | NodeJatsxmlDegrees JatsxmlDegrees
  | NodeJatsxmlDispFormulaGroup JatsxmlDispFormulaGroup
  | NodeJatsxmlDispFormula JatsxmlDispFormula
  | NodeJatsxmlDispQuote JatsxmlDispQuote
  | NodeJatsxmlEdition JatsxmlEdition
  | NodeJatsxmlElementCitation JatsxmlElementCitation
  | NodeJatsxmlElocationId JatsxmlElocationId
  | NodeJatsxmlEmail JatsxmlEmail
  | NodeJatsxmlEquationCount JatsxmlEquationCount
  | NodeJatsxmlEra JatsxmlEra
  | NodeJatsxmlEtal JatsxmlEtal
  | NodeJatsxmlExtLink JatsxmlExtLink
  | NodeJatsxmlFax JatsxmlFax
  | NodeJatsxmlFigCount JatsxmlFigCount
  | NodeJatsxmlFigGroup JatsxmlFigGroup
  | NodeJatsxmlFig JatsxmlFig
  | NodeJatsxmlFixedCase JatsxmlFixedCase
  | NodeJatsxmlFloatsGroup JatsxmlFloatsGroup
  | NodeJatsxmlFnGroup JatsxmlFnGroup
  | NodeJatsxmlFn JatsxmlFn
  | NodeJatsxmlFpage JatsxmlFpage
  | NodeJatsxmlFront JatsxmlFront
  | NodeJatsxmlFrontStub JatsxmlFrontStub
  | NodeJatsxmlFundingGroup JatsxmlFundingGroup
  | NodeJatsxmlFundingSource JatsxmlFundingSource
  | NodeJatsxmlFundingStatement JatsxmlFundingStatement
  | NodeJatsxmlGivenNames JatsxmlGivenNames
  | NodeJatsxmlGlossary JatsxmlGlossary
  | NodeJatsxmlGlyphData JatsxmlGlyphData
  | NodeJatsxmlGlyphRef JatsxmlGlyphRef
  | NodeJatsxmlGov JatsxmlGov
  | NodeJatsxmlGraphic JatsxmlGraphic
  | NodeJatsxmlHistory JatsxmlHistory
  | NodeJatsxmlHr JatsxmlHr
  | NodeJatsxmlInlineFormula JatsxmlInlineFormula
  | NodeJatsxmlInlineGraphic JatsxmlInlineGraphic
  | NodeJatsxmlInlineSupplementaryMaterial JatsxmlInlineSupplementaryMaterial
  | NodeJatsxmlInstitutionId JatsxmlInstitutionId
  | NodeJatsxmlInstitutionWrap JatsxmlInstitutionWrap
  | NodeJatsxmlInstitution JatsxmlInstitution
  | NodeJatsxmlIsbn JatsxmlIsbn
  | NodeJatsxmlIssnL JatsxmlIssnL
  | NodeJatsxmlIssn JatsxmlIssn
  | NodeJatsxmlIssueId JatsxmlIssueId
  | NodeJatsxmlIssuePart JatsxmlIssuePart
  | NodeJatsxmlIssueSponsor JatsxmlIssueSponsor
  | NodeJatsxmlIssueTitle JatsxmlIssueTitle
  | NodeJatsxmlIssue JatsxmlIssue
  | NodeJatsxmlItalic JatsxmlItalic
  | NodeJatsxmlJournalId JatsxmlJournalId
  | NodeJatsxmlJournalMeta JatsxmlJournalMeta
  | NodeJatsxmlJournalSubtitle JatsxmlJournalSubtitle
  | NodeJatsxmlJournalTitle JatsxmlJournalTitle
  | NodeJatsxmlJournalTitleGroup JatsxmlJournalTitleGroup
  | NodeJatsxmlKwdGroup JatsxmlKwdGroup
  | NodeJatsxmlKwd JatsxmlKwd
  | NodeJatsxmlLabel JatsxmlLabel
  | NodeJatsxmlLicenseP JatsxmlLicenseP
  | NodeJatsxmlLicense JatsxmlLicense
  | NodeJatsxmlListItem JatsxmlListItem
  | NodeJatsxmlList JatsxmlList
  | NodeJatsxmlLongDesc JatsxmlLongDesc
  | NodeJatsxmlLpage JatsxmlLpage
  | NodeJatsxmlMedia JatsxmlMedia
  | NodeJatsxmlMetaName JatsxmlMetaName
  | NodeJatsxmlMetaValue JatsxmlMetaValue
  | NodeJatsxmlMilestoneEnd JatsxmlMilestoneEnd
  | NodeJatsxmlMilestoneStart JatsxmlMilestoneStart
  | NodeJatsxmlMixedCitation JatsxmlMixedCitation
  | NodeJatsxmlMmlMath JatsxmlMmlMath
  | NodeJatsxmlMonospace JatsxmlMonospace
  | NodeJatsxmlMonth JatsxmlMonth
  | NodeJatsxmlNameAlternatives JatsxmlNameAlternatives
  | NodeJatsxmlName JatsxmlName
  | NodeJatsxmlNamedContent JatsxmlNamedContent
  | NodeJatsxmlNlmCitation JatsxmlNlmCitation
  | NodeJatsxmlNestedKwd JatsxmlNestedKwd
  | NodeJatsxmlNote JatsxmlNote
  | NodeJatsxmlNotes JatsxmlNotes
  | NodeJatsxmlObjectId JatsxmlObjectId
  | NodeJatsxmlOnBehalfOf JatsxmlOnBehalfOf
  | NodeJatsxmlOpenAccess JatsxmlOpenAccess
  | NodeJatsxmlOverline JatsxmlOverline
  | NodeJatsxmlP JatsxmlP
  | NodeJatsxmlPageCount JatsxmlPageCount
  | NodeJatsxmlPageRange JatsxmlPageRange
  | NodeJatsxmlPartTitle JatsxmlPartTitle
  | NodeJatsxmlPatent JatsxmlPatent
  | NodeJatsxmlPermissions JatsxmlPermissions
  | NodeJatsxmlPersonGroup JatsxmlPersonGroup
  | NodeJatsxmlPhone JatsxmlPhone
  | NodeJatsxmlPostalCode JatsxmlPostalCode
  | NodeJatsxmlPrefix JatsxmlPrefix
  | NodeJatsxmlPreformat JatsxmlPreformat
  | NodeJatsxmlPrice JatsxmlPrice
  | NodeJatsxmlPrincipalAwardRecipient JatsxmlPrincipalAwardRecipient
  | NodeJatsxmlPrincipalInvestigator JatsxmlPrincipalInvestigator
  | NodeJatsxmlPrivateChar JatsxmlPrivateChar
  | NodeJatsxmlProduct JatsxmlProduct
  | NodeJatsxmlPubDate JatsxmlPubDate
  | NodeJatsxmlPubId JatsxmlPubId
  | NodeJatsxmlPublisherLoc JatsxmlPublisherLoc
  | NodeJatsxmlPublisherName JatsxmlPublisherName
  | NodeJatsxmlPublisher JatsxmlPublisher
  | NodeJatsxmlRb JatsxmlRb
  | NodeJatsxmlRefCount JatsxmlRefCount
  | NodeJatsxmlRefList JatsxmlRefList
  | NodeJatsxmlRef JatsxmlRef
  | NodeJatsxmlRelatedArticle JatsxmlRelatedArticle
  | NodeJatsxmlRelatedObject JatsxmlRelatedObject
  | NodeJatsxmlResponse JatsxmlResponse
  | NodeJatsxmlRole JatsxmlRole
  | NodeJatsxmlRoman JatsxmlRoman
  | NodeJatsxmlRt JatsxmlRt
  | NodeJatsxmlRuby JatsxmlRuby
  | NodeJatsxmlSansSerif JatsxmlSansSerif
  | NodeJatsxmlSc JatsxmlSc
  | NodeJatsxmlSeason JatsxmlSeason
  | NodeJatsxmlSecMeta JatsxmlSecMeta
  | NodeJatsxmlSec JatsxmlSec
  | NodeJatsxmlSelfUri JatsxmlSelfUri
  | NodeJatsxmlSeries JatsxmlSeries
  | NodeJatsxmlSeriesText JatsxmlSeriesText
  | NodeJatsxmlSeriesTitle JatsxmlSeriesTitle
  | NodeJatsxmlSigBlock JatsxmlSigBlock
  | NodeJatsxmlSig JatsxmlSig
  | NodeJatsxmlSize JatsxmlSize
  | NodeJatsxmlSource JatsxmlSource
  | NodeJatsxmlSpeaker JatsxmlSpeaker
  | NodeJatsxmlSpeech JatsxmlSpeech
  | NodeJatsxmlState JatsxmlState
  | NodeJatsxmlStatement JatsxmlStatement
  | NodeJatsxmlStdOrganization JatsxmlStdOrganization
  | NodeJatsxmlStd JatsxmlStd
  | NodeJatsxmlStrike JatsxmlStrike
  | NodeJatsxmlStringDate JatsxmlStringDate
  | NodeJatsxmlStringName JatsxmlStringName
  | NodeJatsxmlStyledContent JatsxmlStyledContent
  | NodeJatsxmlSub JatsxmlSub
  | NodeJatsxmlSubArticle JatsxmlSubArticle
  | NodeJatsxmlSubjGroup JatsxmlSubjGroup
  | NodeJatsxmlSubject JatsxmlSubject
  | NodeJatsxmlSubtitle JatsxmlSubtitle
  | NodeJatsxmlSuffix JatsxmlSuffix
  | NodeJatsxmlSup JatsxmlSup
  | NodeJatsxmlSupplement JatsxmlSupplement
  | NodeJatsxmlSupplementaryMaterial JatsxmlSupplementaryMaterial
  | NodeJatsxmlSurname JatsxmlSurname
  | NodeJatsxmlTableCount JatsxmlTableCount
  | NodeJatsxmlTableWrapFoot JatsxmlTableWrapFoot
  | NodeJatsxmlTableWrapGroup JatsxmlTableWrapGroup
  | NodeJatsxmlTableWrap JatsxmlTableWrap
  | NodeJatsxmlTable JatsxmlTable
  | NodeJatsxmlTarget JatsxmlTarget
  | NodeJatsxmlTbody JatsxmlTbody
  | NodeJatsxmlTd JatsxmlTd
  | NodeJatsxmlTermHead JatsxmlTermHead
  | NodeJatsxmlTerm JatsxmlTerm
  | NodeJatsxmlTexMath JatsxmlTexMath
  | NodeJatsxmlTextualForm JatsxmlTextualForm
  | NodeJatsxmlTfoot JatsxmlTfoot
  | NodeJatsxmlTh JatsxmlTh
  | NodeJatsxmlThead JatsxmlThead
  | NodeJatsxmlTimeStamp JatsxmlTimeStamp
  | NodeJatsxmlTitleGroup JatsxmlTitleGroup
  | NodeJatsxmlTitle JatsxmlTitle
  | NodeJatsxmlTr JatsxmlTr
  | NodeJatsxmlTransAbstract JatsxmlTransAbstract
  | NodeJatsxmlTransSource JatsxmlTransSource
  | NodeJatsxmlTransSubtitle JatsxmlTransSubtitle
  | NodeJatsxmlTransTitleGroup JatsxmlTransTitleGroup
  | NodeJatsxmlTransTitle JatsxmlTransTitle
  | NodeJatsxmlUnderline JatsxmlUnderline
  | NodeJatsxmlUri JatsxmlUri
  | NodeJatsxmlVersion JatsxmlVersion
  | NodeJatsxmlVerseGroup JatsxmlVerseGroup
  | NodeJatsxmlVerseLine JatsxmlVerseLine
  | NodeJatsxmlVolumeId JatsxmlVolumeId
  | NodeJatsxmlVolumeIssueGroup JatsxmlVolumeIssueGroup
  | NodeJatsxmlVolumeSeries JatsxmlVolumeSeries
  | NodeJatsxmlVolume JatsxmlVolume
  | NodeJatsxmlWordCount JatsxmlWordCount
  | NodeJatsxmlXref JatsxmlXref
  | NodeJatsxmlYear JatsxmlYear
  deriving(Show)

instance FromXMLNode JATSElement where
  fromXMLNode xmlNode =
      headMaybe (catMaybes (map ($ xmlNode) parsers))

headMaybe []    = Nothing
headMaybe (x:_) = Just x

type Parser = Content -> Maybe JATSElement

parsers :: [Parser]
parsers =
  [ (NodeJatsxmlAbbrev <$>) . (\x -> fromXMLNode x :: Maybe JatsxmlAbbrev)
  , (NodeJatsxmlAbbrevJournalTitle <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlAbbrevJournalTitle
  , (NodeJatsxmlAbstract <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAbstract
  , (NodeJatsxmlAccessDate <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAccessDate
  , (NodeJatsxmlAck <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAck
  , (NodeJatsxmlAddrLine <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAddrLine
  , (NodeJatsxmlAddress <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAddress
  , (NodeJatsxmlAffAlternatives <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlAffAlternatives
  , (NodeJatsxmlAff <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAff
  , (NodeJatsxmlAliFree_to_read <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlAliFree_to_read
  , (NodeJatsxmlAliLicense_ref <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlAliLicense_ref
  , (NodeJatsxmlAltText <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAltText
  , (NodeJatsxmlAltTitle <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAltTitle
  , (NodeJatsxmlAlternatives <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlAlternatives
  , (NodeJatsxmlAnnotation <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAnnotation
  , (NodeJatsxmlAnonymous <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAnonymous
  , (NodeJatsxmlAppGroup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAppGroup
  , (NodeJatsxmlApp <$>) . \x -> fromXMLNode x :: Maybe JatsxmlApp
  , (NodeJatsxmlArray <$>) . \x -> fromXMLNode x :: Maybe JatsxmlArray
  , (NodeJatsxmlArticle <$>) . \x -> fromXMLNode x :: Maybe JatsxmlArticle
  , (NodeJatsxmlArticleCategories <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlArticleCategories
  , (NodeJatsxmlArticleId <$>) . \x -> fromXMLNode x :: Maybe JatsxmlArticleId
  , (NodeJatsxmlArticleMeta <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlArticleMeta
  , (NodeJatsxmlArticleTitle <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlArticleTitle
  , (NodeJatsxmlAttrib <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAttrib
  , (NodeJatsxmlAuthorComment <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlAuthorComment
  , (NodeJatsxmlAuthorNotes <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlAuthorNotes
  , (NodeJatsxmlAwardGroup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAwardGroup
  , (NodeJatsxmlAwardId <$>) . \x -> fromXMLNode x :: Maybe JatsxmlAwardId
  , (NodeJatsxmlBack <$>) . \x -> fromXMLNode x :: Maybe JatsxmlBack
  , (NodeJatsxmlBio <$>) . \x -> fromXMLNode x :: Maybe JatsxmlBio
  , (NodeJatsxmlBody <$>) . \x -> fromXMLNode x :: Maybe JatsxmlBody
  , (NodeJatsxmlBold <$>) . \x -> fromXMLNode x :: Maybe JatsxmlBold
  , (NodeJatsxmlBoxedText <$>) . \x -> fromXMLNode x :: Maybe JatsxmlBoxedText
  , (NodeJatsxmlBreak <$>) . \x -> fromXMLNode x :: Maybe JatsxmlBreak
  , (NodeJatsxmlCaption <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCaption
  , (NodeJatsxmlChapterTitle <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlChapterTitle
  , (NodeJatsxmlChemStructWrap <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlChemStructWrap
  , (NodeJatsxmlChemStruct <$>) . \x -> fromXMLNode x :: Maybe JatsxmlChemStruct
  , (NodeJatsxmlCitationAlternatives <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCitationAlternatives
  , (NodeJatsxmlCity <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCity
  , (NodeJatsxmlCode <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCode
  , (NodeJatsxmlCol <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCol
  , (NodeJatsxmlColgroup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlColgroup
  , (NodeJatsxmlCollabAlternatives <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCollabAlternatives
  , (NodeJatsxmlCollab <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCollab
  , (NodeJatsxmlComment <$>) . \x -> fromXMLNode x :: Maybe JatsxmlComment
  , (NodeJatsxmlCompoundKwdPart <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCompoundKwdPart
  , (NodeJatsxmlCompoundKwd <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCompoundKwd
  , (NodeJatsxmlCompoundSubjectPart <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCompoundSubjectPart
  , (NodeJatsxmlCompoundSubject <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCompoundSubject
  , (NodeJatsxmlConfAcronym <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlConfAcronym
  , (NodeJatsxmlConfDate <$>) . \x -> fromXMLNode x :: Maybe JatsxmlConfDate
  , (NodeJatsxmlConfLoc <$>) . \x -> fromXMLNode x :: Maybe JatsxmlConfLoc
  , (NodeJatsxmlConfName <$>) . \x -> fromXMLNode x :: Maybe JatsxmlConfName
  , (NodeJatsxmlConfNum <$>) . \x -> fromXMLNode x :: Maybe JatsxmlConfNum
  , (NodeJatsxmlConfSponsor <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlConfSponsor
  , (NodeJatsxmlConfTheme <$>) . \x -> fromXMLNode x :: Maybe JatsxmlConfTheme
  , (NodeJatsxmlConference <$>) . \x -> fromXMLNode x :: Maybe JatsxmlConference
  , (NodeJatsxmlContribGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlContribGroup
  , (NodeJatsxmlContribId <$>) . \x -> fromXMLNode x :: Maybe JatsxmlContribId
  , (NodeJatsxmlContrib <$>) . \x -> fromXMLNode x :: Maybe JatsxmlContrib
  , (NodeJatsxmlCopyrightHolder <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCopyrightHolder
  , (NodeJatsxmlCopyrightStatement <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCopyrightStatement
  , (NodeJatsxmlCopyrightYear <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCopyrightYear
  , (NodeJatsxmlCorresp <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCorresp
  , (NodeJatsxmlCount <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCount
  , (NodeJatsxmlCountry <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCountry
  , (NodeJatsxmlCounts <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCounts
  , (NodeJatsxmlCustomMetaGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlCustomMetaGroup
  , (NodeJatsxmlCustomMeta <$>) . \x -> fromXMLNode x :: Maybe JatsxmlCustomMeta
  , (NodeJatsxmlDateInCitation <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlDateInCitation
  , (NodeJatsxmlDataTitle <$>) . \x -> fromXMLNode x :: Maybe JatsxmlDataTitle
  , (NodeJatsxmlDate <$>) . \x -> fromXMLNode x :: Maybe JatsxmlDate
  , (NodeJatsxmlDay <$>) . \x -> fromXMLNode x :: Maybe JatsxmlDay
  , (NodeJatsxmlDefHead <$>) . \x -> fromXMLNode x :: Maybe JatsxmlDefHead
  , (NodeJatsxmlDefItem <$>) . \x -> fromXMLNode x :: Maybe JatsxmlDefItem
  , (NodeJatsxmlDefList <$>) . \x -> fromXMLNode x :: Maybe JatsxmlDefList
  , (NodeJatsxmlDef <$>) . \x -> fromXMLNode x :: Maybe JatsxmlDef
  , (NodeJatsxmlDegrees <$>) . \x -> fromXMLNode x :: Maybe JatsxmlDegrees
  , (NodeJatsxmlDispFormulaGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlDispFormulaGroup
  , (NodeJatsxmlDispFormula <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlDispFormula
  , (NodeJatsxmlDispQuote <$>) . \x -> fromXMLNode x :: Maybe JatsxmlDispQuote
  , (NodeJatsxmlEdition <$>) . \x -> fromXMLNode x :: Maybe JatsxmlEdition
  , (NodeJatsxmlElementCitation <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlElementCitation
  , (NodeJatsxmlElocationId <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlElocationId
  , (NodeJatsxmlEmail <$>) . \x -> fromXMLNode x :: Maybe JatsxmlEmail
  , (NodeJatsxmlEquationCount <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlEquationCount
  , (NodeJatsxmlEra <$>) . \x -> fromXMLNode x :: Maybe JatsxmlEra
  , (NodeJatsxmlEtal <$>) . \x -> fromXMLNode x :: Maybe JatsxmlEtal
  , (NodeJatsxmlExtLink <$>) . \x -> fromXMLNode x :: Maybe JatsxmlExtLink
  , (NodeJatsxmlFax <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFax
  , (NodeJatsxmlFigCount <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFigCount
  , (NodeJatsxmlFigGroup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFigGroup
  , (NodeJatsxmlFig <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFig
  , (NodeJatsxmlFixedCase <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFixedCase
  , (NodeJatsxmlFloatsGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlFloatsGroup
  , (NodeJatsxmlFnGroup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFnGroup
  , (NodeJatsxmlFn <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFn
  , (NodeJatsxmlFpage <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFpage
  , (NodeJatsxmlFront <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFront
  , (NodeJatsxmlFrontStub <$>) . \x -> fromXMLNode x :: Maybe JatsxmlFrontStub
  , (NodeJatsxmlFundingGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlFundingGroup
  , (NodeJatsxmlFundingSource <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlFundingSource
  , (NodeJatsxmlFundingStatement <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlFundingStatement
  , (NodeJatsxmlGivenNames <$>) . \x -> fromXMLNode x :: Maybe JatsxmlGivenNames
  , (NodeJatsxmlGlossary <$>) . \x -> fromXMLNode x :: Maybe JatsxmlGlossary
  , (NodeJatsxmlGlyphData <$>) . \x -> fromXMLNode x :: Maybe JatsxmlGlyphData
  , (NodeJatsxmlGlyphRef <$>) . \x -> fromXMLNode x :: Maybe JatsxmlGlyphRef
  , (NodeJatsxmlGov <$>) . \x -> fromXMLNode x :: Maybe JatsxmlGov
  , (NodeJatsxmlGraphic <$>) . \x -> fromXMLNode x :: Maybe JatsxmlGraphic
  , (NodeJatsxmlHistory <$>) . \x -> fromXMLNode x :: Maybe JatsxmlHistory
  , (NodeJatsxmlHr <$>) . \x -> fromXMLNode x :: Maybe JatsxmlHr
  , (NodeJatsxmlInlineFormula <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlInlineFormula
  , (NodeJatsxmlInlineGraphic <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlInlineGraphic
  , (NodeJatsxmlInlineSupplementaryMaterial <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlInlineSupplementaryMaterial
  , (NodeJatsxmlInstitutionId <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlInstitutionId
  , (NodeJatsxmlInstitutionWrap <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlInstitutionWrap
  , (NodeJatsxmlInstitution <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlInstitution
  , (NodeJatsxmlIsbn <$>) . \x -> fromXMLNode x :: Maybe JatsxmlIsbn
  , (NodeJatsxmlIssnL <$>) . \x -> fromXMLNode x :: Maybe JatsxmlIssnL
  , (NodeJatsxmlIssn <$>) . \x -> fromXMLNode x :: Maybe JatsxmlIssn
  , (NodeJatsxmlIssueId <$>) . \x -> fromXMLNode x :: Maybe JatsxmlIssueId
  , (NodeJatsxmlIssuePart <$>) . \x -> fromXMLNode x :: Maybe JatsxmlIssuePart
  , (NodeJatsxmlIssueSponsor <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlIssueSponsor
  , (NodeJatsxmlIssueTitle <$>) . \x -> fromXMLNode x :: Maybe JatsxmlIssueTitle
  , (NodeJatsxmlIssue <$>) . \x -> fromXMLNode x :: Maybe JatsxmlIssue
  , (NodeJatsxmlItalic <$>) . \x -> fromXMLNode x :: Maybe JatsxmlItalic
  , (NodeJatsxmlJournalId <$>) . \x -> fromXMLNode x :: Maybe JatsxmlJournalId
  , (NodeJatsxmlJournalMeta <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlJournalMeta
  , (NodeJatsxmlJournalSubtitle <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlJournalSubtitle
  , (NodeJatsxmlJournalTitle <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlJournalTitle
  , (NodeJatsxmlJournalTitleGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlJournalTitleGroup
  , (NodeJatsxmlKwdGroup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlKwdGroup
  , (NodeJatsxmlKwd <$>) . \x -> fromXMLNode x :: Maybe JatsxmlKwd
  , (NodeJatsxmlLabel <$>) . \x -> fromXMLNode x :: Maybe JatsxmlLabel
  , (NodeJatsxmlLicenseP <$>) . \x -> fromXMLNode x :: Maybe JatsxmlLicenseP
  , (NodeJatsxmlLicense <$>) . \x -> fromXMLNode x :: Maybe JatsxmlLicense
  , (NodeJatsxmlListItem <$>) . \x -> fromXMLNode x :: Maybe JatsxmlListItem
  , (NodeJatsxmlList <$>) . \x -> fromXMLNode x :: Maybe JatsxmlList
  , (NodeJatsxmlLongDesc <$>) . \x -> fromXMLNode x :: Maybe JatsxmlLongDesc
  , (NodeJatsxmlLpage <$>) . \x -> fromXMLNode x :: Maybe JatsxmlLpage
  , (NodeJatsxmlMedia <$>) . \x -> fromXMLNode x :: Maybe JatsxmlMedia
  , (NodeJatsxmlMetaName <$>) . \x -> fromXMLNode x :: Maybe JatsxmlMetaName
  , (NodeJatsxmlMetaValue <$>) . \x -> fromXMLNode x :: Maybe JatsxmlMetaValue
  , (NodeJatsxmlMilestoneEnd <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlMilestoneEnd
  , (NodeJatsxmlMilestoneStart <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlMilestoneStart
  , (NodeJatsxmlMixedCitation <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlMixedCitation
  , (NodeJatsxmlMmlMath <$>) . \x -> fromXMLNode x :: Maybe JatsxmlMmlMath
  , (NodeJatsxmlMonospace <$>) . \x -> fromXMLNode x :: Maybe JatsxmlMonospace
  , (NodeJatsxmlMonth <$>) . \x -> fromXMLNode x :: Maybe JatsxmlMonth
  , (NodeJatsxmlNameAlternatives <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlNameAlternatives
  , (NodeJatsxmlName <$>) . \x -> fromXMLNode x :: Maybe JatsxmlName
  , (NodeJatsxmlNamedContent <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlNamedContent
  , (NodeJatsxmlNlmCitation <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlNlmCitation
  , (NodeJatsxmlNestedKwd <$>) . \x -> fromXMLNode x :: Maybe JatsxmlNestedKwd
  , (NodeJatsxmlNote <$>) . \x -> fromXMLNode x :: Maybe JatsxmlNote
  , (NodeJatsxmlNotes <$>) . \x -> fromXMLNode x :: Maybe JatsxmlNotes
  , (NodeJatsxmlObjectId <$>) . \x -> fromXMLNode x :: Maybe JatsxmlObjectId
  , (NodeJatsxmlOnBehalfOf <$>) . \x -> fromXMLNode x :: Maybe JatsxmlOnBehalfOf
  , (NodeJatsxmlOpenAccess <$>) . \x -> fromXMLNode x :: Maybe JatsxmlOpenAccess
  , (NodeJatsxmlOverline <$>) . \x -> fromXMLNode x :: Maybe JatsxmlOverline
  , (NodeJatsxmlP <$>) . \x -> fromXMLNode x :: Maybe JatsxmlP
  , (NodeJatsxmlPageCount <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPageCount
  , (NodeJatsxmlPageRange <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPageRange
  , (NodeJatsxmlPartTitle <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPartTitle
  , (NodeJatsxmlPatent <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPatent
  , (NodeJatsxmlPermissions <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlPermissions
  , (NodeJatsxmlPersonGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlPersonGroup
  , (NodeJatsxmlPhone <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPhone
  , (NodeJatsxmlPostalCode <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPostalCode
  , (NodeJatsxmlPrefix <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPrefix
  , (NodeJatsxmlPreformat <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPreformat
  , (NodeJatsxmlPrice <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPrice
  , (NodeJatsxmlPrincipalAwardRecipient <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlPrincipalAwardRecipient
  , (NodeJatsxmlPrincipalInvestigator <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlPrincipalInvestigator
  , (NodeJatsxmlPrivateChar <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlPrivateChar
  , (NodeJatsxmlProduct <$>) . \x -> fromXMLNode x :: Maybe JatsxmlProduct
  , (NodeJatsxmlPubDate <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPubDate
  , (NodeJatsxmlPubId <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPubId
  , (NodeJatsxmlPublisherLoc <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlPublisherLoc
  , (NodeJatsxmlPublisherName <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlPublisherName
  , (NodeJatsxmlPublisher <$>) . \x -> fromXMLNode x :: Maybe JatsxmlPublisher
  , (NodeJatsxmlRb <$>) . \x -> fromXMLNode x :: Maybe JatsxmlRb
  , (NodeJatsxmlRefCount <$>) . \x -> fromXMLNode x :: Maybe JatsxmlRefCount
  , (NodeJatsxmlRefList <$>) . \x -> fromXMLNode x :: Maybe JatsxmlRefList
  , (NodeJatsxmlRef <$>) . \x -> fromXMLNode x :: Maybe JatsxmlRef
  , (NodeJatsxmlRelatedArticle <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlRelatedArticle
  , (NodeJatsxmlRelatedObject <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlRelatedObject
  , (NodeJatsxmlResponse <$>) . \x -> fromXMLNode x :: Maybe JatsxmlResponse
  , (NodeJatsxmlRole <$>) . \x -> fromXMLNode x :: Maybe JatsxmlRole
  , (NodeJatsxmlRoman <$>) . \x -> fromXMLNode x :: Maybe JatsxmlRoman
  , (NodeJatsxmlRt <$>) . \x -> fromXMLNode x :: Maybe JatsxmlRt
  , (NodeJatsxmlRuby <$>) . \x -> fromXMLNode x :: Maybe JatsxmlRuby
  , (NodeJatsxmlSansSerif <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSansSerif
  , (NodeJatsxmlSc <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSc
  , (NodeJatsxmlSeason <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSeason
  , (NodeJatsxmlSecMeta <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSecMeta
  , (NodeJatsxmlSec <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSec
  , (NodeJatsxmlSelfUri <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSelfUri
  , (NodeJatsxmlSeries <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSeries
  , (NodeJatsxmlSeriesText <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSeriesText
  , (NodeJatsxmlSeriesTitle <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlSeriesTitle
  , (NodeJatsxmlSigBlock <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSigBlock
  , (NodeJatsxmlSig <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSig
  , (NodeJatsxmlSize <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSize
  , (NodeJatsxmlSource <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSource
  , (NodeJatsxmlSpeaker <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSpeaker
  , (NodeJatsxmlSpeech <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSpeech
  , (NodeJatsxmlState <$>) . \x -> fromXMLNode x :: Maybe JatsxmlState
  , (NodeJatsxmlStatement <$>) . \x -> fromXMLNode x :: Maybe JatsxmlStatement
  , (NodeJatsxmlStdOrganization <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlStdOrganization
  , (NodeJatsxmlStd <$>) . \x -> fromXMLNode x :: Maybe JatsxmlStd
  , (NodeJatsxmlStrike <$>) . \x -> fromXMLNode x :: Maybe JatsxmlStrike
  , (NodeJatsxmlStringDate <$>) . \x -> fromXMLNode x :: Maybe JatsxmlStringDate
  , (NodeJatsxmlStringName <$>) . \x -> fromXMLNode x :: Maybe JatsxmlStringName
  , (NodeJatsxmlStyledContent <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlStyledContent
  , (NodeJatsxmlSub <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSub
  , (NodeJatsxmlSubArticle <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSubArticle
  , (NodeJatsxmlSubjGroup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSubjGroup
  , (NodeJatsxmlSubject <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSubject
  , (NodeJatsxmlSubtitle <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSubtitle
  , (NodeJatsxmlSuffix <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSuffix
  , (NodeJatsxmlSup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSup
  , (NodeJatsxmlSupplement <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSupplement
  , (NodeJatsxmlSupplementaryMaterial <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlSupplementaryMaterial
  , (NodeJatsxmlSurname <$>) . \x -> fromXMLNode x :: Maybe JatsxmlSurname
  , (NodeJatsxmlTableCount <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTableCount
  , (NodeJatsxmlTableWrapFoot <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlTableWrapFoot
  , (NodeJatsxmlTableWrapGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlTableWrapGroup
  , (NodeJatsxmlTableWrap <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTableWrap
  , (NodeJatsxmlTable <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTable
  , (NodeJatsxmlTarget <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTarget
  , (NodeJatsxmlTbody <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTbody
  , (NodeJatsxmlTd <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTd
  , (NodeJatsxmlTermHead <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTermHead
  , (NodeJatsxmlTerm <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTerm
  , (NodeJatsxmlTexMath <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTexMath
  , (NodeJatsxmlTextualForm <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlTextualForm
  , (NodeJatsxmlTfoot <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTfoot
  , (NodeJatsxmlTh <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTh
  , (NodeJatsxmlThead <$>) . \x -> fromXMLNode x :: Maybe JatsxmlThead
  , (NodeJatsxmlTimeStamp <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTimeStamp
  , (NodeJatsxmlTitleGroup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTitleGroup
  , (NodeJatsxmlTitle <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTitle
  , (NodeJatsxmlTr <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTr
  , (NodeJatsxmlTransAbstract <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlTransAbstract
  , (NodeJatsxmlTransSource <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlTransSource
  , (NodeJatsxmlTransSubtitle <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlTransSubtitle
  , (NodeJatsxmlTransTitleGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlTransTitleGroup
  , (NodeJatsxmlTransTitle <$>) . \x -> fromXMLNode x :: Maybe JatsxmlTransTitle
  , (NodeJatsxmlUnderline <$>) . \x -> fromXMLNode x :: Maybe JatsxmlUnderline
  , (NodeJatsxmlUri <$>) . \x -> fromXMLNode x :: Maybe JatsxmlUri
  , (NodeJatsxmlVersion <$>) . \x -> fromXMLNode x :: Maybe JatsxmlVersion
  , (NodeJatsxmlVerseGroup <$>) . \x -> fromXMLNode x :: Maybe JatsxmlVerseGroup
  , (NodeJatsxmlVerseLine <$>) . \x -> fromXMLNode x :: Maybe JatsxmlVerseLine
  , (NodeJatsxmlVolumeId <$>) . \x -> fromXMLNode x :: Maybe JatsxmlVolumeId
  , (NodeJatsxmlVolumeIssueGroup <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlVolumeIssueGroup
  , (NodeJatsxmlVolumeSeries <$>) . \x ->
      fromXMLNode x :: Maybe JatsxmlVolumeSeries
  , (NodeJatsxmlVolume <$>) . \x -> fromXMLNode x :: Maybe JatsxmlVolume
  , (NodeJatsxmlWordCount <$>) . \x -> fromXMLNode x :: Maybe JatsxmlWordCount
  , (NodeJatsxmlXref <$>) . \x -> fromXMLNode x :: Maybe JatsxmlXref
  , (NodeJatsxmlYear <$>) . (\x -> fromXMLNode x :: Maybe JatsxmlYear)
  ]
