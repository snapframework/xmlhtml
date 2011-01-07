{-# LANGUAGE OverloadedStrings #-}

module Text.XmlHtml.Common where

import Data.ByteString (ByteString)
import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


------------------------------------------------------------------------------
-- | Represents a document fragment, including the format, encoding, and
--   document type declaration as well as its content.
data Document = XmlDocument  {
                    docEncoding :: !Encoding,
                    docType     :: !(Maybe DocType),
                    docContent  :: ![Node]
                }
              | HtmlDocument {
                    docEncoding :: !Encoding,
                    docType     :: !(Maybe DocType),
                    docContent  :: ![Node]
                }
    deriving (Eq, Show)


------------------------------------------------------------------------------
data Node = TextNode !Text
          | Comment  !Text
          | Element {
                elementTag      :: !Text,
                elementAttrs    :: ![(Text, Text)],
                elementChildren :: ![Node]
            }
    deriving (Eq, Show)


------------------------------------------------------------------------------
isTextNode :: Node -> Bool
isTextNode (TextNode _) = True
isTextNode _            = False


------------------------------------------------------------------------------
isComment :: Node -> Bool
isComment (Comment _) = True
isComment _           = False


------------------------------------------------------------------------------
isElement :: Node -> Bool
isElement (Element _ _ _) = True
isElement _               = False


------------------------------------------------------------------------------
tagName :: Node -> Maybe Text
tagName (Element t _ _) = Just t
tagName _               = Nothing


------------------------------------------------------------------------------
nodeText :: Node -> Text
nodeText (TextNode t)    = t
nodeText (Comment _)     = ""
nodeText (Element _ _ c) = T.concat (map nodeText c)


------------------------------------------------------------------------------
childNodes :: Node -> [Node]
childNodes (Element _ _ c) = c
childNodes _               = []


------------------------------------------------------------------------------
childElements :: Node -> [Node]
childElements (Element _ _ c) = filter isElement c
childElements _               = []


------------------------------------------------------------------------------
childElementsTag :: Text -> Node -> [Node]
childElementsTag tag (Element _ _ c) = filter ((== Just tag) . tagName) c
childElementsTag _   _               = []


------------------------------------------------------------------------------
childElementTag :: Text -> Node -> Maybe Node
childElementTag tag n = listToMaybe (childElementsTag tag n)


------------------------------------------------------------------------------
data DocType = DocType !Text !(Maybe ExternalID)
    deriving (Eq, Show)


------------------------------------------------------------------------------
data ExternalID = System !Text
                | Public !Text !Text
    deriving (Eq, Show)


------------------------------------------------------------------------------
data Encoding = UTF8 | UTF16BE | UTF16LE deriving (Eq, Show)


------------------------------------------------------------------------------
encodingName :: Encoding -> Text
encodingName UTF8    = "UTF-8"
encodingName UTF16BE = "UTF-16"
encodingName UTF16LE = "UTF-16"


------------------------------------------------------------------------------
encoder :: Encoding -> Text -> ByteString
encoder UTF8    = T.encodeUtf8
encoder UTF16BE = T.encodeUtf16BE
encoder UTF16LE = T.encodeUtf16LE


------------------------------------------------------------------------------
decoder :: Encoding -> ByteString -> Text
decoder UTF8    = T.decodeUtf8
decoder UTF16BE = T.decodeUtf16BE
decoder UTF16LE = T.decodeUtf16LE


------------------------------------------------------------------------------
isUTF16 :: Encoding -> Bool
isUTF16 e = e == UTF16BE || e == UTF16LE

