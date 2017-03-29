{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.XmlHtml.Common where

import           Data.ByteString (ByteString)
import           Blaze.ByteString.Builder
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as B
import           Data.Char (chr, isAscii, isLatin1)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Map as Map
import           Data.Maybe

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import           Text.Parsec.Text (Parser)

import           Text.XmlHtml.HTML.Meta (reversePredefinedRefs,
                                         explicitAttributes)


------------------------------------------------------------------------------
-- | Represents a document fragment, including the format, encoding, and
-- document type declaration as well as its content.
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
-- | A node of a document structure.  A node can be text, a comment, or an
-- element.  XML processing instructions are intentionally omitted as a
-- simplification, and CDATA and plain text are both text nodes, since they
-- ought to be semantically interchangeable.
data Node = TextNode !Text
          | Comment  !Text
          | Element {
                elementTag      :: !Text,
                elementAttrs    :: ![(Text, Text)],
                elementChildren :: ![Node]
            }
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Rendering options
data RenderOptions = RenderOptions {
      roAttributeSurround :: AttrSurround
      -- ^ Single or double-quotes used around attribute values

    , roAttributeResolveInternal :: AttrResolveInternalQuotes
      -- ^ Quotes inside attribute values that conflict with the surround
      -- are escaped, or the outer quotes are changed to avoid conflicting
      -- with the internal ones

    , roExplicitEmptyAttrs :: Maybe (M.HashMap Text (S.HashSet Text))
      -- ^ Attributes in the whitelist with empty values are
      -- rendered as <div example="">
      -- 'Nothing' applies this rule to all attributes with empty values

    } deriving (Eq, Show)

data AttrSurround = SurroundDoubleQuote | SurroundSingleQuote
    deriving (Eq, Ord, Show)

data AttrResolveInternalQuotes = AttrResolveByEscape | AttrResolveAvoidEscape
    deriving (Eq, Ord, Show)

defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions
    { roAttributeSurround        = SurroundSingleQuote
    , roAttributeResolveInternal = AttrResolveAvoidEscape
    , roExplicitEmptyAttrs       = Just explicitAttributes
    }

------------------------------------------------------------------------------
-- | Determines whether the node is text or not.
isTextNode :: Node -> Bool
isTextNode (TextNode _) = True
isTextNode _            = False


------------------------------------------------------------------------------
-- | Determines whether the node is a comment or not.
isComment :: Node -> Bool
isComment (Comment _) = True
isComment _           = False


------------------------------------------------------------------------------
-- | Determines whether the node is an element or not.
isElement :: Node -> Bool
isElement (Element _ _ _) = True
isElement _               = False


------------------------------------------------------------------------------
-- | Gives the tag name of an element, or 'Nothing' if the node isn't an
-- element.
tagName :: Node -> Maybe Text
tagName (Element t _ _) = Just t
tagName _               = Nothing


------------------------------------------------------------------------------
-- | Retrieves the attribute with the given name.  If the 'Node' is not an
-- element, the result is always 'Nothing'
getAttribute :: Text -> Node -> Maybe Text
getAttribute name (Element _ attrs _) = lookup name attrs
getAttribute _    _                   = Nothing


------------------------------------------------------------------------------
-- | Checks if a given attribute exists in a 'Node'.
hasAttribute :: Text -> Node -> Bool
hasAttribute name = isJust . getAttribute name


------------------------------------------------------------------------------
-- | Sets the attribute name to the given value.  If the 'Node' is not an
-- element, this is the identity.
setAttribute :: Text -> Text -> Node -> Node
setAttribute name val (Element t a c) = Element t newAttrs c
  where newAttrs = (name, val) : filter ((/= name) . fst) a
setAttribute _    _   n                   = n


------------------------------------------------------------------------------
-- | Gives the entire text content of a node, ignoring markup.
nodeText :: Node -> Text
nodeText (TextNode t)    = t
nodeText (Comment _)     = ""
nodeText (Element _ _ c) = T.concat (map nodeText c)


------------------------------------------------------------------------------
-- | Gives the child nodes of the given node.  Only elements have child nodes.
childNodes :: Node -> [Node]
childNodes (Element _ _ c) = c
childNodes _               = []


------------------------------------------------------------------------------
-- | Gives the child elements of the given node.
childElements :: Node -> [Node]
childElements = filter isElement . childNodes


------------------------------------------------------------------------------
-- | Gives all of the child elements of the node with the given tag
-- name.
childElementsTag :: Text -> Node -> [Node]
childElementsTag tag = filter ((== Just tag) . tagName) . childNodes


------------------------------------------------------------------------------
-- | Gives the first child element of the node with the given tag name,
-- or 'Nothing' if there is no such child element.
childElementTag :: Text -> Node -> Maybe Node
childElementTag tag = listToMaybe . childElementsTag tag


------------------------------------------------------------------------------
-- | Gives the descendants of the given node in the order that they begin in
-- the document.
descendantNodes :: Node -> [Node]
descendantNodes = concatMap (\n -> n : descendantNodes n) . childNodes

------------------------------------------------------------------------------
-- | Gives the descendant elements of the given node, in the order that their
-- start tags appear in the document.
descendantElements :: Node -> [Node]
descendantElements = filter isElement . descendantNodes


------------------------------------------------------------------------------
-- | Gives the descendant elements with a given tag name.
descendantElementsTag :: Text -> Node -> [Node]
descendantElementsTag tag = filter ((== Just tag) . tagName) . descendantNodes


------------------------------------------------------------------------------
-- | Gives the first descendant element of the node with the given tag name,
-- or 'Nothing' if there is no such element.
descendantElementTag :: Text -> Node -> Maybe Node
descendantElementTag tag = listToMaybe . descendantElementsTag tag


------------------------------------------------------------------------------
-- | A document type declaration.  Note that DTD internal subsets are
-- currently unimplemented.
data DocType = DocType !Text !ExternalID !InternalSubset
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- | An external ID, as in a document type declaration.  This can be a
-- SYSTEM identifier, or a PUBLIC identifier, or can be omitted.
data ExternalID = Public !Text !Text
                | System !Text
                | NoExternalID
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- | The internal subset is unparsed, but preserved in case it's actually
-- wanted.
data InternalSubset = InternalText !Text
                    | NoInternalSubset
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- | The character encoding of a document.  Currently only the required
-- character encodings are implemented.
data Encoding = UTF8 | UTF16BE | UTF16LE | ISO_8859_1 deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Retrieves the preferred name of a character encoding for embedding in
-- a document.
encodingName :: Encoding -> Text
encodingName UTF8       = "UTF-8"
encodingName UTF16BE    = "UTF-16"
encodingName UTF16LE    = "UTF-16"
encodingName ISO_8859_1 = "ISO-8859-1"


------------------------------------------------------------------------------
-- | Gets the encoding function from 'Text' to 'ByteString' for an encoding.
encoder :: Encoding -> Text -> ByteString
encoder UTF8       = T.encodeUtf8
encoder UTF16BE    = T.encodeUtf16BE
encoder UTF16LE    = T.encodeUtf16LE
encoder ISO_8859_1 = encodeAscii


------------------------------------------------------------------------------
-- | Encodes UTF-8 Text into bytestring with only latin1 characters
-- UTF-8 characters found in the input and present in the
-- 'Text.XmlHtml.Meta.references' map are mapped to their escape sequences,
-- and any other UTF-8 characters are replaced with ascii "?"
encodeAscii :: Text -> ByteString
encodeAscii t = T.encodeUtf8 . T.concat . map toAsciiChunk $
                T.groupBy asciiSplits t
  where

    -- Identify long strings of all-acceptable or all-unacceptable characters
    -- Acceptable strings are passed through
    -- Unacceptable strings are mapped to ASCII character by character
    toAsciiChunk sub =
      if T.any isAscii sub
      then sub
      else T.concat . map toAsciiChar $ T.unpack sub
    asciiSplits x y = isAscii x == isAscii y

    -- A character's mapping to ascii goes through html entity escaping
    -- if that character is in the references table
    -- Otherwise its unicode index is printed to decimal and "&#" is appended
    toAsciiChar c = maybe
        (uniEscape c) (\esc -> T.concat ["&", esc, ";"])
        (Map.lookup (T.singleton c) reversePredefinedRefs)

    uniEscape = T.append "&#" . flip T.snoc ';' . T.pack .
                (show :: Int -> String) . fromEnum


------------------------------------------------------------------------------
-- | Gets the decoding function from 'ByteString' to 'Text' for an encoding.
decoder :: Encoding -> ByteString -> Text
decoder UTF8       = T.decodeUtf8With    (TE.replace '\xFFFF')
decoder UTF16BE    = T.decodeUtf16BEWith (TE.replace '\xFFFF')
decoder UTF16LE    = T.decodeUtf16LEWith (TE.replace '\xFFFF')
decoder ISO_8859_1 = T.decodeLatin1 .
                     BS.map (\c -> if isLatin1 c then c else '?')


------------------------------------------------------------------------------
isUTF16 :: Encoding -> Bool
isUTF16 e = e == UTF16BE || e == UTF16LE


------------------------------------------------------------------------------
fromText :: Encoding -> Text -> Builder
fromText e t = fromByteString (encoder e t)


bmap :: (Text -> Text) -> B.Builder -> B.Builder
bmap f   = B.byteString
               . T.encodeUtf8
               . f
               . TL.toStrict
               . TL.decodeUtf8
               . B.toLazyByteString

------------------------------------------------------------------------------
-- Lookup a character code in some monad (usually a parser)
safeChr :: Int -> Parser Char
safeChr c =
    if c > 1114111
      then fail ("Invalid character code: " ++ show c)
      else return (chr c)
