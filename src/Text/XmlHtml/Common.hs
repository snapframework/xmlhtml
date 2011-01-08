{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.XmlHtml.Common where

import           Blaze.ByteString.Builder
import           Control.Applicative
import           Data.Maybe

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Text.Parsec (Parsec)
import qualified Text.Parsec as P

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B


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
data DocType = DocType !Text !(Maybe ExternalID)
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- | An external ID, as in a document type declaration.  This can be a
-- SYSTEM identifier, or a PUBLIC identifier.
data ExternalID = System !Text
                | Public !Text !Text
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- | The character encoding of a document.  Currently only the required
-- character encodings are implemented.
data Encoding = UTF8 | UTF16BE | UTF16LE deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Retrieves the preferred name of a character encoding for embedding in
-- a document.
encodingName :: Encoding -> Text
encodingName UTF8    = "UTF-8"
encodingName UTF16BE = "UTF-16"
encodingName UTF16LE = "UTF-16"


------------------------------------------------------------------------------
-- | Gets the encoding function from 'Text' to 'ByteString' for an encoding.
encoder :: Encoding -> Text -> ByteString
encoder UTF8    = T.encodeUtf8
encoder UTF16BE = T.encodeUtf16BE
encoder UTF16LE = T.encodeUtf16LE


------------------------------------------------------------------------------
-- | Gets the decoding function from 'ByteString' to 'Text' for an encoding.
decoder :: Encoding -> ByteString -> Text
decoder UTF8    = T.decodeUtf8
decoder UTF16BE = T.decodeUtf16BE
decoder UTF16LE = T.decodeUtf16LE


------------------------------------------------------------------------------
isUTF16 :: Encoding -> Bool
isUTF16 e = e == UTF16BE || e == UTF16LE


------------------------------------------------------------------------------
fromText :: Encoding -> Text -> Builder
fromText e t = fromByteString (encoder e t)


------------------------------------------------------------------------------
-- | Get an initial guess at document encoding from the byte order mark.  If
-- the mark doesn't exist, guess UTF-8.  Otherwise, guess according to the
-- mark.
guessEncoding :: ByteString -> (Encoding, ByteString)
guessEncoding b
    | B.take 3 b == B.pack [ 0xEF, 0xBB, 0xBF ] = (UTF8,    B.drop 3 b)
    | B.take 2 b == B.pack [ 0xFE, 0xFF ]       = (UTF16BE, B.drop 2 b)
    | B.take 2 b == B.pack [ 0xFF, 0xFE ]       = (UTF16LE, B.drop 2 b)
    | otherwise                                 = (UTF8,    b)


------------------------------------------------------------------------------
-- | Specialized type for the parsers we use here.
type Parser = Parsec Text ()


------------------------------------------------------------------------------
-- An (orphaned) instance for parsing Text with Parsec.
instance (Monad m) => P.Stream T.Text m Char where
    uncons = return . T.uncons


------------------------------------------------------------------------------
-- | Parses a 'Text' value and gives back the result.  The parser is expected
-- to match the entire string.
parseText :: Parser a         -- ^ The parser to match
          -> String           -- ^ Name of the source file (can be @""@)
          -> Text             -- ^ Text to parse
          -> Either String a  -- Either an error message or the result
parseText p src t = inLeft show (P.parse p src t)
  where inLeft :: (a -> b) -> Either a c -> Either b c
        inLeft f (Left x)  = Left (f x)
        inLeft _ (Right x) = Right x


------------------------------------------------------------------------------
-- | Consume input as long as the predicate returns 'True', and return the
-- consumed input.  This parser does not fail.  If it matches no input, it
-- will return an empty string.
takeWhile0 :: (Char -> Bool) -> Parser Text
takeWhile0 p = fmap T.pack $ P.many $ P.satisfy p


------------------------------------------------------------------------------
-- | Consume input as long as the predicate returns 'True', and return the
-- consumed input.  This parser requires the predicate to succeed on at least
-- one character of input.  It will fail if the first character fails the
-- predicate.
takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = fmap T.pack $ P.many1 $ P.satisfy p


------------------------------------------------------------------------------
-- | The equivalent of Parsec's string combinator, but for text.  If there is
-- not a complete match, then no input is consumed.  This matches the behavior
-- of @string@ from the attoparsec-text package.
text :: Text -> Parser Text
text t = P.try $ P.string (T.unpack t) *> return t

