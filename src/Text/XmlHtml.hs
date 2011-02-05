------------------------------------------------------------------------------
-- | Parsers and renderers for XML and HTML 5.  Although the formats are
--   treated differently, the data types used by each are the same, which
--   makes it easy to write code that works with the element structure of
--   either XML or HTML 5 documents.
--
--   Limitations:
--
--   * The XML parser does not parse internal DOCTYPE subsets.  They are just
--     stored as blocks of text, with minimal scanning done to match quotes
--     and brackets to determine the end.
--
--   * Since DTDs are not parsed, the XML parser fails on entity references,
--     except for those defined internally.  You cannot use this library for
--     parsing XML documents with entity references outside the predefined
--     set.
--
--   * The HTML 5 parser is not a compliant HTML parser.  Instead, it is a
--     parser for valid HTML 5 content.  It should only be used on content
--     that you have reason to believe is probably correct, since the
--     compatibility features of HTML 5 are missing.  This is the wrong
--     library on which to build a web spider.
--
--   * Both parsers accept fragments of documents, by which is meant that
--     they do not enforce the top-level structure of the document.  Files
--     may contain more than one root element, for example.
module Text.XmlHtml (
    -- * Types
    Document(..),
    Node(..),
    DocType(..),
    ExternalID(..),
    InternalSubset(..),
    Encoding(..),

    -- * Manipulating documents
    isTextNode,
    isComment,
    isElement,
    tagName,
    getAttribute,
    hasAttribute,
    setAttribute,
    nodeText,
    childNodes,
    childElements,
    childElementsTag,
    childElementTag,
    descendantNodes,
    descendantElements,
    descendantElementsTag,
    descendantElementTag,

    -- * Parsing
    parseXML,
    parseHTML,

    -- * Rendering
    render
    ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder (Builder)
import           Data.ByteString (ByteString)

import           Text.XmlHtml.Common
import           Text.XmlHtml.TextParser

import qualified Text.XmlHtml.XML.Parse as XML
import qualified Text.XmlHtml.XML.Render as XML

import qualified Text.XmlHtml.HTML.Parse as HTML
import qualified Text.XmlHtml.HTML.Render as HTML


------------------------------------------------------------------------------
-- | Parses the given XML fragment.
parseXML :: String
         -- ^ Name of document source (perhaps a filename) for error messages
         -> ByteString
         -- ^ Document contents
         -> Either String Document
         -- ^ The document or an error message
parseXML = parse XML.docFragment


------------------------------------------------------------------------------
-- | Parses the given HTML fragment.  This enables HTML quirks mode, which
--   changes the parsing algorithm to parse valid HTML 5 documents correctly.
parseHTML :: String
          -- ^ Name of document source (perhaps a filename) for error messages
          -> ByteString
          -- ^ Document contents
          -> Either String Document
          -- ^ The document or an error message
parseHTML = parse HTML.docFragment


------------------------------------------------------------------------------
-- | Renders a 'Document'.
render :: Document -> Builder
render (XmlDocument  e dt ns) = XML.render  e dt ns
render (HtmlDocument e dt ns) = HTML.render e dt ns

