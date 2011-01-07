------------------------------------------------------------------------------
-- | Parsers and renderers for XML and HTML 5.  Although the formats are
--   treated differently, the data types used by each are the same, which
--   makes it easy to write code that works with the element structure of
--   either XML or HTML 5 documents.
--
--   Limitations:
--
--   * The XML parser does not parse internal DOCTYPE subsets.  If one is
--     found, the parsing will fail.
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
    Encoding(..),

    -- * Parsing
    parseXML,
    parseHTML,

    -- * Rendering
    render
    ) where

------------------------------------------------------------------------------
import Blaze.ByteString.Builder (Builder)
import Data.ByteString (ByteString)

import Text.XmlHtml.Common

import qualified Text.XmlHtml.XML.Parse as XML
import qualified Text.XmlHtml.XML.Render as XML

import qualified Text.XmlHtml.HTML.Parse as HTML
import qualified Text.XmlHtml.HTML.Render as HTML

------------------------------------------------------------------------------
-- | Parses the given XML fragment.
parseXML :: ByteString -> Either String Document
parseXML = XML.parse

------------------------------------------------------------------------------
-- | Parses the given HTML fragment.  This enables HTML quirks mode, which
--   changes the parsing algorithm to parse valid HTML 5 documents correctly.
parseHTML :: ByteString -> Either String Document
parseHTML = HTML.parse

------------------------------------------------------------------------------
-- | Renders a 'Document'.
render :: Document -> Builder
render (XmlDocument  e dt ns) = XML.render  e dt ns
render (HtmlDocument e dt ns) = HTML.render e dt ns

