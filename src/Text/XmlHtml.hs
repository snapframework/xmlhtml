module Text.XmlHtml (
    Document(..),
    Node(..),
    DocType(..),
    ExternalID(..),
    Encoding(..),
    parseXML,
    parseHTML,
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
parseXML :: ByteString -> Either String Document
parseXML = XML.parse


------------------------------------------------------------------------------
parseHTML :: ByteString -> Either String Document
parseHTML = HTML.parse


------------------------------------------------------------------------------
render :: Document -> Builder
render (XmlDocument  e dt ns) = XML.render  e dt ns
render (HtmlDocument e dt ns) = HTML.render e dt ns

