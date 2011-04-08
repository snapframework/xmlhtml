-- | Renderer that supports rendering to xmlhtml forests.  This is a port of
-- the Hexpat renderer.
--
-- Warning: because this renderer doesn't directly create the output, but
-- rather an XML tree representation, it is impossible to render pre-escaped
-- text. This means that @preEscapedString@ will produce the same output as
-- @string@. This also applies to the functions @preEscapedText@,
-- @preEscapedTextValue@...
--
module Text.Blaze.Renderer.XmlHtml (renderHtml, renderHtmlNodes) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Text.Blaze.Internal
import           Text.XmlHtml


-- | Render a 'ChoiceString' to Text. This is only meant to be used for
-- shorter strings, since it is inefficient for large strings.
--
fromChoiceStringText :: ChoiceString -> Text
fromChoiceStringText (Static s)               = getText s
fromChoiceStringText (String s)               = T.pack s
fromChoiceStringText (Text s)                 = s
fromChoiceStringText (ByteString s)           = T.decodeUtf8 s
fromChoiceStringText (PreEscaped s)           = fromChoiceStringText s
fromChoiceStringText (External s)             = fromChoiceStringText s
fromChoiceStringText (AppendChoiceString x y) =
    fromChoiceStringText x `T.append` fromChoiceStringText y
fromChoiceStringText EmptyChoiceString        = T.empty
{-# INLINE fromChoiceStringText #-}


-- | Render a 'ChoiceString' to an appending list of nodes
--
fromChoiceString :: ChoiceString -> [Node] -> [Node]
fromChoiceString s@(Static _)     = (TextNode (fromChoiceStringText s) :)
fromChoiceString s@(String _)     = (TextNode (fromChoiceStringText s) :)
fromChoiceString s@(Text _)       = (TextNode (fromChoiceStringText s) :)
fromChoiceString s@(ByteString _) = (TextNode (fromChoiceStringText s) :)
fromChoiceString (PreEscaped s)   = fromChoiceString s
fromChoiceString (External s)     = fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x . fromChoiceString y
fromChoiceString EmptyChoiceString = id
{-# INLINE fromChoiceString #-}


-- | Render some 'Html' to an appending list of nodes
--
renderNodes :: Html -> [Node] -> [Node]
renderNodes = go []
  where
    go :: [(Text, Text)] -> HtmlM b -> [Node] -> [Node]
    go attrs (Parent tag _ _ content) =
        (Element (getText tag) attrs (go [] content []) :)
    go attrs (Leaf tag _ _) =
        (Element (getText tag) attrs [] :)
    go attrs (AddAttribute key _ value content) =
        go ((getText key, fromChoiceStringText value) : attrs) content
    go attrs (AddCustomAttribute key _ value content) =
        go ((fromChoiceStringText key, fromChoiceStringText value) : attrs)
           content
    go _ (Content content) = fromChoiceString content
    go attrs (Append h1 h2) = go attrs h1 . go attrs h2
    go _ Empty = id
    {-# NOINLINE go #-}
{-# INLINE renderNodes #-}

-- | Render HTML to an xmlhtml 'Document'
--
renderHtml :: Html -> Document
renderHtml html = HtmlDocument UTF8 Nothing (renderNodes html [])
{-# INLINE renderHtml #-}

-- | Render HTML to a list of xmlhtml nodes
--
renderHtmlNodes :: Html -> [Node]
renderHtmlNodes = flip renderNodes []
