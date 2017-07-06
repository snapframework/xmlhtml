
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.XmlHtml.XML.Render where

import qualified Data.ByteString.Builder as B
import           Blaze.ByteString.Builder
import           Data.Char
import           Data.Maybe
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Text.XmlHtml.Common

import           Data.Text (Text)
import qualified Data.Text as T

#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid
#endif


------------------------------------------------------------------------------
renderWithOptions :: RenderOptions -> Encoding -> Maybe DocType -> [Node] -> Builder
renderWithOptions opts e dt ns = byteOrder
       `mappend` xmlDecl e
       `mappend` docTypeDecl e dt
       `mappend` nodes
    where byteOrder | isUTF16 e = fromText e "\xFEFF" -- byte order mark
                    | otherwise = mempty
          nodes | null ns   = mempty
                | otherwise = firstNode opts e (head ns)
                    `mappend` (mconcat $ map (node opts e) (tail ns))


render :: Encoding -> Maybe DocType -> [Node] -> Builder
render = renderWithOptions defaultRenderOptions

------------------------------------------------------------------------------
-- | Function for rendering XML nodes without the overhead of creating a
-- Document structure.
renderXmlFragmentWithOptions :: RenderOptions -> Encoding -> [Node] -> Builder
renderXmlFragmentWithOptions _    _ []     = mempty
renderXmlFragmentWithOptions opts e (n:ns) =
    firstNode opts e n `mappend` (mconcat $ map (node opts e) ns)

renderXmlFragment :: Encoding -> [Node] -> Builder
renderXmlFragment = renderXmlFragmentWithOptions defaultRenderOptions

------------------------------------------------------------------------------
xmlDecl :: Encoding -> Builder
xmlDecl e = fromText e "<?xml version=\"1.0\" encoding=\""
            `mappend` fromText e (encodingName e)
            `mappend` fromText e "\"?>\n"


------------------------------------------------------------------------------
docTypeDecl :: Encoding -> Maybe DocType -> Builder
docTypeDecl _ Nothing                      = mempty
docTypeDecl e (Just (DocType tag ext int)) = fromText e "<!DOCTYPE "
                                   `mappend` fromText e tag
                                   `mappend` externalID e ext
                                   `mappend` internalSubset e int
                                   `mappend` fromText e ">\n"


------------------------------------------------------------------------------
externalID :: Encoding -> ExternalID -> Builder
externalID _ NoExternalID     = mempty
externalID e (System sid)     = fromText e " SYSTEM "
                                `mappend` sysID e sid
externalID e (Public pid sid) = fromText e " PUBLIC "
                                `mappend` pubID e pid
                                `mappend` fromText e " "
                                `mappend` sysID e sid


------------------------------------------------------------------------------
internalSubset :: Encoding -> InternalSubset -> Builder
internalSubset _ NoInternalSubset = mempty
internalSubset e (InternalText t) = fromText e " " `mappend` fromText e t


------------------------------------------------------------------------------
sysID :: Encoding -> Text -> Builder
sysID e sid | not ("\'" `T.isInfixOf` sid) = fromText e "\'"
                                             `mappend` fromText e sid
                                             `mappend` fromText e "\'"
            | not ("\"" `T.isInfixOf` sid) = fromText e "\""
                                             `mappend` fromText e sid
                                             `mappend` fromText e "\""
            | otherwise               = error "SYSTEM id is invalid"


------------------------------------------------------------------------------
pubID :: Encoding -> Text -> Builder
pubID e sid | not ("\"" `T.isInfixOf` sid) = fromText e "\""
                                             `mappend` fromText e sid
                                             `mappend` fromText e "\""
            | otherwise               = error "PUBLIC id is invalid"


------------------------------------------------------------------------------
node :: RenderOptions -> Encoding -> Node -> Builder
node _    e (TextNode t)                        = escaped "<>&" e t
node _    e (Comment t) | "--" `T.isInfixOf` t  = error "Invalid comment"
                        | "-" `T.isSuffixOf` t  = error "Invalid comment"
                        | otherwise             = fromText e "<!--"
                                                  `mappend` fromText e t
                                                  `mappend` fromText e "-->"
node opts e (Element t a c)                     = element opts e t a c


------------------------------------------------------------------------------
-- | Process the first node differently to encode leading whitespace.  This
-- lets us be sure that @parseXML@ is a left inverse to @render@.
firstNode :: RenderOptions -> Encoding -> Node -> Builder
firstNode opts e (Comment t)     = node opts e (Comment t)
firstNode opts e (Element t a c) = node opts e (Element t a c)
firstNode _    _ (TextNode "")   = mempty
firstNode opts e (TextNode t)    = let (c,t') = fromJust $ T.uncons t
                                   in escaped "<>& \t\r" e (T.singleton c)
                                      `mappend` node opts e (TextNode t')


------------------------------------------------------------------------------
escaped :: [Char] -> Encoding -> Text -> Builder
escaped _   _ "" = mempty
escaped bad e t  = let (p,s) = T.break (`elem` bad) t
                       r     = T.uncons s
                   in  fromText e p `mappend` case r of
                         Nothing     -> mempty
                         Just (c,ss) -> entity e c `mappend` escaped bad e ss


------------------------------------------------------------------------------
entity :: Encoding -> Char -> Builder
entity e '&'  = fromText e "&amp;"
entity e '<'  = fromText e "&lt;"
entity e '>'  = fromText e "&gt;"
entity e '\"' = fromText e "&quot;"
entity e c    = fromText e "&#"
                `mappend` fromText e (T.pack (show (ord c)))
                `mappend` fromText e ";"


------------------------------------------------------------------------------
element :: RenderOptions -> Encoding -> Text -> [(Text, Text)] -> [Node] -> Builder
element opts e t a [] = fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute opts e) a)
        `mappend` fromText e "/>"
element opts e t a c = fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute opts e) a)
        `mappend` fromText e ">"
        `mappend` (mconcat $ map (node opts e) c)
        `mappend` fromText e "</"
        `mappend` fromText e t
        `mappend` fromText e ">"


------------------------------------------------------------------------------
attribute :: RenderOptions -> Encoding -> (Text, Text) -> Builder
attribute opts e (n,v)
    | roAttributeResolveInternal opts == AttrResolveAvoidEscape
      && preferredSurround `T.isInfixOf` v
      && not (otherSurround `T.isInfixOf` v) =
      fromText e " "
      `mappend` fromText e n
      `mappend` fromText e (T.cons '=' otherSurround)
      `mappend` escaped "<&" e v
      `mappend` fromText e otherSurround
    | otherwise =
      fromText e " "
      `mappend` fromText e n
      `mappend` fromText e (T.cons '=' preferredSurround)
      `mappend` bmap (T.replace preferredSurround ent) (escaped "<&" e v)
      `mappend` fromText e preferredSurround
  where
    (preferredSurround, otherSurround, ent) = case roAttributeSurround opts of
        SurroundSingleQuote -> ("\'", "\"", "&apos;")
        SurroundDoubleQuote -> ("\"", "\'", "&quot;")
    bmap :: (T.Text -> T.Text) -> B.Builder -> B.Builder
    bmap f   = B.byteString
               . T.encodeUtf8
               . f
               . TL.toStrict
               . TL.decodeUtf8
               . B.toLazyByteString
