{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module Text.XmlHtml.HTML.Render where

import           Blaze.ByteString.Builder
import           Data.Maybe
import           Data.Monoid
import           Text.XmlHtml.Common
import           Text.XmlHtml.HTML.Meta
import           Text.XmlHtml.XML.Render (docTypeDecl, escaped)

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as S

------------------------------------------------------------------------------
-- | And, the rendering code.
render :: Encoding -> Maybe DocType -> [Node] -> Builder
render e dt ns = byteOrder
       `mappend` docTypeDecl e dt
       `mappend` nodes
    where byteOrder | isUTF16 e = fromText e "\xFEFF" -- byte order mark
                    | otherwise = mempty
          nodes | null ns   = mempty
                | otherwise = firstNode e (head ns)
                    `mappend` (mconcat $ map (node e) (tail ns))


------------------------------------------------------------------------------
node :: Encoding -> Node -> Builder
node e (TextNode t)                        = escaped "<>&" e t
node e (Comment t) | "--" `T.isInfixOf` t  = error "Invalid comment"
                   | "-" `T.isSuffixOf` t  = error "Invalid comment"
                   | otherwise             = fromText e "<!--"
                                             `mappend` fromText e t
                                             `mappend` fromText e "-->"
node e (Element t a c)                     = element e t a c


------------------------------------------------------------------------------
-- | Process the first node differently to encode leading whitespace.  This
-- lets us be sure that @parseXML@ is a left inverse to @render@.
firstNode :: Encoding -> Node -> Builder
firstNode e (Comment t)     = node e (Comment t)
firstNode e (Element t a c) = node e (Element t a c)
firstNode e (TextNode t)    = let (c,t') = fromJust $ T.uncons t
                              in escaped "<>& \t\r\n" e (T.singleton c)
                                 `mappend` node e (TextNode t')


------------------------------------------------------------------------------
element :: Encoding -> Text -> [(Text, Text)] -> [Node] -> Builder
element e t a c
    | t `S.member` voidTags && null c         =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute e) a)
        `mappend` fromText e " />"
    | t `S.member` voidTags                   =
        error $ T.unpack t ++ " must be empty"
    | t `S.member` rawTextTags,
      [ TextNode s ] <- c,
      not ("</" `T.append` t `T.isInfixOf` s) =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute e) a)
        `mappend` fromText e ">"
        `mappend` fromText e s
        `mappend` fromText e "</"
        `mappend` fromText e t
        `mappend` fromText e ">"
    | t `S.member` rawTextTags,
      [ TextNode _ ] <- c                     =
        error $ T.unpack t ++ " cannot contain text looking like its end tag"
    | t `S.member` rawTextTags                =
        error $ T.unpack t ++ " cannot contain child elements or comments"
    | t `S.member` rcdataTags,
      [ TextNode s ] <- c                     =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute e) a)
        `mappend` fromText e ">"
        `mappend` escaped "<&" e s
        `mappend` fromText e "</"
        `mappend` fromText e t
        `mappend` fromText e ">"
    | t `S.member` rcdataTags                 =
        error $ T.unpack t ++ " cannot contain child elements or comments"
    | otherwise =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute e) a)
        `mappend` fromText e ">"
        `mappend` (mconcat $ map (node e) c)
        `mappend` fromText e "</"
        `mappend` fromText e t
        `mappend` fromText e ">"


------------------------------------------------------------------------------
attribute :: Encoding -> (Text, Text) -> Builder
attribute e (n,v)
    | v == ""                    =
        fromText e " "
        `mappend` fromText e n
    | not ("\'" `T.isInfixOf` v) =
        fromText e " "
        `mappend` fromText e n
        `mappend` fromText e "=\'"
        `mappend` escaped "&\'" e v
        `mappend` fromText e "\'"
    | otherwise                  =
        fromText e " "
        `mappend` fromText e n
        `mappend` fromText e "=\""
        `mappend` escaped "&\"" e v
        `mappend` fromText e "\""

