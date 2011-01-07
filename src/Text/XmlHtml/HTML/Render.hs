{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module Text.XmlHtml.HTML.Render where

import Blaze.ByteString.Builder
import Data.Monoid
import Text.XmlHtml.Common
import Text.XmlHtml.HTML.Meta

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as S

------------------------------------------------------------------------------
-- | Convenience functions that are useful later.
isTextNode :: Node -> Bool
isTextNode (TextNode _) = True
isTextNode _            = False


------------------------------------------------------------------------------
fromText :: Encoding -> Text -> Builder
fromText e t = fromByteString (encoder e t)


------------------------------------------------------------------------------
-- | And, the rendering code.
render :: Encoding -> Maybe DocType -> [Node] -> Builder
render e dt ns = byteOrder
       `mappend` docType e dt
       `mappend` (mconcat $ map (node e) ns)
    where byteOrder | isUTF16 e = fromText e "\xFEFF" -- byte order mark
                    | otherwise = mempty


------------------------------------------------------------------------------
docType :: Encoding -> Maybe DocType -> Builder
docType _ Nothing                  = mempty
docType e (Just (DocType tag ext)) = fromText e "<!DOCTYPE "
                           `mappend` fromText e tag
                           `mappend` externalID e ext
                           `mappend` fromText e ">"


------------------------------------------------------------------------------
externalID :: Encoding -> Maybe ExternalID -> Builder
externalID _ Nothing                 = mempty
externalID e (Just (System sid))     = fromText e " SYSTEM "
                                       `mappend` sysID e sid
externalID e (Just (Public pid sid)) = fromText e " SYSTEM "
                                       `mappend` pubID e pid
                                       `mappend` fromText e " "
                                       `mappend` sysID e sid


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
node :: Encoding -> Node -> Builder
node e (TextNode t)                        = escaped "<&" e t
node e (Comment t) | "--" `T.isInfixOf` t  = error "Invalid comment"
                   | "-" `T.isSuffixOf` t  = error "Invalid comment"
                   | otherwise             = fromText e "<!--"
                                             `mappend` fromText e t
                                             `mappend` fromText e "-->"
node e (Element t a c)                     = element e t a c


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
entity e '\'' = fromText e "&apos;"
entity e '\"' = fromText e "&quot;"
entity _ _    = error "Misbehaving renderer"


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

