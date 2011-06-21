{-# LANGUAGE OverloadedStrings #-}

module Text.XmlHtml.XML.Render where

import           Blaze.ByteString.Builder
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Text.XmlHtml.Common

import           Data.Text (Text)
import qualified Data.Text as T


------------------------------------------------------------------------------
render :: Encoding -> Maybe DocType -> [Node] -> Builder
render e dt ns = byteOrder
       `mappend` xmlDecl e
       `mappend` docTypeDecl e dt
       `mappend` nodes
    where byteOrder | isUTF16 e = fromText e "\xFEFF" -- byte order mark
                    | otherwise = mempty
          nodes | null ns   = mempty
                | otherwise = firstNode e (head ns)
                    `mappend` (mconcat $ map (node e) (tail ns))


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
firstNode _ (TextNode "")   = mempty
firstNode e (TextNode t)    = let (c,t') = fromJust $ T.uncons t
                              in escaped "<>& \t\r\n" e (T.singleton c)
                                 `mappend` node e (TextNode t')


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
element :: Encoding -> Text -> [(Text, Text)] -> [Node] -> Builder
element e t a [] = fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute e) a)
        `mappend` fromText e "/>"
element e t a c = fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute e) a)
        `mappend` fromText e ">"
        `mappend` (mconcat $ map (node e) c)
        `mappend` fromText e "</"
        `mappend` fromText e t
        `mappend` fromText e ">"


------------------------------------------------------------------------------
attribute :: Encoding -> (Text, Text) -> Builder
attribute e (n,v) | not ("\'" `T.isInfixOf` v) = fromText e " "
                                       `mappend` fromText e n
                                       `mappend` fromText e "=\'"
                                       `mappend` escaped "<&" e v
                                       `mappend` fromText e "\'"
                  | otherwise                  = fromText e " "
                                       `mappend` fromText e n
                                       `mappend` fromText e "=\""
                                       `mappend` escaped "<&\"" e v
                                       `mappend` fromText e "\""

