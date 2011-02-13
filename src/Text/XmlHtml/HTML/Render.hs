{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module Text.XmlHtml.HTML.Render where

import           Blaze.ByteString.Builder
import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import qualified Text.Parsec as P
import           Text.XmlHtml.Common
import           Text.XmlHtml.TextParser
import           Text.XmlHtml.HTML.Meta
import qualified Text.XmlHtml.HTML.Parse as P
import           Text.XmlHtml.XML.Render (docTypeDecl, entity)

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
-- | HTML allows & so long as it is not "ambiguous" (i.e., looks like an
-- entity).  So we have a special case for that.
escaped :: [Char] -> Encoding -> Text -> Builder
escaped _   _ "" = mempty
escaped bad e t  =
    let (p,s) = T.break (`elem` bad) t
        r     = T.uncons s
    in  fromText e p `mappend` case r of
            Nothing
                -> mempty
            Just ('&',ss) | isLeft (parseText ambigAmp "" s)
                -> fromText e "&" `mappend` escaped bad e ss
            Just (c,ss)
                -> entity e c `mappend` escaped bad e ss
  where isLeft   = either (const True) (const False)
        ambigAmp = P.char '&' *>
            (P.finishCharRef *> return () <|> P.finishEntityRef *> return ())


------------------------------------------------------------------------------
node :: Encoding -> Node -> Builder
node e (TextNode t)                        = escaped "<>&" e t
node e (Comment t) | "--" `T.isInfixOf`  t = error "Invalid comment"
                   | "-"  `T.isSuffixOf` t = error "Invalid comment"
                   | otherwise             = fromText e "<!--"
                                             `mappend` fromText e t
                                             `mappend` fromText e "-->"
node e (Element t a c)                     =
    let tbase = T.toLower $ snd $ T.breakOnEnd ":" t
    in  element e t tbase a c


------------------------------------------------------------------------------
-- | Process the first node differently to encode leading whitespace.  This
-- lets us be sure that @parseHTML@ is a left inverse to @render@.
firstNode :: Encoding -> Node -> Builder
firstNode e (Comment t)     = node e (Comment t)
firstNode e (Element t a c) = node e (Element t a c)
firstNode _ (TextNode "")   = mempty
firstNode e (TextNode t)    = let (c,t') = fromJust $ T.uncons t
                              in escaped "<>& \t\r\n" e (T.singleton c)
                                 `mappend` node e (TextNode t')


------------------------------------------------------------------------------
-- XXX: Should do something to avoid concatting large CDATA sections before
-- writing them to the output.
element :: Encoding -> Text -> Text -> [(Text, Text)] -> [Node] -> Builder
element e t tb a c
    | tb `S.member` voidTags && null c         =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute e) a)
        `mappend` fromText e " />"
    | tb `S.member` voidTags                   =
        error $ T.unpack t ++ " must be empty"
    | tb `S.member` rawTextTags,
      all isTextNode c,
      let s = T.concat (map nodeText c),
      not ("</" `T.append` t `T.isInfixOf` s) =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute e) a)
        `mappend` fromText e ">"
        `mappend` fromText e s
        `mappend` fromText e "</"
        `mappend` fromText e t
        `mappend` fromText e ">"
    | tb `S.member` rawTextTags,
      [ TextNode _ ] <- c                     =
        error $ T.unpack t ++ " cannot contain text looking like its end tag"
    | tb `S.member` rawTextTags                =
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
        `mappend` escaped "&" e v
        `mappend` fromText e "\'"
    | otherwise                  =
        fromText e " "
        `mappend` fromText e n
        `mappend` fromText e "=\""
        `mappend` escaped "&\"" e v
        `mappend` fromText e "\""

