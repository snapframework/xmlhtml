{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module Text.XmlHtml.HTML.Render where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8 (fromChar)
import qualified Blaze.ByteString.Builder.Html.Utf8 as Utf
import           Blaze.ByteString.Builder.Internal
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Text.XmlHtml.Common
import           Text.XmlHtml.HTML.Meta
import qualified Text.XmlHtml.XML.Parse as P
import           Text.XmlHtml.XML.Render (docTypeDecl, entity)

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import qualified Data.HashSet as S

------------------------------------------------------------------------------
-- | Render a node list into a builder using the given encoding.
render :: Encoding -> Maybe DocType -> [Node] -> Builder
render e dt ns = case e of
                   UTF8 -> utf8Render dt ns
                   _    -> utf16Render e dt ns


------------------------------------------------------------------------------
utf8Render :: Maybe DocType -> [Node] -> Builder
utf8Render dt ns = docTypeDecl UTF8 dt `mappend` nodes
  where
    nodes = case ns of
              [] -> mempty
              (z:zs) -> utf8FirstNode z `mappend`
                        foldr (\x b -> utf8Node x `mappend` b) mempty zs
{-# INLINE utf8Render #-}


------------------------------------------------------------------------------
utf16Render :: Encoding -> Maybe DocType -> [Node] -> Builder
utf16Render e dt ns =
    fromText e "\xFEFF" `mappend` docTypeDecl e dt `mappend` nodes
  where
    nodes = foldr (\x b -> utf16Node e x `mappend` b) mempty ns


------------------------------------------------------------------------------
utf8Node :: Node -> Builder
utf8Node (TextNode t)    = Utf.fromHtmlEscapedText t

utf8Node (Comment t)
    | commentIsInvalid t = error "invalid comment"
    | otherwise          = mconcat [ fromByteString "<!--"
                                   , Utf.fromText t
                                   , fromByteString "-->" ]

utf8Node (Element t a c) = utf8Element t tbase a c
  where
    tbase = T.toLower $ snd $ T.breakOnEnd ":" t


------------------------------------------------------------------------------
utf8FirstNode :: Node -> Builder
utf8FirstNode (TextNode "") = mempty
utf8FirstNode (TextNode t)  = let (c, t') = fromJust $ T.uncons t
                              in escaped "<>& \t\r\n" UTF8 (T.singleton c)
                                 `mappend` utf8Node (TextNode t')
utf8FirstNode n = utf8Node n


------------------------------------------------------------------------------
commentIsInvalid :: Text -> Bool
commentIsInvalid t
    | "--" `T.isInfixOf` t  = True
    | "-"  `T.isSuffixOf` t = True
    | otherwise             = False


------------------------------------------------------------------------------
utf8Element :: Text -> Text -> [(Text, Text)] -> [Node] -> Builder
utf8Element t tbase a c
    | tbase `S.member` voidTags    = voidTag
    | tbase `S.member` rawTextTags = rawTag
    | otherwise                    = normalTag

  where
    --------------------------------------------------------------------------
    tbuild     = Utf.fromText t
    attributes = foldr (\x b -> utf8Attribute x `mappend` b) mempty a

    --------------------------------------------------------------------------
    voidTag = {-# SCC "utf8Element/voidTag" #-}
              if null c
                then mconcat [ fromChar '<'
                             , tbuild
                             , attributes
                             , fromByteString " />" ]

                else error $ T.unpack t ++ " must be empty"

    --------------------------------------------------------------------------
    rawTag = {-# SCC "utf8Element/rawTag" #-}
             if (all isTextNode c) && ok
               then mconcat [ fromChar '<'
                            , tbuild
                            , attributes
                            , fromChar '>'
                            , Utf.fromLazyText haystack
                            , fromChar '<'
                            , fromChar '/'
                            , tbuild
                            , fromChar '>' ]

               else error $ concat [
                          T.unpack t
                        , " cannot contain non-text children or text looking "
                        , "like its end tag." ]
      where
        ok       = not (needle `LT.isInfixOf` haystack)
        needle   = LT.fromChunks [ "</", t ]
        haystack = LT.fromChunks $ map nodeText c

    --------------------------------------------------------------------------
    normalTag = {-# SCC "utf8Element/normalTag" #-}
                mconcat [ fromChar '<'
                        , tbuild
                        , attributes
                        , fromChar '>'
                        , foldr (\x b -> utf8Node x `mappend` b) mempty c
                        , fromChar '<'
                        , fromChar '/'
                        , tbuild
                        , fromChar '>' ]


------------------------------------------------------------------------------
utf8Attribute :: (Text, Text) -> Builder
utf8Attribute (n, v) | T.null v = fromChar ' ' `mappend` nbuild
                     | not ("\'" `T.isInfixOf` v) =
                         mconcat [ fromChar ' '
                                 , nbuild
                                 , fromChar '='
                                 , fromChar '\''
                                 , sqEscape v
                                 , fromChar '\''
                                 ]
                     | otherwise =
                         mconcat [ fromChar ' '
                                 , nbuild
                                 , fromChar '='
                                 , fromChar '"'
                                 , dqEscape v
                                 , fromChar '"'
                                 ]
  where
    nbuild = Utf.fromHtmlEscapedText n

    sqSubst c = Utf.fromChar c

    sqEscape = escape sqPred sqSubst
    sqPred c = c == '&'

    dqEscape = escape dqPred dqSubst

    dqSubst '\"' = fromByteString "&quot;"
    dqSubst c    = Utf.fromChar c

    dqPred c = c == '"' || c == '&'

    escape p subst = {-# SCC "utf8Attribute/escape" #-} go mempty
      where
        go bl t = let (a,b) = T.break p t
                      bl'   = bl `mappend` Utf.fromText a
                      r     = T.uncons b
                  in case r of
                       Nothing -> bl'
                       Just ('&',ss) ->
                           let str = T.unpack b
                           in if ambiguousAmpersand str
                                then go (bl' `mappend`
                                             fromByteString "&amp;") ss
                                else go (bl' `mappend`
                                            fromWord8 0x26) ss
                       Just (c, ss) -> go (bl' `mappend` subst c) ss


------------------------------------------------------------------------------
-- UTF-16 render code follows; TODO: optimize

ambiguousAmpersand :: String -> Bool
ambiguousAmpersand [] = False
ambiguousAmpersand ('&':s) = ambig2 s
  where
    ambig2 []       = False
    ambig2 ('#':xs) = ambigCharRefStart xs
    ambig2 (x:xs)
        | P.isNameStartChar x = ambigEntity xs
        | otherwise           = False

    ambigCharRefStart [] = False
    ambigCharRefStart (x:xs)
        | isDigit x            = ambigCharRef xs
        | x == 'x' || x == 'X' = ambigHexCharRef xs
        | otherwise            = False

    ambigCharRef [] = False
    ambigCharRef (x:xs)
        | x == ';'  = True
        | isDigit x = ambigCharRef xs
        | otherwise = False

    ambigHexCharRef [] = False
    ambigHexCharRef (x:xs)
        | x == ';'     = True
        | isHexDigit x = ambigCharRef xs
        | otherwise    = False

    ambigEntity [] = False
    ambigEntity (x:xs)
        | x == ';'       = True
        | P.isNameChar x = ambigEntity xs
        | otherwise      = False
ambiguousAmpersand _ = False


------------------------------------------------------------------------------
-- | Function for rendering HTML nodes without the overhead of creating a
-- Document structure.
renderHtmlFragment :: Encoding -> [Node] -> Builder
renderHtmlFragment _ []     = mempty
renderHtmlFragment e (n:ns) =
    firstNode e n `mappend` (mconcat $ map (node e) ns)


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
            Just ('&',ss) | ambiguousAmpersand $ T.unpack s
                -> fromText e "&" `mappend` escaped bad e ss
            Just (c,ss)
                -> entity e c `mappend` escaped bad e ss


------------------------------------------------------------------------------
utf16Node :: Encoding -> Node -> Builder
utf16Node e (TextNode t)                        = escaped "<>&" e t
utf16Node e (Comment t) | "--" `T.isInfixOf`  t = error "Invalid comment"
                        | "-"  `T.isSuffixOf` t = error "Invalid comment"
                        | otherwise             = fromText e "<!--"
                                                  `mappend` fromText e t
                                                  `mappend` fromText e "-->"
utf16Node e (Element t a c)                     =
    let tbase = T.toLower $ snd $ T.breakOnEnd ":" t
    in  utf16Element e t tbase a c


------------------------------------------------------------------------------
-- XXX: Should do something to avoid concatting large CDATA sections before
-- writing them to the output.
utf16Element :: Encoding -> Text -> Text -> [(Text, Text)] -> [Node]
             -> Builder
utf16Element e t tb a c
    | tb `S.member` voidTags && null c         =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (utf16Attribute e) a)
        `mappend` fromText e " />"
    | tb `S.member` voidTags                   =
        error $ T.unpack t ++ " must be empty"
    | tb `S.member` rawTextTags,
      all isTextNode c,
      let s = T.concat (map nodeText c),
      not ("</" `T.append` t `T.isInfixOf` s) =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (utf16Attribute e) a)
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
        `mappend` (mconcat $ map (utf16Attribute e) a)
        `mappend` fromText e ">"
        `mappend` (mconcat $ map (utf16Node e) c)
        `mappend` fromText e "</"
        `mappend` fromText e t
        `mappend` fromText e ">"


------------------------------------------------------------------------------
utf16Attribute :: Encoding -> (Text, Text) -> Builder
utf16Attribute e (n,v)
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
