{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module Text.XmlHtml.HTML.Render where

import           Blaze.ByteString.Builder
import           Control.Applicative
import qualified Data.ByteString.Builder as B
import           Data.Maybe
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
import qualified Text.Parsec as P
import           Text.XmlHtml.Common
import           Text.XmlHtml.TextParser
import           Text.XmlHtml.HTML.Meta
import qualified Text.XmlHtml.HTML.Parse as P
import           Text.XmlHtml.XML.Render (docTypeDecl, entity)

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid
#endif

------------------------------------------------------------------------------
-- | And, the rendering code.
renderWithOptions :: RenderOptions -> Encoding -> Maybe DocType -> [Node] -> Builder
renderWithOptions opts e dt ns = byteOrder
       `mappend` docTypeDecl e dt
       `mappend` nodes
    where byteOrder | isUTF16 e = fromText e "\xFEFF" -- byte order mark
                    | otherwise = mempty
          nodes | null ns   = mempty
                | otherwise = firstNode opts e (head ns)
                    `mappend` (mconcat $ map (node opts e) (tail ns))


------------------------------------------------------------------------------
render :: Encoding -> Maybe DocType -> [Node] -> Builder
render = renderWithOptions defaultRenderOptions


------------------------------------------------------------------------------
-- | Function for rendering HTML nodes without the overhead of creating a
-- Document structure.
renderHtmlFragmentWithOptions :: RenderOptions -> Encoding -> [Node] -> Builder
renderHtmlFragmentWithOptions _ _ []     = mempty
renderHtmlFragmentWithOptions opts e (n:ns) =
    firstNode opts e n `mappend` (mconcat $ map (node opts e) ns)


------------------------------------------------------------------------------
-- | Function for rendering HTML nodes without the overhead of creating a
-- Document structure, using default rendering options
renderHtmlFragment :: Encoding -> [Node] -> Builder
renderHtmlFragment = renderHtmlFragmentWithOptions defaultRenderOptions


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
node :: RenderOptions -> Encoding -> Node -> Builder
node _ e (TextNode t)                        = escaped "<>&" e t
node _ e (Comment t) | "--" `T.isInfixOf`  t = error "Invalid comment"
                     | "-"  `T.isSuffixOf` t = error "Invalid comment"
                     | otherwise             = fromText e "<!--"
                                               `mappend` fromText e t
                                               `mappend` fromText e "-->"
node opts e (Element t a c)                     =
    let tbase = T.toLower $ snd $ T.breakOnEnd ":" t
    in  element opts e t tbase a c


------------------------------------------------------------------------------
-- | Process the first node differently to encode leading whitespace.  This
-- lets us be sure that @parseHTML@ is a left inverse to @render@.
firstNode :: RenderOptions -> Encoding -> Node -> Builder
firstNode opts e (Comment t)     = node opts e (Comment t)
firstNode opts e (Element t a c) = node opts e (Element t a c)
firstNode _    _ (TextNode "")   = mempty
firstNode opts e (TextNode t)    = let (c,t') = fromJust $ T.uncons t
                                   in escaped "<>& \t\r" e (T.singleton c)
                                      `mappend` node opts e (TextNode t')


------------------------------------------------------------------------------
-- XXX: Should do something to avoid concatting large CDATA sections before
-- writing them to the output.
element :: RenderOptions -> Encoding -> Text -> Text -> [(Text, Text)] -> [Node] -> Builder
element opts e t tb a c
    | tb `S.member` voidTags && null c         =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute opts e tb) a)
        `mappend` fromText e " />"
    | tb `S.member` voidTags                   =
        error $ T.unpack t ++ " must be empty"
    | isRawText tb a,
      all isTextNode c,
      let s = T.concat (map nodeText c),
      not ("</" `T.append` t `T.isInfixOf` s) =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute opts e tb) a)
        `mappend` fromText e ">"
        `mappend` fromText e s
        `mappend` fromText e "</"
        `mappend` fromText e t
        `mappend` fromText e ">"
    | isRawText tb a,
      [ TextNode _ ] <- c                     =
        error $ T.unpack t ++ " cannot contain text looking like its end tag"
    | isRawText tb a                           =
        error $ T.unpack t ++ " cannot contain child elements or comments"
    | otherwise =
        fromText e "<"
        `mappend` fromText e t
        `mappend` (mconcat $ map (attribute opts e tb) a)
        `mappend` fromText e ">"
        `mappend` (mconcat $ map (node opts e) c)
        `mappend` fromText e "</"
        `mappend` fromText e t
        `mappend` fromText e ">"

------------------------------------------------------------------------------
attribute :: RenderOptions -> Encoding -> Text -> (Text, Text) -> Builder
attribute opts e tb (n,v)
    | v == "" && not explicit                =
        fromText e " "
        `mappend` fromText e n
    | otherwise =
        fromText e " "
        `mappend` fromText e n
        `mappend` fromText e ('=' `T.cons` surround)
        `mappend` bmap (T.replace surround escapeTo) (escaped "&" e v)
        `mappend` fromText e surround
  where
    (surround, escapeTo) = case attributeSurround opts of
        SurroundDoubleQuote -> ("\"", "&quot;")
        SurroundSingleQuote -> ("'", "&apos;")

    nbase    = T.toLower $ snd $ T.breakOnEnd ":" n
    bmap :: (T.Text -> T.Text) -> B.Builder -> B.Builder
    bmap f   = B.byteString
               . T.encodeUtf8
               . f
               . TL.toStrict
               . TL.decodeUtf8
               . B.toLazyByteString
    explicit = case explicitEmptyAttributes opts of
        Nothing  -> True
        -- ^ Nothing 'explicitEmptyAttributes' means: attach '=""' to all
        -- empty attributes
        Just els -> case M.lookup tb els of
            Nothing -> False
            Just ns -> nbase `S.member` ns
