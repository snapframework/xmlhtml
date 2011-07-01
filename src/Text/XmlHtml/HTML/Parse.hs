{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Text.XmlHtml.HTML.Parse where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Text.XmlHtml.Common
import           Text.XmlHtml.HTML.Meta
import           Text.XmlHtml.TextParser
import qualified Text.XmlHtml.XML.Parse as XML

import qualified Text.Parsec as P

import qualified Data.Set as S
import qualified Data.Map as M

import           Data.Text (Text)
import qualified Data.Text as T


------------------------------------------------------------------------------
-- | HTML version of document fragment parsing rule  It differs only in that
-- it parses the HTML version of 'content' and returns an 'HtmlDocument'.
docFragment :: Encoding -> Parser Document
docFragment e = do
    (dt, nodes1)      <- prolog
    (nodes2, Matched) <- content Nothing
    return $ HtmlDocument e dt (nodes1 ++ nodes2)


------------------------------------------------------------------------------
-- Parsing code                                                             --
------------------------------------------------------------------------------

{-
    The following are the differences between this code and the straight XML
    parsing code.

    1. HTML void tags (area, base, etc.) are always treated as empty tags,
       regardless of whether they have the empty-tag slash.

    2. HTML raw text tags (script and style) are parsed as straight text
       with neither markup nor references, except that they end at the first
       syntactically valid matching end tag.

    3. End tags need only match their corresponding start tags in a case
       insensitive comparison.  In case they are different, the start tag is
       used for the element tag name.

    4. Hexadecimal char references may use &#X...; (capital X)  -- DONE

    5. Attribute names are allowed to consist of any text except for control
       characters, space, '\"', '\'', '>', '/', or '='.

    6. Empty attribute syntax is allowed (an attribute not followed by an eq).
       In this case, the attribute value is considered to be the empty string.

    7. Quoted attribute syntax is relaxed to allow any character except for
       the matching quote.  References are allowed.

    8. Attribute values may be unquoted.  In this case, the attribute value
       may not contain space, single or double quotes, '=', '<', '>', or '`',
       and may not be the empty string.  It can still contain references.

    9. There are many more character references available.

    10. Only "ambiguous" ampersands are prohibited in character data.  This
        means ampersands that parse like character or entity references.

    11. Omittable end tags are inserted automatically.

    12. DOCTYPE tags matched with case insensitive keywords.
-}


------------------------------------------------------------------------------
prolog :: Parser (Maybe DocType, [Node])
prolog = do
    _      <- optional XML.xmlDecl
    nodes1 <- many XML.misc
    rest   <- optional $ do
        dt     <- docTypeDecl
        nodes2 <- many XML.misc
        return (dt, nodes2)
    case rest of
        Nothing           -> return (Nothing, catMaybes nodes1)
        Just (dt, nodes2) -> return (Just dt, catMaybes (nodes1 ++ nodes2))


------------------------------------------------------------------------------
-- | Internal subset is parsed, but ignored since we don't have data types to
-- store it.
docTypeDecl :: Parser DocType
docTypeDecl = do
    P.try $ do
        _      <- text "<!"
        decl   <- XML.name
        when (T.toLower decl /= "doctype") $ fail "Expected DOCTYPE"
    XML.whiteSpace
    tag    <- XML.name
    _      <- optional XML.whiteSpace
    extid  <- externalID
    _      <- optional XML.whiteSpace
    intsub <- XML.internalDoctype
    _      <- P.char '>'
    return (DocType tag extid intsub)


------------------------------------------------------------------------------
externalID :: Parser ExternalID
externalID = do
    tok  <- optional $ T.toLower <$> XML.name
    case tok of
        Just "system" -> systemID
        Just "public" -> publicID
        Just _        -> fail "Expected SYSTEM or PUBLIC"
        Nothing       -> return NoExternalID
  where
    systemID = do
        XML.whiteSpace
        System <$> XML.systemLiteral
    publicID = do
        XML.whiteSpace
        pid <- XML.pubIdLiteral
        XML.whiteSpace
        sid <- XML.systemLiteral
        return (Public pid sid)


------------------------------------------------------------------------------
-- | When parsing an element, three things can happen (besides failure):
--
-- (1) The end tag matches the start tag.  This is a Matched.
--
-- (2) The end tag does not match, but the element has an end tag that can be
-- omitted when there is no more content in its parent.  This is an
-- ImplicitLast.  In this case, we need to remember the tag name of the
-- end tag that we did find, so as to match it later.
--
-- (3) A start tag is found such that it implicitly ends the current element.
-- This is an ImplicitNext.  In this case, we parse and remember the
-- entire element that comes next, so that it can be inserted after the
-- element being parsed.
data ElemResult = Matched
                | ImplicitLast Text
                | ImplicitNext Text Text [(Text, Text)] Bool


------------------------------------------------------------------------------
finishElement :: Text -> Text -> [(Text, Text)] -> Bool
              -> Parser (Node, ElemResult)
finishElement t tbase a b = do
    if b then return (Element t a [], Matched)
         else nonEmptyElem
  where
    nonEmptyElem
        | tbase `S.member` rawTextTags = do
            c <- XML.cdata  "<"  $ P.try (endTag t)
            return (Element t a [c], Matched)
        | tbase `S.member` endOmittableLast = tagContents optional
        | otherwise = tagContents (fmap Just)
    tagContents modifier = do
        (c,r1) <- content (Just tbase)
        case r1 of
            Matched -> do
                r2 <- modifier (endTag t)
                case r2 of
                    Nothing -> return (Element t a c, Matched)
                    Just rr -> return (Element t a c, rr)
            ImplicitLast tag | T.toCaseFold tag == T.toCaseFold t -> do
                return (Element t a c, Matched)
            end -> do
                return (Element t a c, end)


------------------------------------------------------------------------------
emptyOrStartTag :: Parser (Text, Text, [(Text, Text)], Bool)
emptyOrStartTag = do
    t <- P.try $ P.char '<' *> XML.name
    let tbase = T.toLower $ snd $ T.breakOnEnd ":" t
    a <- many $ P.try $ do
        XML.whiteSpace
        attribute
    when (hasDups a) $ fail "Duplicate attribute names in element"
    _ <- optional XML.whiteSpace
    e <- fmap isJust $ optional (P.char '/')
    let e' = e || (tbase `S.member` voidTags)
    _ <- P.char '>'
    return (t, tbase, a, e')
  where
    hasDups a = length (nub (map fst a)) < length a


------------------------------------------------------------------------------
attrName :: Parser Text
attrName = takeWhile1 isAttrName
  where isAttrName c | c `elem` "\0 \"\'>/=" = False
                     | isControlChar c       = False
                     | otherwise             = True


------------------------------------------------------------------------------
-- | From 8.2.2.3 of the HTML 5 spec, omitting the very high control
-- characters because they are unlikely to occur and I got tired of typing.
isControlChar :: Char -> Bool
isControlChar c | c >= '\x007F' && c <= '\x009F' = True
                | c >= '\xFDD0' && c <= '\xFDEF' = True
                | otherwise                      = False


------------------------------------------------------------------------------
quotedAttrValue :: Parser Text
quotedAttrValue = singleQuoted <|> doubleQuoted
  where
    singleQuoted = P.char '\'' *> refTill "&\'" <* P.char '\''
    doubleQuoted = P.char '\"' *> refTill "&\"" <* P.char '\"'
    refTill end = T.concat <$> many
        (takeWhile1 (not . (`elem` end)) <|> reference)


------------------------------------------------------------------------------
unquotedAttrValue :: Parser Text
unquotedAttrValue = refTill " \"\'=<>&`"
  where
    refTill end = T.concat <$> some
        (takeWhile1 (not . (`elem` end)) <|> reference)


------------------------------------------------------------------------------
attrValue :: Parser Text
attrValue = quotedAttrValue <|> unquotedAttrValue


------------------------------------------------------------------------------
attribute :: Parser (Text, Text)
attribute = do
    n <- attrName
    _ <- optional XML.whiteSpace
    v <- optional $ do
        _ <- P.char '='
        _ <- optional XML.whiteSpace
        attrValue
    return $ maybe (n,"") (n,) v


------------------------------------------------------------------------------
endTag :: Text -> Parser ElemResult
endTag s = do
    _ <- text "</"
    t <- XML.name
    let sbase = T.toLower $ snd $ T.breakOnEnd ":" s
    r <- if (T.toCaseFold s == T.toCaseFold t)
            then return Matched
            else if sbase `S.member` endOmittableLast
                then return (ImplicitLast t)
                else fail $ "mismatched tags: </" ++ T.unpack t ++
                            "> found inside <" ++ T.unpack s ++ "> tag"
    _ <- optional XML.whiteSpace
    _ <- text ">"
    return r


------------------------------------------------------------------------------
content :: Maybe Text -> Parser ([Node], ElemResult)
content parent = do
    (ns, end) <- readText
    return (coalesceText (catMaybes ns), end)
  where
    readText     = do
        s <- optional XML.charData
        t <- optional whileMatched
        case t of
            Nothing      -> return ([s], Matched)
            Just (tt, m) -> return (s:tt, m)

    whileMatched = do
        (n,end) <- (,Matched) <$> (:[]) <$> Just <$> TextNode <$> reference
               <|> (,Matched) <$> (:[]) <$> XML.cdSect
               <|> (,Matched) <$> (:[]) <$> XML.processingInstruction
               <|> (,Matched) <$> (:[]) <$> XML.comment
               <|> doElement
        case end of
            Matched -> do
                (ns, end') <- readText
                return (n ++ ns, end')
            _ -> do
                return (n, end)

    doElement = do
        (t,tb, a,b) <- emptyOrStartTag
        handle t tb a b

    handle t tb a b = do
        if breaksTag tb parent
            then return ([Nothing], ImplicitNext t tb a b)
            else do
                (n,end) <- finishElement t tb a b
                case end of
                    ImplicitNext t' tb' a' b' -> do
                        (ns,end') <- handle t' tb' a' b'
                        return (Just n : ns, end')
                    _ -> return ([Just n], end)

    breaksTag _     Nothing       = False
    breaksTag child (Just tag) = case M.lookup tag endOmittableNext of
        Nothing -> False
        Just s  -> S.member child s

    coalesceText (TextNode s : TextNode t : ns)
        = coalesceText (TextNode (T.append s t) : ns)
    coalesceText (n:ns)
        = n : coalesceText ns
    coalesceText []
        = []


------------------------------------------------------------------------------
reference :: Parser Text
reference = do
    _    <- P.char '&'
    r    <- (Left  <$> P.try finishCharRef)
        <|> (Right <$> P.try finishEntityRef)
        <|> (Left  <$> return '&')
    case r of
        Left c   -> do
            when (not (isValidChar c)) $ fail $
                "Reference is not a valid character"
            return (T.singleton c)
        Right nm -> case M.lookup nm predefinedRefs of
            Nothing -> fail $ "Unknown entity reference: " ++ T.unpack nm
            Just t  -> return t


------------------------------------------------------------------------------
finishCharRef :: Parser Char
finishCharRef = P.char '#' *> (hexCharRef <|> decCharRef)
  where
    decCharRef = do
        ds <- some digit
        _ <- P.char ';'
        let c = chr $ foldl' (\a b -> 10 * a + b) 0 ds
        return c
      where
        digit = do
            d <- P.satisfy (\c -> c >= '0' && c <= '9')
            return (ord d - ord '0')
    hexCharRef = do
        _ <- P.char 'x' <|> P.char 'X'
        ds <- some digit
        _ <- P.char ';'
        let c = chr $ foldl' (\a b -> 16 * a + b) 0 ds
        return c
      where
        digit = num <|> upper <|> lower
        num = do
            d <- P.satisfy (\c -> c >= '0' && c <= '9')
            return (ord d - ord '0')
        upper = do
            d <- P.satisfy (\c -> c >= 'A' && c <= 'F')
            return (10 + ord d - ord 'A')
        lower = do
            d <- P.satisfy (\c -> c >= 'a' && c <= 'f')
            return (10 + ord d - ord 'a')


------------------------------------------------------------------------------
finishEntityRef :: Parser Text
finishEntityRef = XML.name <* P.char ';'

