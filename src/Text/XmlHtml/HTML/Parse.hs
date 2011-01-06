{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Text.XmlHtml.HTML.Parse where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Text.XmlHtml.Common
import Text.XmlHtml.HTML.Meta

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T


------------------------------------------------------------------------------
-- | Get an initial guess at document encoding from the byte order mark.  If
-- the mark doesn't exist, guess UTF-8.  Otherwise, guess according to the
-- mark.
guessEncoding :: ByteString -> (Encoding, ByteString)
guessEncoding b
    | B.take 3 b == B.pack [ 0xEF, 0xBB, 0xBF ] = (UTF8,    B.drop 3 b)
    | B.take 2 b == B.pack [ 0xFE, 0xFF ]       = (UTF16BE, B.drop 2 b)
    | B.take 2 b == B.pack [ 0xFF, 0xFE ]       = (UTF16LE, B.drop 2 b)
    | otherwise                                 = (UTF8,    b)


------------------------------------------------------------------------------
parse :: ByteString -> Either String Document
parse b = let (e, b') = guessEncoding b
          in  handleResult e (parseText docFragment (decoder e b'))
  where
    handleResult _ (Left err)       = Left err
    handleResult e (Right (dt, ns)) = Right (HtmlDocument e dt ns)


------------------------------------------------------------------------------
parseText :: Parser a -> Text -> Either String a
parseText p t = case (P.parse parser t) of
    P.Fail _ _ err -> Left err
    P.Partial f    -> case (f T.empty) of
                        P.Fail _ _ err -> Left err
                        P.Done "" r    -> Right r
                        P.Done _  _    -> Left "Unexpected text after input"
                        P.Partial _    -> Left "Misbehaving parser"
    P.Done _ _     -> Left "Unexpected text after input"
  where parser = p <* P.endOfInput

------------------------------------------------------------------------------
-- | This is my best guess as to the best rule for handling document fragments
-- for processing.  It is essentially modeled after document, but allowing
-- multiple nodes.
docFragment :: Parser (Maybe DocType, [Node])
docFragment = do
    (dt, nodes1)      <- prolog
    (nodes2, Matched) <- content Nothing
    return (dt, nodes1 ++ nodes2)

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

    3. HTML RCDATA tags (textarea and title) are parsed as text and references
       but no other markup, except that they end at the first syntactically
       valid matching end tag.

    4. End tags need only match their corresponding start tags in a case
       insensitive comparison.  In case they are different, the start tag is
       used for the element tag name.

    5. Hexadecimal char references may use &#X...; (capital X)  -- DONE

    6. Attribute names are allowed to consist of any text except for control
       characters, space, '\"', '\'', '>', '/', or '='.

    7. Empty attribute syntax is allowed (an attribute not followed by an eq).
       In this case, the attribute value is considered to be the empty string.

    8. Quoted attribute syntax is relaxed to allow any character except for the
       matching quote.  References are allowed.

    9. Attribute values may be unquoted.  In this case, the attribute value may
       not contain space, single or double quotes, '=', '<', '>', or '`', and
       may not be the empty string.  It can still contain references.

    10. There are many more character references available.

    11. Omittable end tags are inserted automatically.
-}


------------------------------------------------------------------------------
document :: Parser (Maybe DocType, [Node])
document = do
    (dt, nodes1)    <- prolog
    (root, Matched) <- element
    nodes2          <- fmap catMaybes $ many misc
    return (dt, nodes1 ++ [ root ] ++ nodes2)


------------------------------------------------------------------------------
whiteSpace :: Parser ()
whiteSpace = some (P.satisfy (`elem` " \t\r\n")) *> return ()


------------------------------------------------------------------------------
isNameStartChar :: Char -> Bool
isNameStartChar c | c == ':'                         = True
                  | c == '_'                         = True
                  | c >= 'a'       && c <= 'z'       = True
                  | c >= 'A'       && c <= 'Z'       = True
                  | c >= '\xc0'    && c <= '\xd6'    = True
                  | c >= '\xd8'    && c <= '\xf6'    = True
                  | c >= '\xf8'    && c <= '\x2ff'   = True
                  | c >= '\x370'   && c <= '\x37d'   = True
                  | c >= '\x37f'   && c <= '\x1fff'  = True
                  | c >= '\x200c'  && c <= '\x200d'  = True
                  | c >= '\x2070'  && c <= '\x218f'  = True
                  | c >= '\x2c00'  && c <= '\x2fef'  = True
                  | c >= '\x3001'  && c <= '\xd7ff'  = True
                  | c >= '\xf900'  && c <= '\xfdcf'  = True
                  | c >= '\xfdf0'  && c <= '\xfffd'  = True
                  | c >= '\x10000' && c <= '\xeffff' = True
                  | otherwise                        = False


------------------------------------------------------------------------------
isNameChar :: Char -> Bool
isNameChar c | isNameStartChar c                = True
             | c == '-'                         = True
             | c == '.'                         = True
             | c == '\xb7'                      = True
             | c >= '0'       && c <= '9'       = True
             | c >= '\x300'   && c <= '\x36f'   = True
             | c >= '\x203f'  && c <= '\x2040'  = True
             | otherwise                        = False


------------------------------------------------------------------------------
name :: Parser Text
name = do
    c <- P.satisfy isNameStartChar
    r <- P.takeWhile isNameChar
    return $ T.cons c r


------------------------------------------------------------------------------
names :: Parser [Text]
names = P.sepBy1 name (P.char ' ')


------------------------------------------------------------------------------
nmtoken :: Parser Text
nmtoken = P.takeWhile1 isNameChar


------------------------------------------------------------------------------
nmtokens :: Parser [Text]
nmtokens = P.sepBy1 nmtoken (P.char ' ')


------------------------------------------------------------------------------
systemLiteral :: Parser Text
systemLiteral = singleQuoted <|> doubleQuoted
  where
    singleQuoted = do
        _ <- P.char '\''
        x <- P.takeWhile (not . (== '\''))
        _ <- P.char '\''
        return x
    doubleQuoted = do
        _ <- P.char '\"'
        x <- P.takeWhile (not . (== '\"'))
        _ <- P.char '\"'
        return x


------------------------------------------------------------------------------
pubIdLiteral :: Parser Text
pubIdLiteral = singleQuoted <|> doubleQuoted
  where
    singleQuoted = do
        _ <- P.char '\''
        x <- P.takeWhile (\c -> isPubIdChar c && c /= '\'')
        _ <- P.char '\''
        return x
    doubleQuoted = do
        _ <- P.char '\"'
        x <- P.takeWhile isPubIdChar
        _ <- P.char '\"'
        return x


------------------------------------------------------------------------------
isPubIdChar :: Char -> Bool
isPubIdChar c | c >= 'a' && c <= 'z'                 = True
              | c >= 'A' && c <= 'Z'                 = True
              | c >= '0' && c <= '9'                 = True
              | c `elem` " \r\n-\'()+,./:=?;!*#@$_%" = True
              | otherwise                            = False

------------------------------------------------------------------------------
-- | XXX: This does not comply with the rule prohibiting ]]> in character data.
-- The purpose of that rule is unclear to me, and it seems possible we don't
-- want to enforce it in the name of being accepting of more input.  Also,
-- it's unclear to me how to express it in parser combinators.    
charData :: Parser Node
charData = TextNode <$> P.takeWhile1 (not . (`elem` "<&"))


------------------------------------------------------------------------------
comment :: Parser Node
comment = P.string "<!--" *> (Comment <$> commentText) <* P.string "-->"
  where
    commentText = fmap T.concat $ many $
        nonDash <|> P.try (T.cons <$> P.char '-' <*> nonDash)
    nonDash = P.takeWhile1 (not . (== '-'))

------------------------------------------------------------------------------
-- | Always returns Nothing since there's no representation for a PI in the
-- document tree.
processingInstruction :: Parser (Maybe Node)
processingInstruction =
    P.string "<?" *> piTarget *> whiteSpace
                  *> P.manyTill P.anyChar (P.string "?>")
                  *> return Nothing


------------------------------------------------------------------------------
piTarget :: Parser Text
piTarget = do
    n <- name
    when (T.map toLower n == "xml") $ fail "xml declaration can't occur here"
    return n


------------------------------------------------------------------------------
cdata :: [Char] -> Parser a -> Parser Node
cdata cs end = TextNode <$> T.concat <$> P.manyTill part end
  where part = P.takeWhile1 (not . (`elem` cs))
             <|> T.singleton <$> P.anyChar

------------------------------------------------------------------------------
-- | Reads text and references, up until the passed-in parser succeeds.
-- (Generally, the passed in parser should be an end tag parser for the
-- RCDATA element.)
rcdata :: [Char] -> Parser a -> Parser Node
rcdata cs end = TextNode <$> T.concat <$> P.manyTill part end
  where part = P.takeWhile1 (not . (`elem` cs))
             <|> reference
             <|> T.singleton <$> P.anyChar


------------------------------------------------------------------------------
cdSect :: Parser Node
cdSect = P.string "<![CDATA[" *> cdata "]" (P.string "]]>")


------------------------------------------------------------------------------
prolog :: Parser (Maybe DocType, [Node])
prolog = do
    _      <- optional xmlDecl
    nodes1 <- many misc
    rest   <- optional $ do
        dt     <- docType
        nodes2 <- many misc
        return (dt, nodes2)
    case rest of
        Nothing           -> return (Nothing, catMaybes nodes1)
        Just (dt, nodes2) -> return (Just dt, catMaybes (nodes1 ++ nodes2))

------------------------------------------------------------------------------
-- | Return value is the encoding, if present.
xmlDecl :: Parser (Text, Maybe Text)
xmlDecl = do
    _ <- P.string "<?xml"
    v <- versionInfo
    e <- optional encodingDecl
    _ <- optional sdDecl
    _ <- optional whiteSpace
    _ <- P.string "?>"
    return (v,e)


------------------------------------------------------------------------------
versionInfo :: Parser Text
versionInfo = do
    whiteSpace *> P.string "version" *> eq *> (singleQuoted <|> doubleQuoted)
  where
    singleQuoted = P.char '\'' *> versionNum <* P.char '\''
    doubleQuoted = P.char '\"' *> versionNum <* P.char '\"'
    versionNum   = do
        a <- P.string "1."
        b <- fmap T.pack $ some (P.satisfy (\c -> c >= '0' && c <= '9'))
        return (T.append a b)


------------------------------------------------------------------------------
eq :: Parser ()
eq = optional whiteSpace *> P.char '=' *> optional whiteSpace *> return ()


------------------------------------------------------------------------------
misc :: Parser (Maybe Node)
misc = Just <$> comment
       <|> processingInstruction
       <|> (whiteSpace *> return Nothing)

------------------------------------------------------------------------------
-- | Internal subset is parsed, but ignored since we don't have data types to
-- store it.
docType :: Parser DocType
docType = do
    _     <- P.string "<!DOCTYPE"
    whiteSpace
    tag   <- name
    extid <- optional $ whiteSpace *> externalID
    _     <- optional whiteSpace
    _     <- optional $ do
            _ <- P.char '['
            {- XXX: Should scan for end, anyway -}
            fail "embedded DTD subsets not allowed"
    _     <- P.char '>'
    return (DocType tag extid)


------------------------------------------------------------------------------
sdDecl :: Parser ()
sdDecl = do
    whiteSpace
    _ <- P.string "standalone"
    eq
    _ <- single <|> double
    return ()
  where
    single = P.char '\'' *> yesno <* P.char '\''
    double = P.char '\"' *> yesno <* P.char '\"'
    yesno  = P.string "yes" <|> P.string "no"

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
                | ImplicitNext Text [(Text, Text)] Bool


------------------------------------------------------------------------------
element :: Parser (Node, ElemResult)
element = do
    (t,a,b) <- emptyOrStartTag
    finishElement t a b


------------------------------------------------------------------------------
finishElement :: Text -> [(Text, Text)] -> Bool -> Parser (Node, ElemResult)
finishElement t a b = do
    if b then return (Element t a [], Matched)
         else nonEmptyElem
  where
    nonEmptyElem
        | T.map toLower t `S.member` rawTextTags = do
            c <- cdata  "<"  $ P.try (endTag t)
            return (Element t a [c], Matched)
        | T.map toLower t `S.member` rcdataTags = do
            c <- rcdata "&<" $ P.try (endTag t)
            return (Element t a [c], Matched)
        | T.map toLower t `S.member` endOmittableLast = tagContents optional
        | otherwise                                   = tagContents (fmap Just)
    tagContents modifier = do
        (c,r1) <- content (Just t)
        case r1 of
            Matched -> do
                r2 <- modifier (endTag t)
                case r2 of
                    Nothing -> return (Element t a c, Matched)
                    Just rr -> return (Element t a c, rr)
            ImplicitLast tag | T.map toLower tag == T.map toLower t -> do
                return (Element t a c, Matched)
            end -> do
                return (Element t a c, end)


------------------------------------------------------------------------------
emptyOrStartTag :: Parser (Text, [(Text, Text)], Bool)
emptyOrStartTag = do
    _ <- P.char '<'
    t <- name
    a <- many $ do
        whiteSpace
        attribute
    _ <- optional whiteSpace
    e <- fmap isJust $ optional (P.char '/')
    let e' = e || (T.map toLower t `S.member` voidTags)
    _ <- P.char '>'
    return (t, a, e')


------------------------------------------------------------------------------
attrName :: Parser Text
attrName = P.takeWhile1 isAttrName
  where isAttrName c | c `elem` "\0 \"\'>/=" = False
                     | isControlChar c       = False
                     | otherwise             = True


------------------------------------------------------------------------------
-- | From 8.2.2.3 of the spec, omitting the very high control characters
-- because they are unlikely to occur and I got tired of typing.
isControlChar :: Char -> Bool
isControlChar c | c >= '\x0001' && c <= '\x0008' = True
                | c >= '\x000E' && c <= '\x001F' = True
                | c >= '\x007F' && c <= '\x009F' = True
                | c >= '\xFDD0' && c <= '\xFDEF' = True
                | otherwise                      = False


------------------------------------------------------------------------------
quotedAttrValue :: Parser Text
quotedAttrValue = singleQuoted <|> doubleQuoted
  where
    singleQuoted = P.char '\'' *> refTill "&\'" <* P.char '\''
    doubleQuoted = P.char '\"' *> refTill "&\"" <* P.char '\"'
    refTill end = T.concat <$> many
        (P.takeWhile1 (not . (`elem` end))
         <|> P.try reference
         <|> T.singleton <$> P.char '&')


------------------------------------------------------------------------------
unquotedAttrValue :: Parser Text
unquotedAttrValue = refTill " \"\'=<>&`"
  where
    refTill end = T.concat <$> some
        (P.takeWhile1 (not . (`elem` end))
         <|> P.try reference
         <|> T.singleton <$> P.char '&')


------------------------------------------------------------------------------
attrValue :: Parser Text
attrValue = quotedAttrValue <|> unquotedAttrValue


------------------------------------------------------------------------------
attribute :: Parser (Text, Text)
attribute = do
    n <- attrName
    _ <- optional whiteSpace
    v <- optional $ do
        _ <- P.char '='
        _ <- optional whiteSpace
        attrValue
    return $ maybe (n,"") (n,) v


------------------------------------------------------------------------------
endTag :: Text -> Parser ElemResult
endTag s = do
    _ <- P.string "</"
    t <- name
    r <- if (T.map toLower s == T.map toLower t)
            then return Matched
            else if T.map toLower s `S.member` endOmittableLast
                then return (ImplicitLast t)
                else fail $ "mismatched tags: </" ++ T.unpack t ++
                            "> found inside <" ++ T.unpack s ++ "> tag"
    _ <- optional whiteSpace
    _ <- P.string ">"
    return r


------------------------------------------------------------------------------
content :: Maybe Text -> Parser ([Node], ElemResult)
content parent = do
    (ns, end) <- readText
    return (coalesceText (catMaybes ns), end)
  where
    readText     = do
        s <- optional charData
        t <- optional whileMatched
        case t of
            Nothing      -> return ([s], Matched)
            Just (tt, m) -> return (s:tt, m)

    whileMatched = do
        (n,end) <- (,Matched) <$> (:[]) <$> Just <$> TextNode <$> reference
               <|> (,Matched) <$> (:[]) <$> Just <$> cdSect
               <|> (,Matched) <$> (:[]) <$> processingInstruction
               <|> (,Matched) <$> (:[]) <$> Just <$> comment
               <|> doElement
        case end of
            Matched -> do
                (ns, end') <- readText
                return (n ++ ns, end')
            _ -> do
                return (n, end)

    doElement = do
        (t,a,b) <- emptyOrStartTag
        handle t a b

    handle t a b = do
        if breaksTag t parent
            then return ([Nothing], ImplicitNext t a b)
            else do
                (n,end) <- finishElement t a b
                case end of
                    ImplicitNext t' a' b' -> do
                        (ns,end') <- handle t' a' b'
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
charRef :: Parser Text
charRef = hexCharRef <|> decCharRef
  where
    decCharRef = do
        _ <- P.string "&#"
        ds <- some digit
        _ <- P.char ';'
        return $ T.singleton $ chr $ foldl' (\a b -> 10 * a + b) 0 ds
      where
        digit = do
            d <- P.satisfy (\c -> c >= '0' && c <= '9')
            return (ord d - ord '0')
    hexCharRef = do
        _ <- P.string "&#x" <|> P.string "&#X"
        ds <- some digit
        _ <- P.char ';'
        return $ T.singleton $ chr $ foldl' (\a b -> 16 * a + b) 0 ds
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
reference :: Parser Text
reference = charRef <|> entityRef


------------------------------------------------------------------------------
entityRef :: Parser Text
entityRef = do
    _ <- P.char '&'
    n <- name
    _ <- P.char ';'
    case M.lookup n predefinedRefs of
        Nothing -> fail $ "Unknown entity reference: " ++ T.unpack n
        Just t  -> return t


------------------------------------------------------------------------------
externalID :: Parser ExternalID
externalID = systemID <|> publicID
  where
    systemID = do
        _ <- P.string "SYSTEM"
        whiteSpace
        fmap System systemLiteral
    publicID = do
        _ <- P.string "PUBLIC"
        whiteSpace
        pid <- pubIdLiteral
        whiteSpace
        sid <- systemLiteral
        return (Public pid sid)

------------------------------------------------------------------------------
-- | Return value is the encoding.
textDecl :: Parser (Maybe Text, Text)
textDecl = do
    _ <- P.string "<?xml"
    v <- optional versionInfo
    e <- encodingDecl
    _ <- optional whiteSpace
    _ <- P.string "?>"
    return (v,e)


------------------------------------------------------------------------------
extParsedEnt :: Parser [Node]
extParsedEnt = do
    (ns, Matched) <- optional textDecl *> content Nothing
    return ns


------------------------------------------------------------------------------
encodingDecl :: Parser Text
encodingDecl = whiteSpace *> P.string "encoding" *> eq
            *> (singleQuoted <|> doubleQuoted)
  where
    singleQuoted = P.char '\'' *> encName <* P.char '\''
    doubleQuoted = P.char '\"' *> encName <* P.char '\"'
    encName      = do
        c  <- P.satisfy isEncStart
        cs <- P.takeWhile isEnc
        return (T.cons c cs)
    isEncStart c | c >= 'A' && c <= 'Z' = True
                 | c >= 'a' && c <= 'z' = True
                 | otherwise = False
    isEnc      c | c >= 'A' && c <= 'Z' = True
                 | c >= 'a' && c <= 'z' = True
                 | c >= '0' && c <= '9' = True
                 | c `elem` "._-"       = True
                 | otherwise = False

