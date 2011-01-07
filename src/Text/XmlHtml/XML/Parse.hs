{-# LANGUAGE OverloadedStrings #-}

module Text.XmlHtml.XML.Parse where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Text.XmlHtml.Common

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P

import           Data.Map (Map)
import qualified Data.Map as M

import           Data.Text (Text)
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
    handleResult e (Right (dt, ns)) = Right (XmlDocument e dt ns)


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
    (dt, nodes1) <- prolog
    nodes2       <- content
    return (dt, nodes1 ++ nodes2)


-------------------------------------------------------------------------------
-- Everything from here forward is translated from the XML specification.    --
-------------------------------------------------------------------------------

{-
    Map from numbered productions in the XML specification to symbols here:

    PROD  SPEC NAME          PARSER NAME           NOTES
    -----|------------------|---------------------|-------
    [1]   document           document
    [2]   Char                                     {2}
    [3]   S                  whiteSpace
    [4]   NameStartChar      isNameStartChar       {1}
    [4a]  NameChar           isNameChar            {1}
    [5]   Name               name
    [6]   Names              names
    [7]   Nmtoken            nmtoken
    [8]   Nmtokens           nmtokens
    [9]   EntityValue                              {4}
    [10]  AttValue           attrValue
    [11]  SystemLiteral      systemLiteral
    [12]  PubidLiteral       pubIdLiteral
    [13]  PubidChar          isPubIdChar           {1}
    [14]  CharData           charData
    [15]  Comment            comment
    [16]  PI                 processingInstruction
    [17]  PITarget           piTarget
    [18]  CDSect             cdSect
    [19]  CDStart            cdSect                {3}
    [20]  CData              cdSect                {3}
    [21]  CDEnd              cdSect                {3}
    [22]  prolog             prolog
    [23]  XMLDecl            xmlDecl
    [24]  VersionInfo        versionInfo
    [25]  Eq                 eq
    [26]  VersionNum         versionInfo           {3}
    [27]  Misc               misc
    [28]  doctypedecl        docTypeDecl
    [28a] DeclSep                                  {4}
    [28b] intSubset                                {4}
    [29]  markupdecl                               {4}
    [30]  extSubset                                {4}
    [31]  extSubsetDecl                            {4}
    [32]  SDDecl             sdDecl
    [39]  element            element
    [40]  STag               emptyOrStartTag
    [41]  Attribute          attribute
    [42]  ETag               endTag
    [43]  content            content
    [44]  EmptyElemTag       emptyOrStartTag
    [45]  elementDecl                              {4}
    [46]  contentSpec                              {4}
    [47]  children                                 {4}
    [48]  cp                                       {4}
    [49]  choice                                   {4}
    [50]  seq                                      {4}
    [51]  Mixed                                    {4}
    [52]  AttlistDecl                              {4}
    [53]  AttDef                                   {4}
    [54]  AttType                                  {4}
    [55]  StringType                               {4}
    [56]  TokenizedType                            {4}
    [57]  EnumeratedType                           {4}
    [58]  NotationType                             {4}
    [59]  Enumeration                              {4}
    [60]  DefaultDecl                              {4}
    [61]  conditionalSect                          {4}
    [62]  includeSect                              {4}
    [63]  ignoreSect                               {4}
    [64]  ignoreSectContents                       {4}
    [65]  Ignore                                   {4}
    [66]  CharRef            charRef
    [67]  Reference          reference
    [68]  EntityRef          entityRef
    [69]  PEReference                              {4}
    [70]  EntityDecl                               {4}
    [71]  GEDecl                                   {4}
    [72]  PEDecl                                   {4}
    [73]  EntityDef                                {4}
    [74]  PEDef                                    {4}
    [75]  ExternalID         externalID
    [76]  NDataDecl                                {4}
    [77]  TextDecl           textDecl
    [78]  extParsedEnt       extParsedEnt
    [80]  EncodingDecl       encodingDecl
    [81]  EncName            encodingDecl          {3}
    [82]  NotationDecl                             {4}
    [83]  PublicID                                 {4}
    [84]  Letter                                   {5}
    [85]  BaseChar                                 {5}
    [86]  Ideographic                              {5}
    [87]  CombiningChar                            {5}
    [88]  Digit                                    {5}
    [89]  Extender                                 {5}

    Notes:
        {1} - These productions match single characters, and so are implemented
              as predicates instead of parsers.
        {2} - As an extension, all valid Haskell characters can occur in a
              parsed file
        {3} - Denotes a production which is not exposed as a top-level symbol
              because it is trivial and included in another definition.
        {4} - This module does not contain a parser for the DTD subsets, so
              grammar that occurs only in DTD subsets is not defined.
        {5} - These are orphaned productions for character classes.
-}


------------------------------------------------------------------------------
document :: Parser (Maybe DocType, [Node])
document = do
    (dt, nodes1) <- prolog
    root         <- element
    nodes2       <- fmap catMaybes $ many misc
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
attrValue :: Parser Text
attrValue = fmap T.concat (singleQuoted <|> doubleQuoted)
  where
    singleQuoted = P.char '\'' *> refTill "<&\'" <* P.char '\''
    doubleQuoted = P.char '\"' *> refTill "<&\"" <* P.char '\"'
    refTill end = many (P.takeWhile1 (not . (`elem` end)) <|> reference)


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
-- | The requirement to not contain "]]>" is for SGML compatibility.  We
-- deliberately choose to not enforce it.  This makes the parser accept
-- strictly more documents than a standards-compliant parser.
charData :: Parser Node
charData = TextNode <$> P.takeWhile1 (not . (`elem` "<&"))


------------------------------------------------------------------------------
comment :: Parser (Maybe Node)
comment = P.string "<!--" *> (Just <$> Comment <$> commentText) <* P.string "-->"
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
cdSect :: Parser (Maybe Node)
cdSect = Just <$> do
    _ <- P.string "<![CDATA["
    cdata "]" (P.string "]]>")


------------------------------------------------------------------------------
prolog :: Parser (Maybe DocType, [Node])
prolog = do
    _      <- optional xmlDecl
    nodes1 <- many misc
    rest   <- optional $ do
        dt     <- docTypeDecl
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
misc = comment <|> processingInstruction <|> (whiteSpace *> return Nothing)


------------------------------------------------------------------------------
-- | Internal subset is parsed, but ignored since we don't have data types to
-- store it.
docTypeDecl :: Parser DocType
docTypeDecl = do
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
element :: Parser Node
element = do
    (t,a,b) <- emptyOrStartTag
    if b then return (Element t a [])
         else nonEmptyElem t a
  where
    nonEmptyElem t a = do
        c <- content
        endTag t
        return (Element t a c)


------------------------------------------------------------------------------
-- | Results are (tag name, attributes, isEmpty)
emptyOrStartTag :: Parser (Text, [(Text, Text)], Bool)
emptyOrStartTag = do
    _ <- P.char '<'
    t <- name
    a <- many $ do
        whiteSpace
        attribute
    _ <- optional whiteSpace
    e <- optional (P.char '/')
    _ <- P.char '>'
    return (t, a, isJust e)


------------------------------------------------------------------------------
attribute :: Parser (Text, Text)
attribute = do
    n <- name
    eq
    v <- attrValue
    return (n,v)


------------------------------------------------------------------------------
endTag :: Text -> Parser ()
endTag s = do
    _ <- P.string "</"
    t <- name
    when (s /= t) $ fail $ "mismatched tags: </" ++ T.unpack t ++
                           "> found inside <" ++ T.unpack s ++ "> tag"
    _ <- optional whiteSpace
    _ <- P.string ">"
    return ()


------------------------------------------------------------------------------
content :: Parser [Node]
content = do
    n  <- optional charData
    ns <- fmap concat $ many $ do
        s <- ((Just <$> TextNode <$> reference)
               <|> cdSect
               <|> processingInstruction
               <|> comment
               <|> fmap Just element)
        t <- optional charData
        return [s,t]
    return $ coalesceText $ catMaybes (n:ns)
  where
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
        _ <- P.string "&#x"
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
    case M.lookup n entityRefLookup of
        Nothing -> fail $ "Unknown entity reference: " ++ T.unpack n
        Just t  -> return t
  where
    entityRefLookup :: Map Text Text
    entityRefLookup = M.fromList [
        ("amp", "&"),
        ("lt", "<"),
        ("gt", ">"),
        ("apos", "\'"),
        ("quot", "\"")
        ]


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
extParsedEnt = optional textDecl *> content


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

