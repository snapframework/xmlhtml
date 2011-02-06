{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.XmlHtml.TextParser where

import           Control.Applicative
import           Data.Char
import           Data.Maybe
import           Text.XmlHtml.Common

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec (Parsec)
import qualified Text.Parsec as P

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B


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
-- | Specialized type for the parsers we use here.
type Parser = Parsec Text ()


------------------------------------------------------------------------------
-- An (orphaned) instance for parsing Text with Parsec.
instance (Monad m) => P.Stream T.Text m Char where
    uncons = return . T.uncons


------------------------------------------------------------------------------
parse :: (Encoding -> Parser a) -> String -> ByteString -> Either String a
parse p src b = let (e, b') = guessEncoding b
                    t       = decoder e b'
                    bad     = T.find (not . isValidChar) t
                in  if isNothing bad
                        then parseText (p e <* P.eof) src t
                        else Left $ "Document contains invalid character:"
                                 ++ " \\" ++ show (ord (fromJust bad))


------------------------------------------------------------------------------
-- | Checks if a document contains invalid characters.
--
isValidChar :: Char -> Bool
isValidChar c | c < '\x9'                     = False
              | c > '\xA'    && c < '\xD'     = False
              | c > '\xD'    && c < '\x20'    = False
              | c > '\xD7FF' && c < '\xE000'  = False
              | c > '\xFFFD' && c < '\x10000' = False
              | otherwise                     = True


------------------------------------------------------------------------------
-- | Parses a 'Text' value and gives back the result.  The parser is expected
-- to match the entire string.
parseText :: Parser a         -- ^ The parser to match
          -> String           -- ^ Name of the source file (can be @\"\"@)
          -> Text             -- ^ Text to parse
          -> Either String a  -- Either an error message or the result
parseText p src t = inLeft show (P.parse p src t)
  where inLeft :: (a -> b) -> Either a c -> Either b c
        inLeft f (Left x)  = Left (f x)
        inLeft _ (Right x) = Right x


------------------------------------------------------------------------------
-- | Consume input as long as the predicate returns 'True', and return the
-- consumed input.  This parser does not fail.  If it matches no input, it
-- will return an empty string.
takeWhile0 :: (Char -> Bool) -> Parser Text
takeWhile0 p = fmap T.pack $ P.many $ P.satisfy p


------------------------------------------------------------------------------
-- | Consume input as long as the predicate returns 'True', and return the
-- consumed input.  This parser requires the predicate to succeed on at least
-- one character of input.  It will fail if the first character fails the
-- predicate.
takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = fmap T.pack $ P.many1 $ P.satisfy p


------------------------------------------------------------------------------
-- | The equivalent of Parsec's string combinator, but for text.  If there is
-- not a complete match, then no input is consumed.  This matches the behavior
-- of @string@ from the attoparsec-text package.
text :: Text -> Parser Text
text t = P.try $ P.string (T.unpack t) *> return t


------------------------------------------------------------------------------
-- | Represents the state of a text scanner, for use with the 'scanText'
-- parser combinator.
data ScanState = ScanNext (Char -> ScanState)
               | ScanFinish
               | ScanFail String


------------------------------------------------------------------------------
-- | Scans text and progresses through a DFA, collecting the complete matching
-- text as it goes.
scanText :: (Char -> ScanState) -> Parser String
scanText f = do
    P.try $ do
        c <- P.anyChar
        case f c of
            ScanNext f'  -> (c:) `fmap` scanText f'
            ScanFinish   -> return [c]
            ScanFail err -> fail err

