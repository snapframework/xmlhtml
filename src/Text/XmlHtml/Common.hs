{-# LANGUAGE OverloadedStrings #-}

module Text.XmlHtml.Common where

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text.Encoding as T

data Document = XmlDocument  Encoding (Maybe DocType) [Node]
              | HtmlDocument Encoding (Maybe DocType) [Node]
    deriving (Eq, Show)

data Node = TextNode Text
          | Comment Text
          | Element Text [(Text, Text)] [Node]
    deriving (Eq, Show)

data DocType = DocType Text (Maybe ExternalID)
    deriving (Eq, Show)

data ExternalID = System Text
                | Public Text Text
    deriving (Eq, Show)

data Encoding = UTF8 | UTF16BE | UTF16LE deriving (Eq, Show)

encodingName :: Encoding -> Text
encodingName UTF8    = "UTF-8"
encodingName UTF16BE = "UTF-16"
encodingName UTF16LE = "UTF-16"

encoder :: Encoding -> Text -> ByteString
encoder UTF8    = T.encodeUtf8
encoder UTF16BE = T.encodeUtf16BE
encoder UTF16LE = T.encodeUtf16LE

decoder :: Encoding -> ByteString -> Text
decoder UTF8    = T.decodeUtf8
decoder UTF16BE = T.decodeUtf16BE
decoder UTF16LE = T.decodeUtf16LE

isUTF16 :: Encoding -> Bool
isUTF16 e = e == UTF16BE || e == UTF16LE

{-
manyWith :: Parser a -> Parser b -> Parser ([a], b)
manyWith p1 p2 = scan
  where scan     = P.try finish <|> continue
        finish   = do y      <- p2
                      return ([],y)
        continue = do x      <- p1
                      (xs,y) <- scan
                      return (x:xs, y)
-}
