{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec
import           Text.XmlHtml.CursorTests
import           Text.XmlHtml.DocumentTests
import           Text.XmlHtml.OASISTest
import           Text.XmlHtml.Tests

main :: IO ()
main = hspec $ do
    describe "xmlParsingTests" xmlParsingTests
    describe "htmlXMLParsingTests" htmlXMLParsingTests
    describe "htmlParsingQuirkTests" htmlParsingQuirkTests
    describe "xmlRenderingTests" xmlRenderingTests
    describe "htmlXMLRenderingTests" htmlXMLRenderingTests
    describe "htmlRenderingQuirkTests" htmlRenderingQuirkTests
    describe "documentTests" documentTests
    describe "cursorTests" cursorTests
    describe "blazeRenderTests" blazeRenderTests
    describe "testsOASIS" testsOASIS
