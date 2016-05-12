{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Framework (defaultMain)
import           Text.XmlHtml.Tests (tests)

main :: IO ()
main = defaultMain tests
