{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.XmlHtml

------------------------------------------------------------------------------
main :: IO ()
main = do
    exampleHTML <- parseExample
    !_ <- return $! length $! show exampleHTML

    defaultMain [
         bench "renderHtml" $ renderHtmlBenchmark exampleHTML
       ]


------------------------------------------------------------------------------
parseExample :: IO Document
parseExample = do
    bytes <- B.readFile "resources/benchmarks/haddock-example.html"
    either error return $ parseHTML "haddock-example.html" bytes


------------------------------------------------------------------------------
renderHtmlBenchmark :: Document -> Pure
renderHtmlBenchmark = whnf (toByteString . render)
