{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.XmlHtml
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html (Html)
------------------------------------------------------------------------------
import           BlazeExample

------------------------------------------------------------------------------
main :: IO ()
main = do
    exampleHTML <- parseExample
    !_ <- return $! length $! show exampleHTML

    defaultMain [
         bench "renderHtml" $ renderHtmlBenchmark exampleHTML
       , bench "renderBlaze" $ renderBlazeBenchmark blazeHtmlExample
       ]


------------------------------------------------------------------------------
parseExample :: IO Document
parseExample = do
    bytes <- B.readFile "resources/benchmarks/haddock-example.html"
    either error return $ parseHTML "haddock-example.html" bytes


------------------------------------------------------------------------------
renderHtmlBenchmark :: Document -> Pure
renderHtmlBenchmark = whnf (toByteString . render)


------------------------------------------------------------------------------
renderBlazeBenchmark :: Html -> Pure
renderBlazeBenchmark = whnf (touch . renderHtml)
  where
    touch l = foldl' seq "" (L.toChunks l) `seq` ()
