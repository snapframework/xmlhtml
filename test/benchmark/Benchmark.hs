{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.XmlHtml
import qualified Text.XmlHtml.HTML.Render as Render

------------------------------------------------------------------------------
main :: IO ()
main = defaultMain [ renderBenchmarks ]


------------------------------------------------------------------------------
renderBenchmarks :: Benchmark
renderBenchmarks = bgroup "render"
                       [ bench "escaped" escapedBench
--                       , bench "element" elementBench
                       ]

------------------------------------------------------------------------------
escapedCorpus :: Text
escapedCorpus = T.concat $ take 100 $
                cycle [ "the qui>ck & brown <fox> jump&gted&gt;over the brown "
                      , "f&o&xxx; & fox foo://bar.com/?bar=baz&quux=quux&...&&"
                      , "\r\n\t\t\t\r\n\t\t\t&&<><><><"
                      ]

escapedBench :: Pure
escapedBench = whnf test escapedCorpus
  where
    test txt = let !bld = Render.escaped "<>& \t\r\n" UTF8 txt
                   !out = toByteString bld
               in out

------------------------------------------------------------------------------

--elementBench = escapedBench
