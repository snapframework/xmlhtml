{-# LANGUAGE OverloadedStrings #-}

module Text.XmlHtml.OASISTest (testsOASIS) where

import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString as B
import           Data.Maybe
import qualified Data.Text as T
import           System.Directory
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, Node)
import           Text.XmlHtml

------------------------------------------------------------------------------
-- | Runs the OASIS XML conformance test suite.  The test cases sit in
-- directories inside of @test/resources@.  This test reads them, parses them,
-- and ensures that they behave as expected:
--
-- * If there is a file in the same directory named /filename/@.correct@
--   or /filename/@.incorrect@, the parsing is expected to either succeed
--   or fail, as indicated.  Otherwise, the remaining cases apply.
--
-- * For those tests marked @not-wf@, the test expects an error message.
--
-- * For tests marked @valid@ or @invalid@ (there is no distinction since
--   @xmlhtml@ is not a validating parser), the test expects a successful
--   parse, but does not verify the parse result.
--
-- For tests that should succeed as XML but not HTML, or vice versa, files
-- can be named /filename/@.xml.correct@, and so on (all 4 combinations).

testsOASIS :: [Test]
testsOASIS = [
    testCase "xmlhtml/ibm-not-wf     " $ oasisP  "ibm/ibm_oasis_not-wf.xml",
    testCase "xmlhtml/ibm-invalid    " $ oasisP  "ibm/ibm_oasis_invalid.xml",
    testCase "xmlhtml/ibm-valid      " $ oasisP  "ibm/ibm_oasis_valid.xml",
    testCase "xmlhtml/oasis          " $ oasisP  "oasis/oasis.xml",
    testCase "xmlhtml/r-ibm-not-wf   " $ oasisR  "ibm/ibm_oasis_not-wf.xml",
    testCase "xmlhtml/r-ibm-invalid  " $ oasisR  "ibm/ibm_oasis_invalid.xml",
    testCase "xmlhtml/r-ibm-valid    " $ oasisR  "ibm/ibm_oasis_valid.xml",
    testCase "xmlhtml/r-oasis        " $ oasisR  "oasis/oasis.xml",
    testCase "xmlhtml/h-ibm-not-wf   " $ oasisHP "ibm/ibm_oasis_not-wf.xml",
    testCase "xmlhtml/h-ibm-invalid  " $ oasisHP "ibm/ibm_oasis_invalid.xml",
    testCase "xmlhtml/h-ibm-valid    " $ oasisHP "ibm/ibm_oasis_valid.xml",
    testCase "xmlhtml/h-oasis        " $ oasisHP "oasis/oasis.xml",
    testCase "xmlhtml/hr-ibm-not-wf  " $ oasisHR "ibm/ibm_oasis_not-wf.xml",
    testCase "xmlhtml/hr-ibm-invalid " $ oasisHR "ibm/ibm_oasis_invalid.xml",
    testCase "xmlhtml/hr-ibm-valid   " $ oasisHR "ibm/ibm_oasis_valid.xml",
    testCase "xmlhtml/hr-oasis       " $ oasisHR "oasis/oasis.xml"
    ]


oasisP :: String -> Assertion
oasisP name = do
    tests <- getOASIS ".xml" name
    forM_ tests $ \(fn, ty) -> case ty of
        True  -> oasisWF fn
        False -> oasisNotWF fn


oasisR :: String -> Assertion
oasisR name = do
    tests <- getOASIS ".xml" name
    forM_ tests $ \(fn, ty) -> case ty of
        True  -> oasisRerender fn
        False -> return ()


oasisHP :: String -> Assertion
oasisHP name = do
    tests <- getOASIS ".html" name
    forM_ tests $ \(fn, ty) -> case ty of
        True  -> hOasisWF fn
        False -> hOasisNotWF fn


oasisHR :: String -> Assertion
oasisHR name = do
    tests <- getOASIS ".html" name
    forM_ tests $ \(fn, ty) -> case ty of
        True  -> hOasisRerender fn
        False -> return ()


getOASIS :: String -> String -> IO [(String, Bool)]
getOASIS sub name = do
    testListSrc <- B.readFile ("resources/" ++ name)
    let Right (XmlDocument _ _ ns) = parseXML name testListSrc
    let Just c = listToMaybe (filter isElement ns)
    oasisTestCases sub name c


oasisTestCases :: String -> String -> Node -> IO [(String, Bool)]
oasisTestCases sub name n = do
    fmap concat $ forM (childElements n) $ \t -> case tagName t of
        Just "TESTCASES" -> oasisTestCases sub name t
        Just "TEST"      -> oasisTest sub name t
        _                -> error (show t)


oasisTest :: String -> String -> Node -> IO [(String, Bool)]
oasisTest sub name t = do
    let fn = file $ fromJust $ getAttribute "URI" t
    fe <- doesFileExist fn
    ce <- (||) <$> doesFileExist (fn ++ ".correct")
               <*> doesFileExist (fn ++ sub ++ ".correct")
    ie <- (||) <$> doesFileExist (fn ++ ".incorrect")
               <*> doesFileExist (fn ++ sub ++ ".incorrect")
    let ty = getAttribute "TYPE" t
    if fe then case () of
        () | ce                   -> return [(fn, True)]
           | ie                   -> return [(fn, False)]
           | ty == Just "not-wf"  -> return [(fn, False)]
           | ty == Just "invalid" -> return [(fn, True)]
           | ty == Just "valid"   -> return [(fn, True)]
           | otherwise            -> return [(fn, True)]
      else return []
  where
    file f        = "resources/" ++ toLastSlash name ++ T.unpack f
    toLastSlash s = snd (go s)
        where go []     = (False, [])
              go (c:cs) = let (found, ccs) = go cs
                          in  if found then (True, c:ccs)
                              else if c == '/' then (True, "/")
                              else (False, c:cs)


oasisNotWF :: String -> Assertion
oasisNotWF name = do
    src <- B.readFile name
    assertBool ("not-wf parse " ++ name) $ isLeft (parseXML name src)
  where
    isLeft (Left _) = True
    isLeft _        = False


oasisWF :: String -> Assertion
oasisWF name = do
    src <- B.readFile name
    assertBool ("wf parse " ++ name) $ isRight (parseXML name src)
  where
    isRight (Right _) = True
    isRight _         = False


oasisRerender :: String -> Assertion
oasisRerender name = do
    src         <- B.readFile name
    let Right d  = parseXML "" src
    let src2     = toByteString (render d)
    let Right d2 = parseXML "" src2
    assertEqual ("rerender " ++ name) d d2


hOasisNotWF :: String -> Assertion
hOasisNotWF name = do
    src <- B.readFile name
    assertBool ("not-wf parse " ++ name) $ isLeft (parseHTML name src)
  where
    isLeft (Left _) = True
    isLeft _        = False


hOasisWF :: String -> Assertion
hOasisWF name = do
    src <- B.readFile name
    assertBool ("wf parse " ++ name) $ isRight (parseHTML name src)
  where
    isRight (Right _) = True
    isRight _         = False


hOasisRerender :: String -> Assertion
hOasisRerender name = do
    src         <- B.readFile name
    let Right d  = parseHTML "" src
    let src2     = toByteString (render d)
    let Right d2 = parseHTML "" src2
    assertEqual ("rerender " ++ name) d d2

