{-# LANGUAGE OverloadedStrings #-}

module Text.XmlHtml.OASISTest (testsOASIS) where

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
-- * If there is a file in the same directory named /filename/@.result@,
--   it is expected to be the result of 'Show' on a 'Document' value.  This
--   is expected to be the result of (successful) parsing.  Otherwise, the
--   remaining cases apply.
--
-- * For those tests marked @not-wf@, the test expects an error message.
--
-- * For tests marked @valid@ or @invalid@ (there is no distinction since
--   @xmlhtml@ is not a validating parser), the test expects a successful
--   parse, but does not verify the parse result.

testsOASIS :: [Test]
testsOASIS = [
    testCase "xmlhtml/ibm-not-wf " $ oasis "ibm/ibm_oasis_not-wf.xml",
    testCase "xmlhtml/ibm-invalid" $ oasis "ibm/ibm_oasis_invalid.xml",
    testCase "xmlhtml/ibm-valid  " $ oasis "ibm/ibm_oasis_valid.xml",
    testCase "xmlhtml/oasis      " $ oasis "oasis/oasis.xml",
    testCase "xmlhtml/sun-not-wf " $ oasis "sun/sun-not-wf.xml",
    testCase "xmlhtml/sun-invalid" $ oasis "sun/sun-invalid.xml",
    testCase "xmlhtml/sun-valid  " $ oasis "sun/sun-valid.xml",
    testCase "xmlhtml/xmltest    " $ oasis "xmltest/xmltest.xml"
    ]

oasis :: String -> Assertion
oasis name = do
    testListSrc <- B.readFile ("resources/" ++ name)
    let Right (XmlDocument _ _ ns) = parseXML name testListSrc
    let Just c = listToMaybe (filter isElement ns)
    oasisTestCases name c

oasisTestCases :: String -> Node -> Assertion
oasisTestCases name n = do
    forM_ (childElements n) $ \t -> case tagName t of
        Just "TESTCASES" -> oasisTestCases name t
        Just "TEST"      -> oasisTest name t
        _                -> error (show t)

oasisTest :: String -> Node -> Assertion
oasisTest name t = do
    let fn = file $ fromJust $ getAttribute "URI" t
    fe <- doesFileExist fn
    re <- doesFileExist (fn ++ ".result")
    ce <- doesFileExist (fn ++ ".correct")
    ie <- doesFileExist (fn ++ ".incorrect")
    let ty = getAttribute "TYPE" t
    when fe $ case () of
        () | re                   -> oasisResult fn (fn ++ ".result")
           | ce                   -> oasisWF fn
           | ie                   -> oasisNotWF fn
           | ty == Just "not-wf"  -> oasisNotWF fn
           | ty == Just "invalid" -> oasisWF fn
           | ty == Just "valid"   -> oasisWF fn
           | otherwise            -> oasisWF fn
  where
    file f        = "resources/" ++ toLastSlash name ++ T.unpack f
    toLastSlash s = snd (go s)
        where go []     = (False, [])
              go (c:cs) = let (found, ccs) = go cs
                          in  if found then (True, c:ccs)
                              else if c == '/' then (True, "/")
                              else (False, c:cs)


oasisResult :: String -> String -> Assertion
oasisResult name correctFN = do
    expected <- fmap read (readFile correctFN)
    src <- B.readFile name
    assertBool ("result parse " ++ name) $
        Right expected == parseXML name src


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

