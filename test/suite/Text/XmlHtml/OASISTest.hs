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


data Type = Result String | WF | NotWF


oasisP :: String -> Assertion
oasisP name = do
    tests <- getOASIS ".xml" name
    forM_ tests $ \(fn, ty) -> case ty of
        Result rn -> oasisResult fn rn
        WF        -> oasisWF fn
        NotWF     -> oasisNotWF fn


oasisR :: String -> Assertion
oasisR name = do
    tests <- getOASIS ".xml" name
    forM_ tests $ \(fn, ty) -> case ty of
        Result _ -> oasisRerender fn
        WF       -> oasisRerender fn
        _        -> return ()


oasisHP :: String -> Assertion
oasisHP name = do
    tests <- getOASIS ".html" name
    forM_ tests $ \(fn, ty) -> case ty of
        Result rn -> hOasisResult fn rn
        WF        -> hOasisWF fn
        NotWF     -> hOasisNotWF fn


oasisHR :: String -> Assertion
oasisHR name = do
    tests <- getOASIS ".html" name
    forM_ tests $ \(fn, ty) -> case ty of
        Result _ -> hOasisRerender fn
        WF       -> hOasisRerender fn
        _        -> return ()


getOASIS :: String -> String -> IO [(String, Type)]
getOASIS sub name = do
    testListSrc <- B.readFile ("resources/" ++ name)
    let Right (XmlDocument _ _ ns) = parseXML name testListSrc
    let Just c = listToMaybe (filter isElement ns)
    oasisTestCases sub name c

oasisTestCases :: String -> String -> Node -> IO [(String, Type)]
oasisTestCases sub name n = do
    fmap concat $ forM (childElements n) $ \t -> case tagName t of
        Just "TESTCASES" -> oasisTestCases sub name t
        Just "TEST"      -> oasisTest sub name t
        _                -> error (show t)

oasisTest :: String -> String -> Node -> IO [(String, Type)]
oasisTest sub name t = do
    let fn = file $ fromJust $ getAttribute "URI" t
    fe <- doesFileExist fn
    re <- doesFileExist (fn ++ sub ++ ".result")
    ce <- (||) <$> doesFileExist (fn ++ ".correct")
               <*> doesFileExist (fn ++ sub ++ ".correct")
    ie <- (||) <$> doesFileExist (fn ++ ".incorrect")
               <*> doesFileExist (fn ++ sub ++ ".incorrect")
    let ty = getAttribute "TYPE" t
    if fe then case () of
        () | re                   -> return [(fn, Result (fn ++ sub ++ ".result"))]
           | ce                   -> return [(fn, WF)]
           | ie                   -> return [(fn, NotWF)]
           | ty == Just "not-wf"  -> return [(fn, NotWF)]
           | ty == Just "invalid" -> return [(fn, WF)]
           | ty == Just "valid"   -> return [(fn, WF)]
           | otherwise            -> return [(fn, WF)]
      else return []
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


oasisRerender :: String -> Assertion
oasisRerender name = do
    src         <- B.readFile name
    let Right d  = parseXML "" src
    let src2     = toByteString (render d)
    let Right d2 = parseXML "" src2
    assertEqual ("rerender " ++ name) d d2


hOasisResult :: String -> String -> Assertion
hOasisResult name correctFN = do
    expected <- fmap read (readFile (correctFN ++ ".html"))
    src <- B.readFile name
    assertBool ("result parse " ++ name) $
        Right expected == parseHTML name src


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

