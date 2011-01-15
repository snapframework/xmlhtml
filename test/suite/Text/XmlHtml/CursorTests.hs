{-# LANGUAGE OverloadedStrings         #-}

module Text.XmlHtml.CursorTests (cursorTests) where

import           Data.Maybe
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, Node)
import           Text.XmlHtml
import           Text.XmlHtml.Cursor
import           Text.XmlHtml.TestCommon

------------------------------------------------------------------------------
-- Tests of navigating with the Cursor type ----------------------------------
------------------------------------------------------------------------------

cursorTests :: [Test]
cursorTests = [
    testIt   "fromNodeAndCurrent     " $ fromNodeAndCurrent,
    testIt   "fromNodesAndSiblings   " $ fromNodesAndSiblings,
    testIt   "leftSiblings           " $ leftSiblings,
    testIt   "emptyFromNodes         " $ emptyFromNodes,
    testCase "cursorNavigation       " $ cursorNavigation,
    testCase "cursorSearch           " $ cursorSearch
    ]

fromNodeAndCurrent :: Bool
fromNodeAndCurrent = all (\n -> n == current (fromNode n)) ns
    where ns = [
            TextNode "foo",
            Comment "bar",
            Element "foo" [] [],
            Element "root" [] [
                TextNode "foo",
                Comment "bar",
                Element "foo" [] []
                ]
            ]

fromNodesAndSiblings :: Bool
fromNodesAndSiblings = n == siblings (fromJust $ fromNodes n)
    where n = [
            TextNode "foo",
            Comment "bar",
            Element "foo" [] [],
            Element "root" [] [
                TextNode "foo",
                Comment "bar",
                Element "foo" [] []
                ]
            ]

leftSiblings :: Bool
leftSiblings = fromJust $ do
        r <- do
            c1 <- fromNodes n
            c2 <- right c1
            c3 <- right c2
            return c3
        return (n == siblings r)
    where n = [
            TextNode "foo",
            Comment "bar",
            Element "foo" [] [],
            Element "root" [] [
                TextNode "foo",
                Comment "bar",
                Element "foo" [] []
                ]
            ]

emptyFromNodes :: Bool
emptyFromNodes = isNothing (fromNodes [])

-- Sample node structure for running cursor tests.
cursorTestTree :: Node
cursorTestTree = Element "department" [("code", "A17")] [
    Element "employee" [("name", "bob")] [
        Comment "My best friend",
        Element "address" [] [
            TextNode "123 My Road"
            ],
        Element "temp" [] []
        ],
    Element "employee" [("name", "alice")] [
        Element "address" [] [
            TextNode "124 My Road"
            ],
        Element "phone" [] [
            TextNode "555-1234"
            ]
        ],
    Element "employee" [("name", "camille")] [
        Element "phone" [] [
            TextNode "800-888-8888"
            ]
        ]
    ]

cursorNavigation :: Assertion
cursorNavigation = do
    let r = fromNode cursorTestTree

    let Just e1 = firstChild r
    let Just e2 = getChild 1 r
    let Just e3 = lastChild r

    assertBool "rootElem" $ isElement (current r)
    assertBool "parent of root" $ isNothing (parent r)

    assertBool "getChild bounds" $ isNothing (getChild 3 r)
    assertBool "firstChild"      $
        getAttribute "name" (current e1) == Just "bob"
    assertBool "childAt 1 "      $
        getAttribute "name" (current e2) == Just "alice"
    assertBool "lastChild "      $
        getAttribute "name" (current e3) == Just "camille"

    do let Just a = lastChild e1
       assertBool "firstChild on empty element" $ isNothing (firstChild a)
       assertBool "getChild on empty element"   $ isNothing (getChild 0 a)
       assertBool "lastChild on empty element"  $ isNothing (lastChild a)

    do let Just a = right e1
       let Just b = right a
       assertBool "two paths #1" $ a == e2
       assertBool "two paths #2" $ b == e3
       assertBool "right off end" $ isNothing (right b)

       let Just c = left e3
       let Just d = left e2
       assertBool "two paths #3" $ c == e2
       assertBool "two paths #4" $ d == e1
       assertBool  "left off end" $ isNothing (left d)

    do let Just r1 = parent e2
       assertEqual "child -> parent" (current r) (current r1)

    do let Just cmt = firstChild e1
       assertBool  "topNode"  $ tagName (topNode cmt) == Just "department"
       assertBool  "topNodes" $ map tagName (topNodes cmt) == [ Just "department" ]

       assertBool "first child of comment" $ isNothing (firstChild cmt)
       assertBool "last  child of comment" $ isNothing (lastChild cmt)

    do assertBool "nextDF down" $ nextDF r == Just e1

       let Just cmt = firstChild e1
       assertBool "nextDF right" $ nextDF cmt == right cmt

       let Just em = lastChild e1
       assertBool "nextDF up-right" $ nextDF em == Just e2
       
       let Just pelem = lastChild e3
       let Just ptext = lastChild pelem
       assertBool "nextDF end" $ isNothing (nextDF ptext)


cursorSearch :: Assertion
cursorSearch = do
    let r = fromNode cursorTestTree

    let Just e1 = findChild isFirst r
    let Just e2 = findChild ((==(Just "alice")) . getAttribute "name" .  current) r
    let Just e3 = findChild isLast r

    assertBool "findChild" $
        getAttribute "name" (current e2) == Just "alice"
    assertBool "findChild isFirst" $
        getAttribute "name" (current e1) == Just "bob"
    assertBool "findChild isLast" $
        getAttribute "name" (current e3) == Just "camille"

    assertBool "findLeft Just" $ findLeft (const True) e2 == Just e1
    assertBool "findLeft Nothing" $ findLeft (const False) e2 == Nothing
    assertBool "findRight Just" $ findRight (const True) e2 == Just e3
    assertBool "findRight Nothing" $ findRight (const False) e2 == Nothing
    assertBool "findRec" $ findRec (not . hasChildren) r == (firstChild e1)

    assertBool "isChild true" $ isChild e1
    assertBool "isChild false" $ not $ isChild r
    assertBool "getNodeIndex" $ getNodeIndex e2 == 1

