{-# LANGUAGE OverloadedStrings         #-}

module Text.XmlHtml.DocumentTests (documentTests) where

import           Data.Text ()                  -- for string instance
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Node, Test)
import           Text.XmlHtml
import           Text.XmlHtml.TestCommon


------------------------------------------------------------------------------
-- Tests of manipulating the Node tree and Document --------------------------
------------------------------------------------------------------------------

documentTests :: [Test]
documentTests = [
    -- Exercise the (/=) operators; (==) is done plenty of places.
    testIt   "compareExternalIDs     " $ compareExternalIDs,
    testIt   "compareInternalSubs    " $ compareInternalSubs,
    testIt   "compareDoctypes        " $ compareDoctypes,
    testIt   "compareNodes           " $ compareNodes,
    testIt   "compareDocuments       " $ compareDocuments,
    testIt   "compareEncodings       " $ compareEncodings,

    -- Silly tests just to exercise the Show instances on types.
    testCase "exerciseShows          " $ exerciseShows,

    -- Exercise the accessors for Document and Node
    testCase "docNodeAccessors       " $ docNodeAccessors,

    testIt   "isTextNodeYes          " $ isTextNode someTextNode,
    testIt   "isTextNodeNo           " $ not $ isTextNode someComment,
    testIt   "isTextNodeNo2          " $ not $ isTextNode someElement,
    testIt   "isCommentYes           " $ isComment someComment,
    testIt   "isCommentNo            " $ not $ isComment someTextNode,
    testIt   "isCommentNo2           " $ not $ isComment someElement,
    testIt   "isElementYes           " $ isElement someElement,
    testIt   "isElementNo            " $ not $ isElement someTextNode,
    testIt   "isElementNo2           " $ not $ isElement someComment,
    testIt   "tagNameElement         " $ tagName someElement == Just "baz",
    testIt   "tagNameText            " $ tagName someTextNode == Nothing,
    testIt   "tagNameComment         " $ tagName someComment == Nothing,
    testIt   "getAttributePresent    " $ getAttribute "fiz" someElement
                                            == Just "buzz",
    testIt   "getAttributeMissing    " $ getAttribute "baz" someElement
                                            == Nothing,
    testIt   "getAttributeWrongType  " $ getAttribute "fix" someTextNode
                                            == Nothing,
    testIt   "hasAttributePresent    " $ hasAttribute "fiz" someElement,
    testIt   "hasAttributeMissing    " $ not $ hasAttribute "baz" someElement,
    testIt   "hasAttributeWrongType  " $ not $ hasAttribute "fix" someTextNode,
    testIt   "setAttributeNew        " $ setAttributeNew,
    testIt   "setAttributeReplace    " $ setAttributeReplace,
    testIt   "setAttributeWrongType  " $ setAttributeWrongType,
    testIt   "nestedNodeText         " $ nestedNodeText,
    testIt   "childNodesElem         " $ childNodesElem,
    testIt   "childNodesOther        " $ childNodesOther,
    testIt   "childElemsTest         " $ childElemsTest,
    testIt   "childElemsTagTest      " $ childElemsTagTest,
    testIt   "childElemTagExists     " $ childElemTagExists,
    testIt   "childElemTagNotExists  " $ childElemTagNotExists,
    testIt   "childElemTagOther      " $ childElemTagOther,
    testIt   "descNodesElem          " $ descNodesElem,
    testIt   "descNodesOther         " $ descNodesOther,
    testIt   "descElemsTest          " $ descElemsTest,
    testIt   "descElemsTagTest       " $ descElemsTagTest,
    testIt   "descElemTagExists      " $ descElemTagExists,
    testIt   "descElemTagDFS         " $ descElemTagDFS,
    testIt   "descElemTagNotExists   " $ descElemTagNotExists,
    testIt   "descElemTagOther       " $ descElemTagOther
    ]


compareExternalIDs :: Bool
compareExternalIDs = Public "foo" "bar" /= System "bar"

compareInternalSubs :: Bool
compareInternalSubs = InternalText "" /= NoInternalSubset

compareDoctypes :: Bool
compareDoctypes = DocType "html" NoExternalID NoInternalSubset
               /= DocType "foo"  NoExternalID NoInternalSubset

compareNodes :: Bool
compareNodes = TextNode "" /= Comment ""

compareDocuments :: Bool
compareDocuments = XmlDocument UTF8 Nothing [] /= HtmlDocument UTF8 Nothing []

compareEncodings :: Bool
compareEncodings = UTF8 /= UTF16BE

exerciseShows :: Assertion
exerciseShows = do
    assertBool "1" $ length (showList [NoExternalID] "") > 0
    assertBool "2" $ length (showList [NoInternalSubset] "") > 0
    assertBool "3" $ length (showList [DocType "foo" NoExternalID NoInternalSubset] "") > 0
    assertBool "4" $ length (showList [TextNode ""] "") > 0
    assertBool "5" $ length (showList [XmlDocument UTF8 Nothing []] "") > 0
    assertBool "6" $ length (showList [UTF8] "") > 0

docNodeAccessors :: Assertion
docNodeAccessors = do
    let hdoc = HtmlDocument UTF8 Nothing []
    assertEqual "html enc"  (docEncoding hdoc) UTF8
    assertEqual "html type" (docType hdoc) Nothing
    assertEqual "html nodes" (docContent hdoc) []

    let xdoc = XmlDocument UTF8 Nothing []
    assertEqual "xml enc"  (docEncoding xdoc) UTF8
    assertEqual "xml type" (docType xdoc) Nothing
    assertEqual "xml nodes" (docContent xdoc) []

    let elm = Element  "foo" [] []
    let txt = TextNode ""
    let cmt = Comment  ""
    assertEqual "elm tag"   (elementTag      elm) "foo"
    assertEqual "elm attr"  (elementAttrs    elm) []
    assertEqual "elm child" (elementChildren elm) []
    assertBool  "txt tag"   $ isBottom (elementTag      txt)
    assertBool  "txt attr"  $ isBottom (elementAttrs    txt)
    assertBool  "txt child" $ isBottom (elementChildren txt)
    assertBool  "cmt tag"   $ isBottom (elementTag      cmt)
    assertBool  "cmt attr"  $ isBottom (elementAttrs    cmt)
    assertBool  "cmt child" $ isBottom (elementChildren cmt)


someTextNode :: Node
someTextNode = TextNode "foo"

someComment :: Node
someComment = Comment "bar"

someElement :: Node
someElement = Element "baz" [("fiz","buzz")] [TextNode "content"]

someTree :: Node
someTree = Element "department" [("code", "A17")] [
    Element "employee" [("name", "bob")] [
        Comment "My best friend",
        Element "address" [] [
            TextNode "123 My Road"
            ]
        ],
    Element "employee" [("name", "alice")] [
        Element "address" [] [
            TextNode "124 My Road"
            ],
        Element "phone" [] [
            TextNode "555-1234"
            ]
        ]
    ]

setAttributeNew :: Bool
setAttributeNew =
    let e = setAttribute "flo" "friz" someElement
    in  length (elementAttrs e) == 2
        && getAttribute "fiz" e == Just "buzz"
        && getAttribute "flo" e == Just "friz"

setAttributeReplace :: Bool
setAttributeReplace =
    let e = setAttribute "fiz" "bat" someElement
    in  length (elementAttrs e) == 1
        && getAttribute "fiz" e == Just "bat"

setAttributeWrongType :: Bool
setAttributeWrongType =
    setAttribute "fuss" "plus" someTextNode == someTextNode
    && setAttribute "fuss" "plus" someComment == someComment

nestedNodeText :: Bool
nestedNodeText = nodeText someTree == "123 My Road124 My Road555-1234"

childNodesElem :: Bool
childNodesElem = length (childNodes n) == 3
    where n = Element "foo" [] [ TextNode "bar",
                                 Comment  "baz",
                                 Element  "bat" [] [] ]

childNodesOther :: Bool
childNodesOther = childNodes (TextNode "foo") == []
               && childNodes (Comment "bar")  == []

childElemsTest :: Bool
childElemsTest = length (childElements n) == 1
    where n = Element "foo" [] [ TextNode "bar",
                                 Comment  "baz",
                                 Element  "bat" [] [] ]

childElemsTagTest :: Bool
childElemsTagTest = length (childElementsTag "good" n) == 2
    where n = Element "parent" [] [
                Element "good" [] [],
                TextNode "foo",
                Comment "bar",
                Element "bad" [] [],
                Element "good" [] [],
                Element "bad" [] []
              ]

childElemTagExists :: Bool
childElemTagExists = childElementTag "b" n == Just (Element "b" [] [])
    where n = Element "parent" [] [
                Element "a" [] [],
                Element "b" [] [],
                Element "c" [] []
              ]

childElemTagNotExists :: Bool
childElemTagNotExists = childElementTag "b" n == Nothing
    where n = Element "parent" [] [
                Element "a" [] [],
                Element "c" [] []
              ]

childElemTagOther :: Bool
childElemTagOther = childElementTag "b" n == Nothing
    where n = TextNode ""


descNodesElem :: Bool
descNodesElem = length (descendantNodes n) == 3
    where n = Element "foo" [] [ TextNode "bar",
                                 Element  "bat" [] [ Comment  "baz" ] ]

descNodesOther :: Bool
descNodesOther = descendantNodes (TextNode "foo") == []
              && descendantNodes (Comment "bar")  == []

descElemsTest :: Bool
descElemsTest = length (descendantElements n) == 1
    where n = Element "foo" [] [ TextNode "bar",
                                 Element  "bat" [] [ Comment  "baz" ] ]

descElemsTagTest :: Bool
descElemsTagTest = length (descendantElementsTag "good" n) == 2
    where n = Element "parent" [] [
                TextNode "foo",
                Element "good" [] [],
                Comment "bar",
                Element "parent" [] [ Element "good" [] [] ],
                Element "bad" [] []
              ]

descElemTagExists :: Bool
descElemTagExists = descendantElementTag "b" n == Just (Element "b" [] [])
    where n = Element "parent" [] [
                Element "a" [] [ Element "b" [] [] ],
                Element "c" [] []
              ]

descElemTagDFS :: Bool
descElemTagDFS = descendantElementTag "b" n == Just (Element "b" [] [])
    where n = Element "parent" [] [
                Element "a" [] [ Element "b" [] [] ],
                Element "b" [("wrong", "")] [],
                Element "c" [] []
              ]

descElemTagNotExists :: Bool
descElemTagNotExists = descendantElementTag "b" n == Nothing
    where n = Element "parent" [] [
                Element "a" [] [],
                Element "c" [] [ Element "d" [] [] ]
              ]

descElemTagOther :: Bool
descElemTagOther = descendantElementTag "b" n == Nothing
    where n = TextNode ""

