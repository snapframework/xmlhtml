{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Text.XmlHtml.Tests where

import Data.ByteString.Char8()
import Text.XmlHtml

import Test.QuickCheck

data Test = forall prop. Testable prop => Test String prop

runTest :: Test -> IO ()
runTest (Test n t) = putStr n >> putStr "\t" >> quickCheck t

run :: IO ()
run = mapM_ runTest tests

tests :: [Test]
tests = [
    -- XML parsing tests
    Test "emptyDocument    " emptyDocument,
    Test "publicDocType    " publicDocType,
    Test "systemDocType    " systemDocType,
    Test "emptyDocType     " emptyDocType,
    Test "textOnly         " textOnly,
    Test "textWithRefs     " textWithRefs,
    Test "untermRef        " untermRef,
    Test "textWithCDATA    " textWithCDATA,
    Test "cdataOnly        " cdataOnly,
    Test "commentOnly      " commentOnly,
    Test "emptyElement     " emptyElement,
    Test "elemWithText     " elemWithText,
    Test "xmlDecl          " xmlDecl,
    Test "procInst         " procInst,

    -- Repeat XML tests with HTML parser
    Test "emptyDocumentHTML" emptyDocumentHTML,
    Test "publicDocTypeHTML" publicDocTypeHTML,
    Test "systemDocTypeHTML" systemDocTypeHTML,
    Test "emptyDocTypeHTML " emptyDocTypeHTML,
    Test "textOnlyHTML     " textOnlyHTML,
    Test "textWithRefsHTML " textWithRefsHTML,
    Test "untermRefHTML    " untermRefHTML,
    Test "textWithCDataHTML" textWithCDataHTML,
    Test "cdataOnlyHTML    " cdataOnlyHTML,
    Test "commentOnlyHTML  " commentOnlyHTML,
    Test "emptyElementHTML " emptyElementHTML,
    Test "elemWithTextHTML " elemWithTextHTML,
    Test "xmlDeclHTML      " xmlDeclHTML,
    Test "procInstHTML     " procInstHTML,

    -- Test HTML parser quirks
    Test "voidElem         " voidElem,
    Test "voidEmptyElem    " voidEmptyElem,
    Test "rawTextElem      " rawTextElem,
    Test "rcdataElem       " rcdataElem,
    Test "endTagCase       " endTagCase,
    Test "hexEntityCap     " hexEntityCap,
    Test "laxAttrName      " laxAttrName,
    Test "emptyAttr        " emptyAttr,
    Test "unquotedAttr     " unquotedAttr,
    Test "laxAttrVal       " laxAttrVal,
    Test "omitOptionalEnds " omitOptionalEnds,
    Test "omitEndHEAD      " omitEndHEAD,
    Test "omitEndLI        " omitEndLI,
    Test "omitEndDT        " omitEndDT,
    Test "omitEndDD        " omitEndDD,
    Test "omitEndP         " omitEndP,
    Test "omitEndRT        " omitEndRT,
    Test "omitEndRP        " omitEndRP,
    Test "omitEndOPTGRP    " omitEndOPTGRP,
    Test "omitEndOPTION    " omitEndOPTION,
    Test "omitEndCOLGRP    " omitEndCOLGRP,
    Test "omitEndTHEAD     " omitEndTHEAD,
    Test "omitEndTBODY     " omitEndTBODY,
    Test "omitEndTFOOT     " omitEndTFOOT,
    Test "omitEndTR        " omitEndTR,
    Test "omitEndTD        " omitEndTD,
    Test "omitEndTH        " omitEndTH,
    Test "testNewRefs      " testNewRefs
    ]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

e :: Encoding
e = UTF8

emptyDocument = parseXML ""
    == Right (XmlDocument e Nothing [])

publicDocType = parseXML "<!DOCTYPE tag PUBLIC \"foo\" \"bar\">"
    == Right (XmlDocument e (Just (DocType "tag" (Just (Public "foo" "bar")))) [])

systemDocType = parseXML "<!DOCTYPE tag SYSTEM \"foo\">"
    == Right (XmlDocument e (Just (DocType "tag" (Just (System "foo")))) [])

emptyDocType  = parseXML "<!DOCTYPE tag >"
    == Right (XmlDocument e (Just (DocType "tag" Nothing)) [])

textOnly      = parseXML "sldhfsklj''a's s"
    == Right (XmlDocument e Nothing [TextNode "sldhfsklj''a's s"])

textWithRefs  = parseXML "This is Bob&apos;s sled"
    == Right (XmlDocument e Nothing [TextNode "This is Bob's sled"])

untermRef     = isLeft (parseXML "&#X6a")

textWithCDATA = parseXML "Testing <![CDATA[with <some> c]data]]>"
    == Right (XmlDocument e Nothing [TextNode "Testing with <some> c]data"])

cdataOnly     = parseXML "<![CDATA[ Testing <![CDATA[ test ]]>"
    == Right (XmlDocument e Nothing [TextNode " Testing <![CDATA[ test "])

commentOnly   = parseXML "<!-- this <is> a \"comment -->"
    == Right (XmlDocument e Nothing [Comment " this <is> a \"comment "])

emptyElement  = parseXML "<myElement/>"
    == Right (XmlDocument e Nothing [Element "myElement" [] []])

elemWithText  = parseXML "<myElement>text</myElement>"
    == Right (XmlDocument e Nothing [Element "myElement" [] [TextNode "text"]])

xmlDecl       = parseXML "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    == Right (XmlDocument e Nothing [])

procInst      = parseXML "<?myPI This''is <not> parsed!?>"
    == Right (XmlDocument e Nothing [])


emptyDocumentHTML = parseHTML ""
    == Right (HtmlDocument e Nothing [])

publicDocTypeHTML = parseHTML "<!DOCTYPE tag PUBLIC \"foo\" \"bar\">"
    == Right (HtmlDocument e (Just (DocType "tag" (Just (Public "foo" "bar")))) [])

systemDocTypeHTML = parseHTML "<!DOCTYPE tag SYSTEM \"foo\">"
    == Right (HtmlDocument e (Just (DocType "tag" (Just (System "foo")))) [])

emptyDocTypeHTML  = parseHTML "<!DOCTYPE tag >"
    == Right (HtmlDocument e (Just (DocType "tag" Nothing)) [])

textOnlyHTML      = parseHTML "sldhfsklj''a's s"
    == Right (HtmlDocument e Nothing [TextNode "sldhfsklj''a's s"])

textWithRefsHTML  = parseHTML "This is Bob&apos;s sled"
    == Right (HtmlDocument e Nothing [TextNode "This is Bob's sled"])

untermRefHTML     = isLeft (parseHTML "&#X6a")

textWithCDataHTML = parseHTML "Testing <![CDATA[with <some> c]data]]>"
    == Right (HtmlDocument e Nothing [TextNode "Testing with <some> c]data"])

cdataOnlyHTML     = parseHTML "<![CDATA[ Testing <![CDATA[ test ]]>"
    == Right (HtmlDocument e Nothing [TextNode " Testing <![CDATA[ test "])

commentOnlyHTML   = parseHTML "<!-- this <is> a \"comment -->"
    == Right (HtmlDocument e Nothing [Comment " this <is> a \"comment "])

emptyElementHTML  = parseHTML "<myElement/>"
    == Right (HtmlDocument e Nothing [Element "myElement" [] []])

elemWithTextHTML  = parseHTML "<myElement>text</myElement>"
    == Right (HtmlDocument e Nothing [Element "myElement" [] [TextNode "text"]])

xmlDeclHTML       = parseHTML "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    == Right (HtmlDocument e Nothing [])

procInstHTML      = parseHTML "<?myPI This''is <not> parsed!?>"
    == Right (HtmlDocument e Nothing [])

voidElem      = parseHTML "<img>"
    == Right (HtmlDocument e Nothing [Element "img" [] []])

voidEmptyElem = parseHTML "<img/>"
    == Right (HtmlDocument e Nothing [Element "img" [] []])

rawTextElem   = parseHTML "<script>This<is'\"a]]>test&amp;</script>"
    == Right (HtmlDocument e Nothing [Element "script" [] [
                    TextNode "This<is'\"a]]>test&amp;"]
                    ])

rcdataElem    = parseHTML "<textarea>This<is>'\"a]]>test&amp;</textarea>"
    == Right (HtmlDocument e Nothing [Element "textarea" [] [
                    TextNode "This<is>'\"a]]>test&"]
                    ])

endTagCase    = parseHTML "<testing></TeStInG>"
    == Right (HtmlDocument e Nothing [Element "testing" [] []])

hexEntityCap  = parseHTML "&#X6a;"
    == Right (HtmlDocument e Nothing [TextNode "\x6a"])

laxAttrName   = parseHTML "<test val<fun=\"test\"></test>"
    == Right (HtmlDocument e Nothing [Element "test" [("val<fun", "test")] []])

emptyAttr     = parseHTML "<test attr></test>"
    == Right (HtmlDocument e Nothing [Element "test" [("attr", "")] []])

unquotedAttr  = parseHTML "<test attr=you&amp;me></test>"
    == Right (HtmlDocument e Nothing [Element "test" [("attr", "you&me")] []])

laxAttrVal    = parseHTML "<test attr=\"a &amp; d < b & c\"/>"
    == Right (HtmlDocument e Nothing [Element "test" [("attr", "a & d < b & c")] []])

omitOptionalEnds   = parseHTML "<html><body><p></html>"
    == Right (HtmlDocument e Nothing [Element "html" [] [
                Element "body" [] [ Element "p" [] []]]])

omitEndHEAD   = parseHTML "<head><body>"
    == Right (HtmlDocument e Nothing [Element "head" [] [], Element "body" [] []])
omitEndLI     = parseHTML "<li><li>"
    == Right (HtmlDocument e Nothing [Element "li" [] [], Element "li" [] []])
omitEndDT     = parseHTML "<dt><dd>"
    == Right (HtmlDocument e Nothing [Element "dt" [] [], Element "dd" [] []])
omitEndDD     = parseHTML "<dd><dt>"
    == Right (HtmlDocument e Nothing [Element "dd" [] [], Element "dt" [] []])
omitEndP      = parseHTML "<p><h1></h1>"
    == Right (HtmlDocument e Nothing [Element "p" [] [], Element "h1" [] []])
omitEndRT     = parseHTML "<rt><rp>"
    == Right (HtmlDocument e Nothing [Element "rt" [] [], Element "rp" [] []])
omitEndRP     = parseHTML "<rp><rt>"
    == Right (HtmlDocument e Nothing [Element "rp" [] [], Element "rt" [] []])
omitEndOPTGRP = parseHTML "<optgroup><optgroup>"
    == Right (HtmlDocument e Nothing [Element "optgroup" [] [], Element "optgroup" [] []])
omitEndOPTION = parseHTML "<option><option>"
    == Right (HtmlDocument e Nothing [Element "option" [] [], Element "option" [] []])
omitEndCOLGRP = parseHTML "<colgroup><tbody>"
    == Right (HtmlDocument e Nothing [Element "colgroup" [] [], Element "tbody" [] []])
omitEndTHEAD  = parseHTML "<thead><tbody>"
    == Right (HtmlDocument e Nothing [Element "thead" [] [], Element "tbody" [] []])
omitEndTBODY  = parseHTML "<tbody><thead>"
    == Right (HtmlDocument e Nothing [Element "tbody" [] [], Element "thead" [] []])
omitEndTFOOT  = parseHTML "<tfoot><tbody>"
    == Right (HtmlDocument e Nothing [Element "tfoot" [] [], Element "tbody" [] []])
omitEndTR     = parseHTML "<tr><tr>"
    == Right (HtmlDocument e Nothing [Element "tr" [] [], Element "tr" [] []])
omitEndTD     = parseHTML "<td><td>"
    == Right (HtmlDocument e Nothing [Element "td" [] [], Element "td" [] []])
omitEndTH     = parseHTML "<th><td>"
    == Right (HtmlDocument e Nothing [Element "th" [] [], Element "td" [] []])
testNewRefs   = parseHTML "&CenterDot;&doublebarwedge;&fjlig;"
    == Right (HtmlDocument e Nothing [TextNode "\x000B7\x02306\&fj"])

