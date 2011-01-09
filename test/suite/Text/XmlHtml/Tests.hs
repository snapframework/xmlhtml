{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Text.XmlHtml.Tests (tests) where

import           Data.ByteString.Char8()
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Text.XmlHtml
import           Text.XmlHtml.OASISTest
import           Test.HUnit hiding (Test)

tests :: [Test]
tests = [
    -- XML parsing tests
    testIt "emptyDocument    " emptyDocument,
    testIt "publicDocType    " publicDocType,
    testIt "systemDocType    " systemDocType,
    testIt "emptyDocType     " emptyDocType,
    testIt "textOnly         " textOnly,
    testIt "textWithRefs     " textWithRefs,
    testIt "untermRef        " untermRef,
    testIt "textWithCDATA    " textWithCDATA,
    testIt "cdataOnly        " cdataOnly,
    testIt "commentOnly      " commentOnly,
    testIt "emptyElement     " emptyElement,
    testIt "emptyElement2    " emptyElement2,
    testIt "elemWithText     " elemWithText,
    testIt "xmlDecl          " xmlDecl,
    testIt "procInst         " procInst,

    -- Repeat XML tests with HTML parser
    testIt "emptyDocumentHTML" emptyDocumentHTML,
    testIt "publicDocTypeHTML" publicDocTypeHTML,
    testIt "systemDocTypeHTML" systemDocTypeHTML,
    testIt "emptyDocTypeHTML " emptyDocTypeHTML,
    testIt "textOnlyHTML     " textOnlyHTML,
    testIt "textWithRefsHTML " textWithRefsHTML,
    testIt "untermRefHTML    " untermRefHTML,
    testIt "textWithCDataHTML" textWithCDataHTML,
    testIt "cdataOnlyHTML    " cdataOnlyHTML,
    testIt "commentOnlyHTML  " commentOnlyHTML,
    testIt "emptyElementHTML " emptyElementHTML,
    testIt "emptyElement2HTML" emptyElement2HTML,
    testIt "elemWithTextHTML " elemWithTextHTML,
    testIt "xmlDeclHTML      " xmlDeclHTML,
    testIt "procInstHTML     " procInstHTML,

    -- testIt HTML parser quirks
    testIt "voidElem         " voidElem,
    testIt "voidEmptyElem    " voidEmptyElem,
    testIt "rawTextElem      " rawTextElem,
    testIt "rcdataElem       " rcdataElem,
    testIt "endTagCase       " endTagCase,
    testIt "hexEntityCap     " hexEntityCap,
    testIt "laxAttrName      " laxAttrName,
    testIt "emptyAttr        " emptyAttr,
    testIt "unquotedAttr     " unquotedAttr,
    testIt "laxAttrVal       " laxAttrVal,
    testIt "omitOptionalEnds " omitOptionalEnds,
    testIt "omitEndHEAD      " omitEndHEAD,
    testIt "omitEndLI        " omitEndLI,
    testIt "omitEndDT        " omitEndDT,
    testIt "omitEndDD        " omitEndDD,
    testIt "omitEndP         " omitEndP,
    testIt "omitEndRT        " omitEndRT,
    testIt "omitEndRP        " omitEndRP,
    testIt "omitEndOPTGRP    " omitEndOPTGRP,
    testIt "omitEndOPTION    " omitEndOPTION,
    testIt "omitEndCOLGRP    " omitEndCOLGRP,
    testIt "omitEndTHEAD     " omitEndTHEAD,
    testIt "omitEndTBODY     " omitEndTBODY,
    testIt "omitEndTFOOT     " omitEndTFOOT,
    testIt "omitEndTR        " omitEndTR,
    testIt "omitEndTD        " omitEndTD,
    testIt "omitEndTH        " omitEndTH,
    testIt "testNewRefs      " testNewRefs
    ]
    ++ testsOASIS

testIt :: TestName -> Bool -> Test
testIt name b = testCase name $ assertBool name b

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

e :: Encoding
e = UTF8

emptyDocument :: Bool
emptyDocument = parseXML "" ""
    == Right (XmlDocument e Nothing [])

publicDocType :: Bool
publicDocType = parseXML "" "<!DOCTYPE tag PUBLIC \"foo\" \"bar\">"
    == Right (XmlDocument e (Just (DocType "tag" (Just (Public "foo" "bar")))) [])

systemDocType :: Bool
systemDocType = parseXML "" "<!DOCTYPE tag SYSTEM \"foo\">"
    == Right (XmlDocument e (Just (DocType "tag" (Just (System "foo")))) [])

emptyDocType :: Bool
emptyDocType  = parseXML "" "<!DOCTYPE tag >"
    == Right (XmlDocument e (Just (DocType "tag" Nothing)) [])

textOnly :: Bool
textOnly      = parseXML "" "sldhfsklj''a's s"
    == Right (XmlDocument e Nothing [TextNode "sldhfsklj''a's s"])

textWithRefs :: Bool
textWithRefs  = parseXML "" "This is Bob&apos;s sled"
    == Right (XmlDocument e Nothing [TextNode "This is Bob's sled"])

untermRef :: Bool
untermRef     = isLeft (parseXML "" "&#X6a")

textWithCDATA :: Bool
textWithCDATA = parseXML "" "Testing <![CDATA[with <some> c]data]]>"
    == Right (XmlDocument e Nothing [TextNode "Testing with <some> c]data"])

cdataOnly :: Bool
cdataOnly     = parseXML "" "<![CDATA[ Testing <![CDATA[ test ]]>"
    == Right (XmlDocument e Nothing [TextNode " Testing <![CDATA[ test "])

commentOnly :: Bool
commentOnly   = parseXML "" "<!-- this <is> a \"comment -->"
    == Right (XmlDocument e Nothing [Comment " this <is> a \"comment "])

emptyElement :: Bool
emptyElement  = parseXML "" "<myElement/>"
    == Right (XmlDocument e Nothing [Element "myElement" [] []])

emptyElement2 :: Bool
emptyElement2  = parseXML "" "<myElement />"
    == Right (XmlDocument e Nothing [Element "myElement" [] []])

elemWithText :: Bool
elemWithText  = parseXML "" "<myElement>text</myElement>"
    == Right (XmlDocument e Nothing [Element "myElement" [] [TextNode "text"]])

xmlDecl :: Bool
xmlDecl       = parseXML "" "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    == Right (XmlDocument e Nothing [])

procInst :: Bool
procInst      = parseXML "" "<?myPI This''is <not> parsed!?>"
    == Right (XmlDocument e Nothing [])


emptyDocumentHTML :: Bool
emptyDocumentHTML = parseHTML "" ""
    == Right (HtmlDocument e Nothing [])

publicDocTypeHTML :: Bool
publicDocTypeHTML = parseHTML "" "<!DOCTYPE tag PUBLIC \"foo\" \"bar\">"
    == Right (HtmlDocument e (Just (DocType "tag" (Just (Public "foo" "bar")))) [])

systemDocTypeHTML :: Bool
systemDocTypeHTML = parseHTML "" "<!DOCTYPE tag SYSTEM \"foo\">"
    == Right (HtmlDocument e (Just (DocType "tag" (Just (System "foo")))) [])

emptyDocTypeHTML :: Bool
emptyDocTypeHTML  = parseHTML "" "<!DOCTYPE tag >"
    == Right (HtmlDocument e (Just (DocType "tag" Nothing)) [])

textOnlyHTML :: Bool
textOnlyHTML      = parseHTML "" "sldhfsklj''a's s"
    == Right (HtmlDocument e Nothing [TextNode "sldhfsklj''a's s"])

textWithRefsHTML :: Bool
textWithRefsHTML  = parseHTML "" "This is Bob&apos;s sled"
    == Right (HtmlDocument e Nothing [TextNode "This is Bob's sled"])

untermRefHTML :: Bool
untermRefHTML     = isLeft (parseHTML "" "&#X6a")

textWithCDataHTML :: Bool
textWithCDataHTML = parseHTML "" "Testing <![CDATA[with <some> c]data]]>"
    == Right (HtmlDocument e Nothing [TextNode "Testing with <some> c]data"])

cdataOnlyHTML :: Bool
cdataOnlyHTML     = parseHTML "" "<![CDATA[ Testing <![CDATA[ test ]]>"
    == Right (HtmlDocument e Nothing [TextNode " Testing <![CDATA[ test "])

commentOnlyHTML :: Bool
commentOnlyHTML   = parseHTML "" "<!-- this <is> a \"comment -->"
    == Right (HtmlDocument e Nothing [Comment " this <is> a \"comment "])

emptyElementHTML :: Bool
emptyElementHTML  = parseHTML "" "<myElement/>"
    == Right (HtmlDocument e Nothing [Element "myElement" [] []])

emptyElement2HTML :: Bool
emptyElement2HTML = parseHTML "" "<myElement />"
    == Right (HtmlDocument e Nothing [Element "myElement" [] []])

elemWithTextHTML :: Bool
elemWithTextHTML  = parseHTML "" "<myElement>text</myElement>"
    == Right (HtmlDocument e Nothing [Element "myElement" [] [TextNode "text"]])

xmlDeclHTML :: Bool
xmlDeclHTML       = parseHTML "" "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    == Right (HtmlDocument e Nothing [])

procInstHTML :: Bool
procInstHTML      = parseHTML "" "<?myPI This''is <not> parsed!?>"
    == Right (HtmlDocument e Nothing [])

voidElem :: Bool
voidElem      = parseHTML "" "<img>"
    == Right (HtmlDocument e Nothing [Element "img" [] []])

voidEmptyElem :: Bool
voidEmptyElem = parseHTML "" "<img/>"
    == Right (HtmlDocument e Nothing [Element "img" [] []])

rawTextElem :: Bool
rawTextElem   = parseHTML "" "<script>This<is'\"a]]>test&amp;</script>"
    == Right (HtmlDocument e Nothing [Element "script" [] [
                    TextNode "This<is'\"a]]>test&amp;"]
                    ])

rcdataElem :: Bool
rcdataElem    = parseHTML "" "<textarea>This<is>'\"a]]>test&amp;</textarea>"
    == Right (HtmlDocument e Nothing [Element "textarea" [] [
                    TextNode "This<is>'\"a]]>test&"]
                    ])

endTagCase :: Bool
endTagCase    = parseHTML "" "<testing></TeStInG>"
    == Right (HtmlDocument e Nothing [Element "testing" [] []])

hexEntityCap :: Bool
hexEntityCap  = parseHTML "" "&#X6a;"
    == Right (HtmlDocument e Nothing [TextNode "\x6a"])

laxAttrName :: Bool
laxAttrName   = parseHTML "" "<test val<fun=\"test\"></test>"
    == Right (HtmlDocument e Nothing [Element "test" [("val<fun", "test")] []])

emptyAttr :: Bool
emptyAttr     = parseHTML "" "<test attr></test>"
    == Right (HtmlDocument e Nothing [Element "test" [("attr", "")] []])

unquotedAttr :: Bool
unquotedAttr  = parseHTML "" "<test attr=you&amp;me></test>"
    == Right (HtmlDocument e Nothing [Element "test" [("attr", "you&me")] []])

laxAttrVal :: Bool
laxAttrVal    = parseHTML "" "<test attr=\"a &amp; d < b & c\"/>"
    == Right (HtmlDocument e Nothing [Element "test" [("attr", "a & d < b & c")] []])

omitOptionalEnds :: Bool
omitOptionalEnds   = parseHTML "" "<html><body><p></html>"
    == Right (HtmlDocument e Nothing [Element "html" [] [
                Element "body" [] [ Element "p" [] []]]])

omitEndHEAD :: Bool
omitEndHEAD   = parseHTML "" "<head><body>"
    == Right (HtmlDocument e Nothing [Element "head" [] [], Element "body" [] []])

omitEndLI :: Bool
omitEndLI     = parseHTML "" "<li><li>"
    == Right (HtmlDocument e Nothing [Element "li" [] [], Element "li" [] []])

omitEndDT :: Bool
omitEndDT     = parseHTML "" "<dt><dd>"
    == Right (HtmlDocument e Nothing [Element "dt" [] [], Element "dd" [] []])

omitEndDD :: Bool
omitEndDD     = parseHTML "" "<dd><dt>"
    == Right (HtmlDocument e Nothing [Element "dd" [] [], Element "dt" [] []])

omitEndP :: Bool
omitEndP      = parseHTML "" "<p><h1></h1>"
    == Right (HtmlDocument e Nothing [Element "p" [] [], Element "h1" [] []])

omitEndRT :: Bool
omitEndRT     = parseHTML "" "<rt><rp>"
    == Right (HtmlDocument e Nothing [Element "rt" [] [], Element "rp" [] []])

omitEndRP :: Bool
omitEndRP     = parseHTML "" "<rp><rt>"
    == Right (HtmlDocument e Nothing [Element "rp" [] [], Element "rt" [] []])

omitEndOPTGRP :: Bool
omitEndOPTGRP = parseHTML "" "<optgroup><optgroup>"
    == Right (HtmlDocument e Nothing [Element "optgroup" [] [], Element "optgroup" [] []])

omitEndOPTION :: Bool
omitEndOPTION = parseHTML "" "<option><option>"
    == Right (HtmlDocument e Nothing [Element "option" [] [], Element "option" [] []])

omitEndCOLGRP :: Bool
omitEndCOLGRP = parseHTML "" "<colgroup><tbody>"
    == Right (HtmlDocument e Nothing [Element "colgroup" [] [], Element "tbody" [] []])

omitEndTHEAD :: Bool
omitEndTHEAD  = parseHTML "" "<thead><tbody>"
    == Right (HtmlDocument e Nothing [Element "thead" [] [], Element "tbody" [] []])

omitEndTBODY :: Bool
omitEndTBODY  = parseHTML "" "<tbody><thead>"
    == Right (HtmlDocument e Nothing [Element "tbody" [] [], Element "thead" [] []])

omitEndTFOOT :: Bool
omitEndTFOOT  = parseHTML "" "<tfoot><tbody>"
    == Right (HtmlDocument e Nothing [Element "tfoot" [] [], Element "tbody" [] []])

omitEndTR :: Bool
omitEndTR     = parseHTML "" "<tr><tr>"
    == Right (HtmlDocument e Nothing [Element "tr" [] [], Element "tr" [] []])

omitEndTD :: Bool
omitEndTD     = parseHTML "" "<td><td>"
    == Right (HtmlDocument e Nothing [Element "td" [] [], Element "td" [] []])

omitEndTH :: Bool
omitEndTH     = parseHTML "" "<th><td>"
    == Right (HtmlDocument e Nothing [Element "th" [] [], Element "td" [] []])

testNewRefs :: Bool
testNewRefs   = parseHTML "" "&CenterDot;&doublebarwedge;&fjlig;"
    == Right (HtmlDocument e Nothing [TextNode "\x000B7\x02306\&fj"])

