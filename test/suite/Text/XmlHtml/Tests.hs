{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Text.XmlHtml.Tests (tests) where

import           Blaze.ByteString.Builder
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import           Data.String
import           Data.Text ()                  -- for string instance
import qualified Data.Text.Encoding as T
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, Node)
import           Text.Blaze
import           Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as H
import           Text.Blaze.Renderer.XmlHtml
import           Text.XmlHtml
import           Text.XmlHtml.CursorTests
import           Text.XmlHtml.DocumentTests
import           Text.XmlHtml.TestCommon
import           Text.XmlHtml.OASISTest


------------------------------------------------------------------------------
-- The master list of tests to run. ------------------------------------------
------------------------------------------------------------------------------

tests :: [Test]
tests = xmlParsingTests
     ++ htmlXMLParsingTests
     ++ htmlParsingQuirkTests
     ++ xmlRenderingTests
     ++ htmlXMLRenderingTests
     ++ htmlRenderingQuirkTests
     ++ documentTests
     ++ cursorTests
     ++ blazeRenderTests
     ++ testsOASIS


------------------------------------------------------------------------------
-- XML Parsing Tests ---------------------------------------------------------
------------------------------------------------------------------------------

xmlParsingTests :: [Test]
xmlParsingTests = [
    testCase "byteOrderMark          " byteOrderMark,
    testIt   "emptyDocument          " emptyDocument,
    testIt   "publicDocType          " publicDocType,
    testIt   "systemDocType          " systemDocType,
    testIt   "emptyDocType           " emptyDocType,
    testCase "dtdInternalScan        " dtdInternalScan,
    testIt   "textOnly               " textOnly,
    testIt   "textWithRefs           " textWithRefs,
    testIt   "untermRef              " untermRef,
    testIt   "textWithCDATA          " textWithCDATA,
    testIt   "cdataOnly              " cdataOnly,
    testIt   "commentOnly            " commentOnly,
    testIt   "emptyElement           " emptyElement,
    testIt   "emptyElement2          " emptyElement2,
    testIt   "elemWithText           " elemWithText,
    testIt   "xmlDecl                " xmlDecl,
    testIt   "procInst               " procInst,
    testIt   "badDoctype1            " badDoctype1,
    testIt   "badDoctype2            " badDoctype2,
    testIt   "badDoctype3            " badDoctype3,
    testIt   "badDoctype4            " badDoctype4,
    testIt   "badDoctype5            " badDoctype5,
    testCase "tagNames               " tagNames
    ]

byteOrderMark :: Assertion
byteOrderMark = do
    assertEqual "BOM UTF16BE" (Right $ XmlDocument UTF16BE Nothing [])
        (parseXML "" $ T.encodeUtf16BE "\xFEFF")
    assertEqual "BOM UTF16LE" (Right $ XmlDocument UTF16LE Nothing [])
        (parseXML "" $ T.encodeUtf16LE "\xFEFF")
    assertEqual "BOM UTF8" (Right $ XmlDocument UTF8 Nothing [])
        (parseXML "" $ T.encodeUtf8 "\xFEFF")
    assertEqual "BOM None" (Right $ XmlDocument UTF8 Nothing [])
        (parseXML "" $ T.encodeUtf8 "")

emptyDocument :: Bool
emptyDocument = parseXML "" ""
    == Right (XmlDocument UTF8 Nothing [])

publicDocType :: Bool
publicDocType = parseXML "" "<!DOCTYPE tag PUBLIC \"foo\" \"bar\">"
    == Right (XmlDocument UTF8 (Just (DocType "tag" (Public "foo" "bar") NoInternalSubset)) [])

systemDocType :: Bool
systemDocType = parseXML "" "<!DOCTYPE tag SYSTEM \"foo\">"
    == Right (XmlDocument UTF8 (Just (DocType "tag" (System "foo") NoInternalSubset)) [])

emptyDocType :: Bool
emptyDocType  = parseXML "" "<!DOCTYPE tag >"
    == Right (XmlDocument UTF8 (Just (DocType "tag" NoExternalID NoInternalSubset)) [])

dtdInternalScan :: Assertion
dtdInternalScan = do
    assertEqual "empty" (parseXML "" "<!DOCTYPE a []>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[]" ))) []))
    assertBool "bad brackets" (isLeft $ parseXML "" "<!DOCTYPE a ()>")
    assertEqual "quoted" (parseXML "" "<!DOCTYPE a [\"]\"]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[\"]\"]" ))) []))
    assertBool "bad quote" (isLeft $ parseXML "" "<!DOCTYPE a [\"]>")
    assertEqual "nested brackets" (parseXML "" "<!DOCTYPE a [[[]]]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[[[]]]" ))) []))
    assertEqual "part comment 1" (parseXML "" "<!DOCTYPE a [<]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[<]" ))) []))
    assertEqual "part comment 2" (parseXML "" "<!DOCTYPE a [[<]]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[[<]]" ))) []))
    assertEqual "part comment 3" (parseXML "" "<!DOCTYPE a [<[]]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[<[]]" ))) []))
    assertEqual "part comment 4" (parseXML "" "<!DOCTYPE a [<!]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[<!]" ))) []))
    assertEqual "part comment 5" (parseXML "" "<!DOCTYPE a [[<!]]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[[<!]]" ))) []))
    assertEqual "part comment 6" (parseXML "" "<!DOCTYPE a [<!-]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[<!-]" ))) []))
    assertEqual "comment" (parseXML "" "<!DOCTYPE a [<!--foo-->]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[<!--foo-->]" ))) []))
    assertEqual "docint 1" (parseXML "" "<!DOCTYPE a [<''<\"\"<!''<!\"\">]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[<''<\"\"<!''<!\"\">]" ))) []))
    assertEqual "docint2" (parseXML "" "<!DOCTYPE a [<![<!-[<!-]<!-''<!-\"\"]]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[<![<!-[<!-]<!-''<!-\"\"]]" ))) []))
    assertEqual "docint3" (parseXML "" "<!DOCTYPE a [<!- ]>")
        (Right (XmlDocument UTF8 (Just (DocType "a" NoExternalID (InternalText
            "[<!- ]" ))) []))
    assertBool "bad comment" (isLeft $ parseXML "" "<!DOCTYPE a [<!-- -- -->]>")

textOnly :: Bool
textOnly      = parseXML "" "sldhfsklj''a's s"
    == Right (XmlDocument UTF8 Nothing [TextNode "sldhfsklj''a's s"])

textWithRefs :: Bool
textWithRefs  = parseXML "" "This is Bob&apos;s sled"
    == Right (XmlDocument UTF8 Nothing [TextNode "This is Bob's sled"])

untermRef :: Bool
untermRef     = isLeft (parseXML "" "&#X6a")

textWithCDATA :: Bool
textWithCDATA = parseXML "" "Testing <![CDATA[with <some> c]data]]>"
    == Right (XmlDocument UTF8 Nothing [TextNode "Testing with <some> c]data"])

cdataOnly :: Bool
cdataOnly     = parseXML "" "<![CDATA[ Testing <![CDATA[ test ]]>"
    == Right (XmlDocument UTF8 Nothing [TextNode " Testing <![CDATA[ test "])

commentOnly :: Bool
commentOnly   = parseXML "" "<!-- this <is> a \"comment -->"
    == Right (XmlDocument UTF8 Nothing [Comment " this <is> a \"comment "])

emptyElement :: Bool
emptyElement  = parseXML "" "<myElement/>"
    == Right (XmlDocument UTF8 Nothing [Element "myElement" [] []])

emptyElement2 :: Bool
emptyElement2  = parseXML "" "<myElement />"
    == Right (XmlDocument UTF8 Nothing [Element "myElement" [] []])

elemWithText :: Bool
elemWithText  = parseXML "" "<myElement>text</myElement>"
    == Right (XmlDocument UTF8 Nothing [Element "myElement" [] [TextNode "text"]])

xmlDecl :: Bool
xmlDecl       = parseXML "" "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    == Right (XmlDocument UTF8 Nothing [])

procInst :: Bool
procInst      = parseXML "" "<?myPI This''is <not> parsed!?>"
    == Right (XmlDocument UTF8 Nothing [])

badDoctype1 :: Bool
badDoctype1    = isLeft $ parseXML "" "<!DOCTYPE>"

badDoctype2 :: Bool
badDoctype2    = isLeft $ parseXML "" "<!DOCTYPE html BAD>"

badDoctype3 :: Bool
badDoctype3    = isLeft $ parseXML "" "<!DOCTYPE html SYSTEM>"

badDoctype4 :: Bool
badDoctype4    = isLeft $ parseXML "" "<!DOCTYPE html PUBLIC \"foo\">"

badDoctype5 :: Bool
badDoctype5    = isLeft $ parseXML "" ("<!DOCTYPE html SYSTEM \"foo\" "
                                       `B.append` "PUBLIC \"bar\" \"baz\">")

tagNames :: Assertion
tagNames = do
    assertBool "tag name 0"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<foo />")
    assertBool "tag name 1"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\xc0\&foo />")
    assertBool "tag name 2"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\xd8\&foo />")
    assertBool "tag name 3"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\xf8\&foo />")
    assertBool "tag name 4"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\x370\&foo />")
    assertBool "tag name 5"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\x37f\&foo />")
    assertBool "tag name 6"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\x200c\&foo />")
    assertBool "tag name 7"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\x2070\&foo />")
    assertBool "tag name 8"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\x2c00\&foo />")
    assertBool "tag name 9"  $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\x3001\&foo />")
    assertBool "tag name 10" $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\xf900\&foo />")
    assertBool "tag name 11" $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\xfdf0\&foo />")
    assertBool "tag name 12" $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<\x10000\&foo />")
    assertBool "tag name 13" $ id  $
        isLeft $ parseXML "" (T.encodeUtf8 "<\xd7\&foo />")
    assertBool "tag name 14" $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<f-oo />")
    assertBool "tag name 15" $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<f.oo />")
    assertBool "tag name 16" $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<f\xb7\&oo />")
    assertBool "tag name 17" $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<f3oo />")
    assertBool "tag name 18" $ not $
        isLeft $ parseXML "" (T.encodeUtf8 "<f\x203f\&oo />")
    assertBool "tag name 19" $ id  $
        isLeft $ parseXML "" (T.encodeUtf8 "<f\x2041\&oo />")


------------------------------------------------------------------------------
-- HTML Repetitions of XML Parsing Tests -------------------------------------
------------------------------------------------------------------------------

htmlXMLParsingTests :: [Test]
htmlXMLParsingTests = [
    testIt "emptyDocumentHTML      " emptyDocumentHTML,
    testIt "publicDocTypeHTML      " publicDocTypeHTML,
    testIt "systemDocTypeHTML      " systemDocTypeHTML,
    testIt "emptyDocTypeHTML       " emptyDocTypeHTML,
    testIt "textOnlyHTML           " textOnlyHTML,
    testIt "textWithRefsHTML       " textWithRefsHTML,
    testIt "textWithCDataHTML      " textWithCDataHTML,
    testIt "cdataOnlyHTML          " cdataOnlyHTML,
    testIt "commentOnlyHTML        " commentOnlyHTML,
    testIt "emptyElementHTML       " emptyElementHTML,
    testIt "emptyElement2HTML      " emptyElement2HTML,
    testIt "elemWithTextHTML       " elemWithTextHTML,
    testIt "xmlDeclHTML            " xmlDeclHTML,
    testIt "procInstHTML           " procInstHTML,
    testIt "badDoctype1HTML        " badDoctype1HTML,
    testIt "badDoctype2HTML        " badDoctype2HTML,
    testIt "badDoctype3HTML        " badDoctype3HTML,
    testIt "badDoctype4HTML        " badDoctype4HTML,
    testIt "badDoctype5HTML        " badDoctype5HTML
    ]

emptyDocumentHTML :: Bool
emptyDocumentHTML = parseHTML "" ""
    == Right (HtmlDocument UTF8 Nothing [])

publicDocTypeHTML :: Bool
publicDocTypeHTML = parseHTML "" "<!DOCTYPE tag PUBLIC \"foo\" \"bar\">"
    == Right (HtmlDocument UTF8 (Just (DocType "tag" (Public "foo" "bar") NoInternalSubset)) [])

systemDocTypeHTML :: Bool
systemDocTypeHTML = parseHTML "" "<!DOCTYPE tag SYSTEM \"foo\">"
    == Right (HtmlDocument UTF8 (Just (DocType "tag" (System "foo") NoInternalSubset)) [])

emptyDocTypeHTML :: Bool
emptyDocTypeHTML  = parseHTML "" "<!DOCTYPE tag >"
    == Right (HtmlDocument UTF8 (Just (DocType "tag" NoExternalID NoInternalSubset)) [])

textOnlyHTML :: Bool
textOnlyHTML      = parseHTML "" "sldhfsklj''a's s"
    == Right (HtmlDocument UTF8 Nothing [TextNode "sldhfsklj''a's s"])

textWithRefsHTML :: Bool
textWithRefsHTML  = parseHTML "" "This is Bob&apos;s sled"
    == Right (HtmlDocument UTF8 Nothing [TextNode "This is Bob's sled"])

textWithCDataHTML :: Bool
textWithCDataHTML = parseHTML "" "Testing <![CDATA[with <some> c]data]]>"
    == Right (HtmlDocument UTF8 Nothing [TextNode "Testing with <some> c]data"])

cdataOnlyHTML :: Bool
cdataOnlyHTML     = parseHTML "" "<![CDATA[ Testing <![CDATA[ test ]]>"
    == Right (HtmlDocument UTF8 Nothing [TextNode " Testing <![CDATA[ test "])

commentOnlyHTML :: Bool
commentOnlyHTML   = parseHTML "" "<!-- this <is> a \"comment -->"
    == Right (HtmlDocument UTF8 Nothing [Comment " this <is> a \"comment "])

emptyElementHTML :: Bool
emptyElementHTML  = parseHTML "" "<myElement/>"
    == Right (HtmlDocument UTF8 Nothing [Element "myElement" [] []])

emptyElement2HTML :: Bool
emptyElement2HTML = parseHTML "" "<myElement />"
    == Right (HtmlDocument UTF8 Nothing [Element "myElement" [] []])

elemWithTextHTML :: Bool
elemWithTextHTML  = parseHTML "" "<myElement>text</myElement>"
    == Right (HtmlDocument UTF8 Nothing [Element "myElement" [] [TextNode "text"]])

xmlDeclHTML :: Bool
xmlDeclHTML       = parseHTML "" "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    == Right (HtmlDocument UTF8 Nothing [])

procInstHTML :: Bool
procInstHTML      = parseHTML "" "<?myPI This''is <not> parsed!?>"
    == Right (HtmlDocument UTF8 Nothing [])

badDoctype1HTML :: Bool
badDoctype1HTML    = isLeft $ parseHTML "" "<!DOCTYPE>"

badDoctype2HTML :: Bool
badDoctype2HTML    = isLeft $ parseHTML "" "<!DOCTYPE html BAD>"

badDoctype3HTML :: Bool
badDoctype3HTML    = isLeft $ parseHTML "" "<!DOCTYPE html SYSTEM>"

badDoctype4HTML :: Bool
badDoctype4HTML    = isLeft $ parseHTML "" "<!DOCTYPE html PUBLIC \"foo\">"

badDoctype5HTML :: Bool
badDoctype5HTML    = isLeft $ parseHTML "" ("<!DOCTYPE html SYSTEM \"foo\" "
                                       `B.append` "PUBLIC \"bar\" \"baz\">")


------------------------------------------------------------------------------
-- HTML Quirks Parsing Tests -------------------------------------------------
------------------------------------------------------------------------------

htmlParsingQuirkTests :: [Test]
htmlParsingQuirkTests = [
    testIt   "voidElem               " voidElem,
    testIt   "caseInsDoctype1        " caseInsDoctype1,
    testIt   "caseInsDoctype2        " caseInsDoctype2,
    testIt   "voidEmptyElem          " voidEmptyElem,
    testIt   "rawTextElem            " rawTextElem,
    testIt   "endTagCase             " endTagCase,
    testIt   "hexEntityCap           " hexEntityCap,
    testIt   "laxAttrName            " laxAttrName,
    testCase "badAttrName            " badAttrName,
    testIt   "emptyAttr              " emptyAttr,
    testIt   "emptyAttr2             " emptyAttr2,
    testIt   "unquotedAttr           " unquotedAttr,
    testIt   "laxAttrVal             " laxAttrVal,
    testIt   "ampersandInText        " ampersandInText,
    testIt   "omitOptionalEnds       " omitOptionalEnds,
    testIt   "omitEndHEAD            " omitEndHEAD,
    testIt   "omitEndLI              " omitEndLI,
    testIt   "omitEndDT              " omitEndDT,
    testIt   "omitEndDD              " omitEndDD,
    testIt   "omitEndP               " omitEndP,
    testIt   "omitEndRT              " omitEndRT,
    testIt   "omitEndRP              " omitEndRP,
    testIt   "omitEndOPTGRP          " omitEndOPTGRP,
    testIt   "omitEndOPTION          " omitEndOPTION,
    testIt   "omitEndCOLGRP          " omitEndCOLGRP,
    testIt   "omitEndTHEAD           " omitEndTHEAD,
    testIt   "omitEndTBODY           " omitEndTBODY,
    testIt   "omitEndTFOOT           " omitEndTFOOT,
    testIt   "omitEndTR              " omitEndTR,
    testIt   "omitEndTD              " omitEndTD,
    testIt   "omitEndTH              " omitEndTH,
    testIt   "testNewRefs            " testNewRefs,
    testIt   "errorImplicitClose     " errorImplicitClose,
    testIt   "weirdScriptThing       " weirdScriptThing
    ]

caseInsDoctype1 :: Bool
caseInsDoctype1 = parseHTML "" "<!dOcTyPe html SyStEm 'foo'>"
    == Right (HtmlDocument UTF8 (Just (DocType "html" (System "foo") NoInternalSubset)) [])

caseInsDoctype2 :: Bool
caseInsDoctype2 = parseHTML "" "<!dOcTyPe html PuBlIc 'foo' 'bar'>"
    == Right (HtmlDocument UTF8 (Just (DocType "html" (Public "foo" "bar") NoInternalSubset)) [])

voidElem :: Bool
voidElem      = parseHTML "" "<img>"
    == Right (HtmlDocument UTF8 Nothing [Element "img" [] []])

voidEmptyElem :: Bool
voidEmptyElem = parseHTML "" "<img/>"
    == Right (HtmlDocument UTF8 Nothing [Element "img" [] []])

rawTextElem :: Bool
rawTextElem   = parseHTML "" "<script>This<is'\"a]]>test&amp;</script>"
    == Right (HtmlDocument UTF8 Nothing [Element "script" [] [
                    TextNode "This<is'\"a]]>test&amp;"]
                    ])

endTagCase :: Bool
endTagCase    = parseHTML "" "<testing></TeStInG>"
    == Right (HtmlDocument UTF8 Nothing [Element "testing" [] []])

hexEntityCap :: Bool
hexEntityCap  = parseHTML "" "&#X6a;"
    == Right (HtmlDocument UTF8 Nothing [TextNode "\x6a"])

laxAttrName :: Bool
laxAttrName   = parseHTML "" "<test val<fun=\"test\"></test>"
    == Right (HtmlDocument UTF8 Nothing [Element "test" [("val<fun", "test")] []])

badAttrName :: Assertion
badAttrName = do
    assertBool "attr name 0"  $ not $
        isLeft $ parseHTML "" (T.encodeUtf8 "<foo attr/>")
    assertBool "attr name 1"  $
        isLeft $ parseHTML "" (T.encodeUtf8 "<foo \x0002\&ttr/>")
    assertBool "attr name 2"  $
        isLeft $ parseHTML "" (T.encodeUtf8 "<foo \x000F\&ttr/>")
    assertBool "attr name 3"  $
        isLeft $ parseHTML "" (T.encodeUtf8 "<foo \x007F\&ttr/>")
    assertBool "attr name 4"  $
        isLeft $ parseHTML "" (T.encodeUtf8 "<foo \xFDD0\&ttr/>")

emptyAttr :: Bool
emptyAttr     = parseHTML "" "<test attr></test>"
    == Right (HtmlDocument UTF8 Nothing [Element "test" [("attr", "")] []])

emptyAttr2 :: Bool
emptyAttr2     = parseHTML "" "<div itemscope itemtype=\"type\"></div>"
    == Right (HtmlDocument UTF8 Nothing [Element "div" [("itemscope", ""), ("itemtype", "type")] []])

unquotedAttr :: Bool
unquotedAttr  = parseHTML "" "<test attr=you&amp;me></test>"
    == Right (HtmlDocument UTF8 Nothing [Element "test" [("attr", "you&me")] []])

laxAttrVal :: Bool
laxAttrVal    = parseHTML "" "<test attr=\"a &amp; d < b & c\"/>"
    == Right (HtmlDocument UTF8 Nothing [Element "test" [("attr", "a & d < b & c")] []])

ampersandInText :: Bool
ampersandInText   = parseHTML "" "&#X6a"
    == Right (HtmlDocument UTF8 Nothing [TextNode "&#X6a"])

omitOptionalEnds :: Bool
omitOptionalEnds   = parseHTML "" "<html><body><p></html>"
    == Right (HtmlDocument UTF8 Nothing [Element "html" [] [
                Element "body" [] [ Element "p" [] []]]])

omitEndHEAD :: Bool
omitEndHEAD   = parseHTML "" "<head><body>"
    == Right (HtmlDocument UTF8 Nothing [Element "head" [] [], Element "body" [] []])

omitEndLI :: Bool
omitEndLI     = parseHTML "" "<li><li>"
    == Right (HtmlDocument UTF8 Nothing [Element "li" [] [], Element "li" [] []])

omitEndDT :: Bool
omitEndDT     = parseHTML "" "<dt><dd>"
    == Right (HtmlDocument UTF8 Nothing [Element "dt" [] [], Element "dd" [] []])

omitEndDD :: Bool
omitEndDD     = parseHTML "" "<dd><dt>"
    == Right (HtmlDocument UTF8 Nothing [Element "dd" [] [], Element "dt" [] []])

omitEndP :: Bool
omitEndP      = parseHTML "" "<p><h1></h1>"
    == Right (HtmlDocument UTF8 Nothing [Element "p" [] [], Element "h1" [] []])

omitEndRT :: Bool
omitEndRT     = parseHTML "" "<rt><rp>"
    == Right (HtmlDocument UTF8 Nothing [Element "rt" [] [], Element "rp" [] []])

omitEndRP :: Bool
omitEndRP     = parseHTML "" "<rp><rt>"
    == Right (HtmlDocument UTF8 Nothing [Element "rp" [] [], Element "rt" [] []])

omitEndOPTGRP :: Bool
omitEndOPTGRP = parseHTML "" "<optgroup><optgroup>"
    == Right (HtmlDocument UTF8 Nothing [Element "optgroup" [] [], Element "optgroup" [] []])

omitEndOPTION :: Bool
omitEndOPTION = parseHTML "" "<option><option>"
    == Right (HtmlDocument UTF8 Nothing [Element "option" [] [], Element "option" [] []])

omitEndCOLGRP :: Bool
omitEndCOLGRP = parseHTML "" "<colgroup><tbody>"
    == Right (HtmlDocument UTF8 Nothing [Element "colgroup" [] [], Element "tbody" [] []])

omitEndTHEAD :: Bool
omitEndTHEAD  = parseHTML "" "<thead><tbody>"
    == Right (HtmlDocument UTF8 Nothing [Element "thead" [] [], Element "tbody" [] []])

omitEndTBODY :: Bool
omitEndTBODY  = parseHTML "" "<tbody><thead>"
    == Right (HtmlDocument UTF8 Nothing [Element "tbody" [] [], Element "thead" [] []])

omitEndTFOOT :: Bool
omitEndTFOOT  = parseHTML "" "<tfoot><tbody>"
    == Right (HtmlDocument UTF8 Nothing [Element "tfoot" [] [], Element "tbody" [] []])

omitEndTR :: Bool
omitEndTR     = parseHTML "" "<tr><tr>"
    == Right (HtmlDocument UTF8 Nothing [Element "tr" [] [], Element "tr" [] []])

omitEndTD :: Bool
omitEndTD     = parseHTML "" "<td><td>"
    == Right (HtmlDocument UTF8 Nothing [Element "td" [] [], Element "td" [] []])

omitEndTH :: Bool
omitEndTH     = parseHTML "" "<th><td>"
    == Right (HtmlDocument UTF8 Nothing [Element "th" [] [], Element "td" [] []])

testNewRefs :: Bool
testNewRefs   = parseHTML "" "&CenterDot;&doublebarwedge;&fjlig;"
    == Right (HtmlDocument UTF8 Nothing [TextNode "\x000B7\x02306\&fj"])

errorImplicitClose :: Bool
errorImplicitClose = isLeft $ parseHTML "" "<p><pre>foo</pre></p>"

weirdScriptThing :: Bool
weirdScriptThing = parseHTML "" "<div><script type=\"text/javascript\">selector.append('<option>'+name+'</option>');</script></div>"
    == Right (HtmlDocument UTF8 Nothing [
        Element "div" [] [
            Element "script" [("type", "text/javascript")] [
                TextNode "selector.append('<option>'+name+'</option>');"
                ]
            ]
        ])

------------------------------------------------------------------------------
-- XML Rendering Tests -------------------------------------------------------
------------------------------------------------------------------------------

xmlRenderingTests :: [Test]
xmlRenderingTests = [
    testIt "renderByteOrderMark    " renderByteOrderMark,
    testIt "renderByteOrderMarkLE  " renderByteOrderMarkLE,
    testIt "singleQuoteInSysID     " singleQuoteInSysID,
    testIt "doubleQuoteInSysID     " doubleQuoteInSysID,
    testIt "bothQuotesInSysID      " bothQuotesInSysID,
    testIt "doubleQuoteInPubID     " doubleQuoteInPubID,
    testIt "doubleDashInComment    " doubleDashInComment,
    testIt "trailingDashInComment  " trailingDashInComment,
    testIt "renderEmptyText        " renderEmptyText,
    testIt "singleQuoteInAttr      " singleQuoteInAttr,
    testIt "doubleQuoteInAttr      " doubleQuoteInAttr,
    testIt "bothQuotesInAttr       " bothQuotesInAttr
    ]

renderByteOrderMark :: Bool
renderByteOrderMark =
    toByteString (render (XmlDocument UTF16BE Nothing []))
    == T.encodeUtf16BE "\xFEFF<?xml version=\"1.0\" encoding=\"UTF-16\"?>\n"

renderByteOrderMarkLE :: Bool
renderByteOrderMarkLE =
    toByteString (render (XmlDocument UTF16LE Nothing []))
    == T.encodeUtf16LE "\xFEFF<?xml version=\"1.0\" encoding=\"UTF-16\"?>\n"

-- (Appears at the beginning of all XML output)
utf8Decl :: ByteString
utf8Decl = T.encodeUtf8 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

singleQuoteInSysID :: Bool
singleQuoteInSysID =
    toByteString (render (XmlDocument UTF8
        (Just (DocType "html" (System "test\'ing") NoInternalSubset))
        []))
    == utf8Decl `B.append` "<!DOCTYPE html SYSTEM \"test\'ing\">\n"

doubleQuoteInSysID :: Bool
doubleQuoteInSysID =
    toByteString (render (XmlDocument UTF8
        (Just (DocType "html" (System "test\"ing") NoInternalSubset))
        []))
    == utf8Decl `B.append` "<!DOCTYPE html SYSTEM \'test\"ing\'>\n"

bothQuotesInSysID :: Bool
bothQuotesInSysID = isBottom $
    toByteString (render (XmlDocument UTF8
        (Just (DocType "html" (System "test\"\'ing") NoInternalSubset))
        []))

doubleQuoteInPubID :: Bool
doubleQuoteInPubID = isBottom $
    toByteString (render (XmlDocument UTF8
        (Just (DocType "html" (Public "test\"ing" "foo") NoInternalSubset))
        []))

doubleDashInComment :: Bool
doubleDashInComment = isBottom $
    toByteString (render (XmlDocument UTF8 Nothing [
        Comment "test--ing"
        ]))

trailingDashInComment :: Bool
trailingDashInComment = isBottom $
    toByteString (render (XmlDocument UTF8 Nothing [
        Comment "testing-"
        ]))

renderEmptyText :: Bool
renderEmptyText =
    toByteString (render (XmlDocument UTF8 Nothing [
        TextNode ""
        ]))
    == utf8Decl

singleQuoteInAttr :: Bool
singleQuoteInAttr =
    toByteString (render (XmlDocument UTF8 Nothing [
        Element "foo" [("bar", "test\'ing")] []
        ]))
    == utf8Decl `B.append` "<foo bar=\"test\'ing\"/>"

doubleQuoteInAttr :: Bool
doubleQuoteInAttr =
    toByteString (render (XmlDocument UTF8 Nothing [
        Element "foo" [("bar", "test\"ing")] []
        ]))
    == utf8Decl `B.append` "<foo bar=\'test\"ing\'/>"

bothQuotesInAttr :: Bool
bothQuotesInAttr =
    toByteString (render (XmlDocument UTF8 Nothing [
        Element "foo" [("bar", "test\'\"ing")] []
        ]))
    == utf8Decl `B.append` "<foo bar=\"test\'&quot;ing\"/>"


------------------------------------------------------------------------------
-- HTML Repeats of XML Rendering Tests ---------------------------------------
------------------------------------------------------------------------------

htmlXMLRenderingTests :: [Test]
htmlXMLRenderingTests = [
    testIt "hRenderByteOrderMark   " hRenderByteOrderMark,
    testIt "hSingleQuoteInSysID    " hSingleQuoteInSysID,
    testIt "hDoubleQuoteInSysID    " hDoubleQuoteInSysID,
    testIt "hBothQuotesInSysID     " hBothQuotesInSysID,
    testIt "hDoubleQuoteInPubID    " hDoubleQuoteInPubID,
    testIt "hDoubleDashInComment   " hDoubleDashInComment,
    testIt "hTrailingDashInComment " hTrailingDashInComment,
    testIt "hRenderEmptyText       " hRenderEmptyText,
    testIt "hSingleQuoteInAttr     " hSingleQuoteInAttr,
    testIt "hDoubleQuoteInAttr     " hDoubleQuoteInAttr,
    testIt "hBothQuotesInAttr      " hBothQuotesInAttr
    ]

hRenderByteOrderMark :: Bool
hRenderByteOrderMark =
    toByteString (render (HtmlDocument UTF16BE Nothing []))
    == "\xFE\xFF"

hSingleQuoteInSysID :: Bool
hSingleQuoteInSysID =
    toByteString (render (HtmlDocument UTF8
        (Just (DocType "html" (System "test\'ing") NoInternalSubset))
        []))
    == "<!DOCTYPE html SYSTEM \"test\'ing\">\n"

hDoubleQuoteInSysID :: Bool
hDoubleQuoteInSysID =
    toByteString (render (HtmlDocument UTF8
        (Just (DocType "html" (System "test\"ing") NoInternalSubset))
        []))
    == "<!DOCTYPE html SYSTEM \'test\"ing\'>\n"

hBothQuotesInSysID :: Bool
hBothQuotesInSysID = isBottom $
    toByteString (render (HtmlDocument UTF8
        (Just (DocType "html" (System "test\"\'ing") NoInternalSubset))
        []))

hDoubleQuoteInPubID :: Bool
hDoubleQuoteInPubID = isBottom $
    toByteString (render (HtmlDocument UTF8
        (Just (DocType "html" (Public "test\"ing" "foo") NoInternalSubset))
        []))

hDoubleDashInComment :: Bool
hDoubleDashInComment = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Comment "test--ing"
        ]))

hTrailingDashInComment :: Bool
hTrailingDashInComment = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Comment "testing-"
        ]))

hRenderEmptyText :: Bool
hRenderEmptyText =
    toByteString (render (HtmlDocument UTF8 Nothing [
        TextNode ""
        ]))
    == ""

hSingleQuoteInAttr :: Bool
hSingleQuoteInAttr =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo" [("bar", "test\'ing")] []
        ]))
    == "<foo bar=\"test\'ing\"></foo>"

hDoubleQuoteInAttr :: Bool
hDoubleQuoteInAttr =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo" [("bar", "test\"ing")] []
        ]))
    == "<foo bar=\'test\"ing\'></foo>"

hBothQuotesInAttr :: Bool
hBothQuotesInAttr =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo" [("bar", "test\'\"ing")] []
        ]))
    == "<foo bar=\"test\'&quot;ing\"></foo>"


------------------------------------------------------------------------------
-- HTML Quirks Rendering Tests -----------------------------------------------
------------------------------------------------------------------------------

htmlRenderingQuirkTests :: [Test]
htmlRenderingQuirkTests = [
    testIt "renderHTMLVoid         " renderHTMLVoid,
    testIt "renderHTMLVoid2        " renderHTMLVoid2,
    testIt "renderHTMLRaw          " renderHTMLRaw,
    testIt "renderHTMLRawMult      " renderHTMLRawMult,
    testIt "renderHTMLRaw2         " renderHTMLRaw2,
    testIt "renderHTMLRaw3         " renderHTMLRaw3,
    testIt "renderHTMLRaw4         " renderHTMLRaw4,
    testIt "renderHTMLAmpAttr1     " renderHTMLAmpAttr1,
    testIt "renderHTMLAmpAttr2     " renderHTMLAmpAttr2,
    testIt "renderHTMLAmpAttr3     " renderHTMLAmpAttr3,
    testIt "renderHTMLQVoid        " renderHTMLQVoid,
    testIt "renderHTMLQVoid2       " renderHTMLQVoid2,
    testIt "renderHTMLQRaw         " renderHTMLQRaw,
    testIt "renderHTMLQRawMult     " renderHTMLQRawMult,
    testIt "renderHTMLQRaw2        " renderHTMLQRaw2,
    testIt "renderHTMLQRaw3        " renderHTMLQRaw3,
    testIt "renderHTMLQRaw4        " renderHTMLQRaw4
    ]

renderHTMLVoid :: Bool
renderHTMLVoid =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "img" [("src", "foo")] []
        ]))
    == "<img src=\'foo\' />"

renderHTMLVoid2 :: Bool
renderHTMLVoid2 = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "img" [] [TextNode "foo"]
        ]))

renderHTMLRaw :: Bool
renderHTMLRaw =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "script" [("type", "text/javascript")] [
            TextNode "<testing>/&+</foo>"
            ]
        ]))
    == "<script type=\'text/javascript\'><testing>/&+</foo></script>"

renderHTMLRawMult :: Bool
renderHTMLRawMult =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "script" [("type", "text/javascript")] [
            TextNode "foo",
            TextNode "bar"
            ]
        ]))
    == "<script type=\'text/javascript\'>foobar</script>"

renderHTMLRaw2 :: Bool
renderHTMLRaw2 = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "script" [("type", "text/javascript")] [
            TextNode "</script>"
            ]
        ]))

renderHTMLRaw3 :: Bool
renderHTMLRaw3 = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "script" [("type", "text/javascript")] [
            Comment "foo"
            ]
        ]))

renderHTMLRaw4 :: Bool
renderHTMLRaw4 = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "script" [("type", "text/javascript")] [
            TextNode "</scri",
            TextNode "pt>"
            ]
        ]))

renderHTMLAmpAttr1 :: Bool
renderHTMLAmpAttr1 =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "body" [("foo", "a & b")] [] ]))
    == "<body foo=\'a & b\'></body>"

renderHTMLAmpAttr2 :: Bool
renderHTMLAmpAttr2 =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "body" [("foo", "a &amp; b")] [] ]))
    == "<body foo=\'a &amp;amp; b\'></body>"

renderHTMLAmpAttr3 :: Bool
renderHTMLAmpAttr3 =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "body" [("foo", "a &#x65; b")] [] ]))
    == "<body foo=\'a &amp;#x65; b\'></body>"

renderHTMLQVoid :: Bool
renderHTMLQVoid =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo:img" [("src", "foo")] []
        ]))
    == "<foo:img src=\'foo\' />"

renderHTMLQVoid2 :: Bool
renderHTMLQVoid2 = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo:img" [] [TextNode "foo"]
        ]))

renderHTMLQRaw :: Bool
renderHTMLQRaw =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo:script" [("type", "text/javascript")] [
            TextNode "<testing>/&+</foo>"
            ]
        ]))
    == "<foo:script type=\'text/javascript\'><testing>/&+</foo></foo:script>"

renderHTMLQRawMult :: Bool
renderHTMLQRawMult =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo:script" [("type", "text/javascript")] [
            TextNode "foo",
            TextNode "bar"
            ]
        ]))
    == "<foo:script type=\'text/javascript\'>foobar</foo:script>"

renderHTMLQRaw2 :: Bool
renderHTMLQRaw2 = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo:script" [("type", "text/javascript")] [
            TextNode "</foo:script>"
            ]
        ]))

renderHTMLQRaw3 :: Bool
renderHTMLQRaw3 = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo:script" [("type", "text/javascript")] [
            Comment "foo"
            ]
        ]))

renderHTMLQRaw4 :: Bool
renderHTMLQRaw4 = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "foo:script" [("type", "text/javascript")] [
            TextNode "</foo:scri",
            TextNode "pt>"
            ]
        ]))


------------------------------------------------------------------------------
-- Tests of rendering from the blaze-html package ----------------------------
------------------------------------------------------------------------------

blazeRenderTests :: [Test]
blazeRenderTests = [
    testIt   "blazeTestString        " blazeTestString,
    testIt   "blazeTestText          " blazeTestText,
    testIt   "blazeTestBS            " blazeTestBS,
    testIt   "blazeTestPre           " blazeTestPre,
    testIt   "blazeTestExternal      " blazeTestExternal,
    testIt   "blazeTestCustom        " blazeTestCustom,
    testIt   "blazeTestMulti         " blazeTestMulti,
    testIt   "blazeTestEmpty         " blazeTestEmpty
    ]

blazeTestIsString :: (IsString t1, IsString t) =>
     (t -> AttributeValue) -> (t1 -> Html) -> Bool
blazeTestIsString valFunc tagFunc = renderHtml html == result
  where
    html = H.div ! A.class_ (valFunc "foo") $ tagFunc "hello world"
    result = HtmlDocument UTF8 Nothing [Element "div" [("class", "foo")] [TextNode "hello world"]]

blazeTestString :: Bool
blazeTestString = blazeTestIsString H.stringValue H.string

blazeTestText :: Bool
blazeTestText = blazeTestIsString H.textValue H.text

blazeTestBS :: Bool
blazeTestBS = blazeTestIsString H.unsafeByteStringValue H.unsafeByteString

blazeTestPre :: Bool
blazeTestPre = blazeTestIsString H.preEscapedStringValue H.preEscapedString

blazeTestExternal :: Bool
blazeTestExternal = renderHtml html == result
  where
    html = do
        H.script $ H.string "alert('hello world');"
    result = HtmlDocument UTF8 Nothing [Element "script" [] [TextNode "alert('hello world');"]]

blazeTestCustom :: Bool
blazeTestCustom = renderHtml html == result
  where
    html = do
        H.select ! H.customAttribute "dojoType" (mappend "select " "this") $ "foo"
    result = HtmlDocument UTF8 Nothing [Element "select" [("dojoType", "select this")] [TextNode "foo"]]

blazeTestMulti :: Bool
blazeTestMulti = renderHtml (selectCustom `mappend` html) == result
  where
    html = do
        H.link ! A.rel "stylesheet"
    result = HtmlDocument UTF8 Nothing
        [ Element "select" [("dojoType", "select")] [TextNode "foo ", TextNode "bar"]
        , Element "link" [("rel", "stylesheet")] []
        ]

blazeTestEmpty :: Bool
blazeTestEmpty = renderHtmlNodes mempty == []

selectCustom :: Html
selectCustom = H.select ! H.customAttribute "dojoType" "select" $ (mappend "foo " "bar")



