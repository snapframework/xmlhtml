{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Text.XmlHtml.Tests (tests) where

import           Blaze.ByteString.Builder
import           Control.Exception as E
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text ()                  -- for string instance
import qualified Data.Text.Encoding as T
import           System.IO.Unsafe
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, Node)
import           Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.XmlHtml
import           Text.XmlHtml
import           Text.XmlHtml.Cursor
import           Text.XmlHtml.CursorTests
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
     ++ nodeTreeTests
     ++ cursorTests
     ++ blazeRenderTests
     ++ testsOASIS


------------------------------------------------------------------------------
-- Code adapted from ChasingBottoms.
--
-- Adding an actual dependency isn't possible because Cabal refuses to build
-- the package due to version conflicts.
--
-- isBottom is impossible to write, but very useful!  So we defy the
-- impossible, and write it anyway.
isBottom :: a -> Bool
isBottom a = unsafePerformIO $
    (E.evaluate a >> return False)
    `E.catch` \ (_ :: ErrorCall)        -> return True
    `E.catch` \ (_ :: PatternMatchFail) -> return True


------------------------------------------------------------------------------
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)


------------------------------------------------------------------------------
-- XML Parsing Tests ---------------------------------------------------------
------------------------------------------------------------------------------

xmlParsingTests :: [Test]
xmlParsingTests = [
    testIt "emptyDocument          " emptyDocument,
    testIt "publicDocType          " publicDocType,
    testIt "systemDocType          " systemDocType,
    testIt "emptyDocType           " emptyDocType,
    testIt "textOnly               " textOnly,
    testIt "textWithRefs           " textWithRefs,
    testIt "untermRef              " untermRef,
    testIt "textWithCDATA          " textWithCDATA,
    testIt "cdataOnly              " cdataOnly,
    testIt "commentOnly            " commentOnly,
    testIt "emptyElement           " emptyElement,
    testIt "emptyElement2          " emptyElement2,
    testIt "elemWithText           " elemWithText,
    testIt "xmlDecl                " xmlDecl,
    testIt "procInst               " procInst,
    testIt "badDoctype1            " badDoctype1,
    testIt "badDoctype2            " badDoctype2,
    testIt "badDoctype3            " badDoctype3,
    testIt "badDoctype4            " badDoctype4,
    testIt "badDoctype5            " badDoctype5
    ]

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
    testIt "voidElem               " voidElem,
    testIt "caseInsDoctype1        " caseInsDoctype1,
    testIt "caseInsDoctype2        " caseInsDoctype2,
    testIt "voidEmptyElem          " voidEmptyElem,
    testIt "rawTextElem            " rawTextElem,
    testIt "rcdataElem             " rcdataElem,
    testIt "endTagCase             " endTagCase,
    testIt "hexEntityCap           " hexEntityCap,
    testIt "laxAttrName            " laxAttrName,
    testIt "emptyAttr              " emptyAttr,
    testIt "unquotedAttr           " unquotedAttr,
    testIt "laxAttrVal             " laxAttrVal,
    testIt "ampersandInText        " ampersandInText,
    testIt "omitOptionalEnds       " omitOptionalEnds,
    testIt "omitEndHEAD            " omitEndHEAD,
    testIt "omitEndLI              " omitEndLI,
    testIt "omitEndDT              " omitEndDT,
    testIt "omitEndDD              " omitEndDD,
    testIt "omitEndP               " omitEndP,
    testIt "omitEndRT              " omitEndRT,
    testIt "omitEndRP              " omitEndRP,
    testIt "omitEndOPTGRP          " omitEndOPTGRP,
    testIt "omitEndOPTION          " omitEndOPTION,
    testIt "omitEndCOLGRP          " omitEndCOLGRP,
    testIt "omitEndTHEAD           " omitEndTHEAD,
    testIt "omitEndTBODY           " omitEndTBODY,
    testIt "omitEndTFOOT           " omitEndTFOOT,
    testIt "omitEndTR              " omitEndTR,
    testIt "omitEndTD              " omitEndTD,
    testIt "omitEndTH              " omitEndTH,
    testIt "testNewRefs            " testNewRefs
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

rcdataElem :: Bool
rcdataElem    = parseHTML "" "<textarea>This<is>'\"a]]>test&amp;</textarea>"
    == Right (HtmlDocument UTF8 Nothing [Element "textarea" [] [
                    TextNode "This<is>'\"a]]>test&"]
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

emptyAttr :: Bool
emptyAttr     = parseHTML "" "<test attr></test>"
    == Right (HtmlDocument UTF8 Nothing [Element "test" [("attr", "")] []])

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


------------------------------------------------------------------------------
-- XML Rendering Tests -------------------------------------------------------
------------------------------------------------------------------------------

xmlRenderingTests :: [Test]
xmlRenderingTests = [
    testIt "renderByteOrderMark    " renderByteOrderMark,
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

-- (Appears at the beginning of all XML output)
utf8Decl :: ByteString
utf8Decl = T.encodeUtf8 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

singleQuoteInSysID :: Bool
singleQuoteInSysID =
    toByteString (render (XmlDocument UTF8
        (Just (DocType "html" (System "test\'ing") NoInternalSubset))
        []))
    == utf8Decl `B.append` "<!DOCTYPE html SYSTEM \"test\'ing\">"

doubleQuoteInSysID :: Bool
doubleQuoteInSysID =
    toByteString (render (XmlDocument UTF8
        (Just (DocType "html" (System "test\"ing") NoInternalSubset))
        []))
    == utf8Decl `B.append` "<!DOCTYPE html SYSTEM \'test\"ing\'>"

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
    == "<!DOCTYPE html SYSTEM \"test\'ing\">"

hDoubleQuoteInSysID :: Bool
hDoubleQuoteInSysID =
    toByteString (render (HtmlDocument UTF8
        (Just (DocType "html" (System "test\"ing") NoInternalSubset))
        []))
    == "<!DOCTYPE html SYSTEM \'test\"ing\'>"

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
    testIt "renderHTMLRcdata       " renderHTMLRcdata,
    testIt "renderHTMLRcdataMult   " renderHTMLRcdataMult,
    testIt "renderHTMLRcdata2      " renderHTMLRcdata2,
    testIt "renderHTMLAmpAttr1     " renderHTMLAmpAttr1,
    testIt "renderHTMLAmpAttr2     " renderHTMLAmpAttr2
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

renderHTMLRcdata :: Bool
renderHTMLRcdata =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "title" [] [
            TextNode "<testing>/&+</&quot;>"
            ]
        ]))
    == "<title>&lt;testing>/&+&lt;/&amp;quot;></title>"

renderHTMLRcdataMult :: Bool
renderHTMLRcdataMult =
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "title" [] [
            TextNode "foo",
            TextNode "bar"
            ]
        ]))
    == "<title>foobar</title>"

renderHTMLRcdata2 :: Bool
renderHTMLRcdata2 = isBottom $
    toByteString (render (HtmlDocument UTF8 Nothing [
        Element "title" [] [
            Comment "foo"
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


------------------------------------------------------------------------------
-- Tests of manipulating the Node tree ---------------------------------------
------------------------------------------------------------------------------

nodeTreeTests :: [Test]
nodeTreeTests = [
    testIt "isTextNodeYes          " $ isTextNode someTextNode,
    testIt "isTextNodeNo           " $ not $ isTextNode someComment,
    testIt "isTextNodeNo2          " $ not $ isTextNode someElement,
    testIt "isCommentYes           " $ isComment someComment,
    testIt "isCommentNo            " $ not $ isComment someTextNode,
    testIt "isCommentNo2           " $ not $ isComment someElement,
    testIt "isElementYes           " $ isElement someElement,
    testIt "isElementNo            " $ not $ isElement someTextNode,
    testIt "isElementNo2           " $ not $ isElement someComment,
    testIt "tagNameElement         " $ tagName someElement == Just "baz",
    testIt "tagNameText            " $ tagName someTextNode == Nothing,
    testIt "tagNameComment         " $ tagName someComment == Nothing,
    testIt "getAttributePresent    " $ getAttribute "fiz" someElement
                                            == Just "buzz",
    testIt "getAttributeMissing    " $ getAttribute "baz" someElement
                                            == Nothing,
    testIt "getAttributeWrongType  " $ getAttribute "fix" someTextNode
                                            == Nothing,
    testIt "hasAttributePresent    " $ hasAttribute "fiz" someElement,
    testIt "hasAttributeMissing    " $ not $ hasAttribute "baz" someElement,
    testIt "hasAttributeWrongType  " $ not $ hasAttribute "fix" someTextNode,
    testIt "setAttributeNew        " $ setAttributeNew,
    testIt "setAttributeReplace    " $ setAttributeReplace,
    testIt "setAttributeWrongType  " $ setAttributeWrongType,
    testIt "nestedNodeText         " $ nestedNodeText,
    testIt "childNodesElem         " $ childNodesElem,
    testIt "childNodesOther        " $ childNodesOther,
    testIt "childElemsTest         " $ childElemsTest,
    testIt "childElemsTagTest      " $ childElemsTagTest,
    testIt "childElemTagExists     " $ childElemTagExists,
    testIt "childElemTagNotExists  " $ childElemTagNotExists,
    testIt "childElemTagOther      " $ childElemTagOther,
    testIt "descNodesElem          " $ descNodesElem,
    testIt "descNodesOther         " $ descNodesOther,
    testIt "descElemsTest          " $ descElemsTest,
    testIt "descElemsTagTest       " $ descElemsTagTest,
    testIt "descElemTagExists      " $ descElemTagExists,
    testIt "descElemTagDFS         " $ descElemTagDFS,
    testIt "descElemTagNotExists   " $ descElemTagNotExists,
    testIt "descElemTagOther       " $ descElemTagOther
    ]

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

blazeTestString = blazeTestIsString H.stringValue H.string
blazeTestText = blazeTestIsString H.textValue H.text
blazeTestBS = blazeTestIsString H.unsafeByteStringValue H.unsafeByteString
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
blazeTestEmpty = renderHtml mempty == HtmlDocument UTF8 Nothing []

selectCustom = H.select ! H.customAttribute "dojoType" "select" $ (mappend "foo " "bar")



