{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module BlazeExample (blazeHtmlExample) where

import Prelude
import qualified Prelude as P
import Data.Monoid (mempty)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

blazeHtmlExample :: Html
blazeHtmlExample = do
    docTypeHtml ! xmlns "http://www.w3.org/1999/xhtml" $ do
        H.head $ do
            meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
            H.title "Data.Text.Lazy.Builder"
            link ! href "ocean.css" ! rel "stylesheet" ! type_ "text/css" ! A.title "Ocean"
            script ! src "haddock-util.js" ! type_ "text/javascript" $ mempty
            script ! type_ "text/javascript" $ "//\nwindow.onload = function () {pageLoad();setSynopsis(\"mini_Data-Text-Lazy-Builder.html\");};\n//"
        body $ do
            H.div ! A.id "package-header" $ do
                ul ! class_ "links" ! A.id "page-menu" $ do
                    li $ a ! href "src/Data-Text-Lazy-Builder.html" $ "Source"
                    li $ a ! href "index.html" $ "Contents"
                    li $ a ! href "doc-index.html" $ "Index"
                p ! class_ "caption" $ "text-0.11.1.5: An efficient packed Unicode text type."
            H.div ! A.id "content" $ do
                H.div ! A.id "module-header" $ do
                    table ! class_ "info" $ do
                        tr $ do
                            th "Portability"
                            td "portable to Hugs and GHC"
                        tr $ do
                            th "Stability"
                            td "experimental"
                        tr $ do
                            th "Maintainer"
                            td "Johan Tibell <johan.tibell@gmail.com>"
                    p ! class_ "caption" $ "Data.Text.Lazy.Builder"
                H.div ! A.id "table-of-contents" $ do
                    p ! class_ "caption" $ "Contents"
                    ul $ do
                        li $ a ! href "#g:1" $ "The Builder type"
                        li $ a ! href "#g:2" $ "Constructing Builders"
                        li $ a ! href "#g:3" $ "Flushing the buffer state"
                H.div ! A.id "description" $ do
                    p ! class_ "caption" $ "Description"
                    H.div ! class_ "doc" $ do
                        p $ do
                            "Efficient construction of lazy"
                            code "Text"
                            "values.  The principal\n operations on a"
                            code "Builder"
                            "are"
                            code "singleton"
                            ","
                            code "fromText"
                            ", and"
                            code "fromLazyText"
                            ", which construct new builders, and"
                            code $ a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Monoid.html#v:mappend" $ "mappend"
                            ", which\n concatenates two builders."
                        p $ do
                            "To get maximum performance when building lazy"
                            code "Text"
                            "values using a builder, associate"
                            code "mappend"
                            "calls to the right.  For example, prefer"
                        pre "singleton 'a' `mappend` (singleton 'b' `mappend` singleton 'c')"
                        p "to"
                        pre "singleton 'a' `mappend` singleton 'b' `mappend` singleton 'c'"
                        p $ do
                            "as the latter associates"
                            code "mappend"
                            "to the left."
                H.div ! A.id "synopsis" $ do
                    p ! A.id "control.syn" ! class_ "caption expander" ! onclick "toggleSection('syn')" $ "Synopsis"
                    ul ! A.id "section.syn" ! class_ "hide" ! onclick "toggleSection('syn')" $ do
                        li ! class_ "src short" $ do
                            H.span ! class_ "keyword" $ "data"
                            a ! href "#t:Builder" $ "Builder"
                        li ! class_ "src short" $ do
                            a ! href "#v:toLazyText" $ "toLazyText"
                            "::"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                            "->"
                            a ! href "Data-Text-Lazy-Internal.html#t:Text" $ "Text"
                        li ! class_ "src short" $ do
                            a ! href "#v:toLazyTextWith" $ "toLazyTextWith"
                            "::"
                            a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Int.html#t:Int" $ "Int"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                            "->"
                            a ! href "Data-Text-Lazy-Internal.html#t:Text" $ "Text"
                        li ! class_ "src short" $ do
                            a ! href "#v:singleton" $ "singleton"
                            "::"
                            a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Char.html#t:Char" $ "Char"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                        li ! class_ "src short" $ do
                            a ! href "#v:fromText" $ "fromText"
                            "::"
                            a ! href "Data-Text-Internal.html#t:Text" $ "Text"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                        li ! class_ "src short" $ do
                            a ! href "#v:fromLazyText" $ "fromLazyText"
                            "::"
                            a ! href "Data-Text-Lazy-Internal.html#t:Text" $ "Text"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                        li ! class_ "src short" $ do
                            a ! href "#v:fromString" $ "fromString"
                            "::"
                            a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Char.html#t:String" $ "String"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                        li ! class_ "src short" $ do
                            a ! href "#v:flush" $ "flush"
                            "::"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                H.div ! A.id "interface" $ do
                    h1 ! A.id "g:1" $ "The Builder type"
                    H.div ! class_ "top" $ do
                        p ! class_ "src" $ do
                            H.span ! class_ "keyword" $ "data"
                            a ! name "t:Builder" ! class_ "def" $ "Builder"
                            a ! href "src/Data-Text-Lazy-Builder.html#Builder" ! class_ "link" $ "Source"
                        H.div ! class_ "doc" $ do
                            p $ do
                                "A"
                                code "Builder"
                                "is an efficient way to build lazy"
                                code "Text"
                                "values.\n There are several functions for constructing builders, but only one\n to inspect them: to extract any data, you have to turn them into\n lazy"
                                code "Text"
                                "values using"
                                code "toLazyText"
                                "."
                            p $ do
                                "Internally, a builder constructs a lazy"
                                code "Text"
                                "by filling arrays\n piece by piece.  As each buffer is filled, it is 'popped' off, to\n become a new chunk of the resulting lazy"
                                code "Text"
                                ".  All this is\n hidden from the user of the"
                                code "Builder"
                                "."
                        H.div ! class_ "subs instances" $ do
                            p ! A.id "control.i:Builder" ! class_ "caption collapser" ! onclick "toggleSection('i:Builder')" $ "Instances"
                            H.div ! A.id "section.i:Builder" ! class_ "show" $ table $ do
                                tr $ do
                                    td ! class_ "src" $ do
                                        a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Eq.html#t:Eq" $ "Eq"
                                        a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                                    td ! class_ "doc empty" $ mempty
                                tr $ do
                                    td ! class_ "src" $ do
                                        a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Ord.html#t:Ord" $ "Ord"
                                        a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                                    td ! class_ "doc empty" $ mempty
                                tr $ do
                                    td ! class_ "src" $ do
                                        a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Text-Show.html#t:Show" $ "Show"
                                        a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                                    td ! class_ "doc empty" $ mempty
                                tr $ do
                                    td ! class_ "src" $ do
                                        a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-String.html#t:IsString" $ "IsString"
                                        a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                                    td ! class_ "doc empty" $ mempty
                                tr $ do
                                    td ! class_ "src" $ do
                                        a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Monoid.html#t:Monoid" $ "Monoid"
                                        a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                                    td ! class_ "doc empty" $ mempty
                    H.div ! class_ "top" $ do
                        p ! class_ "src" $ do
                            a ! name "v:toLazyText" ! class_ "def" $ "toLazyText"
                            "::"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                            "->"
                            a ! href "Data-Text-Lazy-Internal.html#t:Text" $ "Text"
                            a ! href "src/Data-Text-Lazy-Builder.html#toLazyText" ! class_ "link" $ "Source"
                        H.div ! class_ "doc" $ p $ do
                            em "O(n)."
                            "Extract a lazy"
                            code "Text"
                            "from a"
                            code "Builder"
                            "with a default\n buffer size.  The construction work takes place if and when the\n relevant part of the lazy"
                            code "Text"
                            "is demanded."
                    H.div ! class_ "top" $ do
                        p ! class_ "src" $ do
                            a ! name "v:toLazyTextWith" ! class_ "def" $ "toLazyTextWith"
                            "::"
                            a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Int.html#t:Int" $ "Int"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                            "->"
                            a ! href "Data-Text-Lazy-Internal.html#t:Text" $ "Text"
                            a ! href "src/Data-Text-Lazy-Builder.html#toLazyTextWith" ! class_ "link" $ "Source"
                        H.div ! class_ "doc" $ do
                            p $ do
                                em "O(n)."
                                "Extract a lazy"
                                code "Text"
                                "from a"
                                code "Builder"
                                ", using the given\n size for the initial buffer.  The construction work takes place if\n and when the relevant part of the lazy"
                                code "Text"
                                "is demanded."
                            p "If the initial buffer is too small to hold all data, subsequent\n buffers will be the default buffer size."
                    h1 ! A.id "g:2" $ "Constructing Builders"
                    H.div ! class_ "top" $ do
                        p ! class_ "src" $ do
                            a ! name "v:singleton" ! class_ "def" $ "singleton"
                            "::"
                            a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Char.html#t:Char" $ "Char"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                            a ! href "src/Data-Text-Lazy-Builder.html#singleton" ! class_ "link" $ "Source"
                        H.div ! class_ "doc" $ do
                            p $ do
                                em "O(1)."
                                "A"
                                code "Builder"
                                "taking a single character, satisfying"
                            ul $ li $ pre $ do
                                code $ a ! href "Data-Text-Lazy-Builder.html#v:toLazyText" $ "toLazyText"
                                "("
                                code $ a ! href "Data-Text-Lazy-Builder.html#v:singleton" $ "singleton"
                                "c) ="
                                code $ a ! href "Data-Text-Lazy.html#v:singleton" $ "singleton"
                                "c"
                    H.div ! class_ "top" $ do
                        p ! class_ "src" $ do
                            a ! name "v:fromText" ! class_ "def" $ "fromText"
                            "::"
                            a ! href "Data-Text-Internal.html#t:Text" $ "Text"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                            a ! href "src/Data-Text-Lazy-Builder.html#fromText" ! class_ "link" $ "Source"
                        H.div ! class_ "doc" $ do
                            p $ do
                                em "O(1)."
                                "A"
                                code "Builder"
                                "taking a"
                                code $ a ! href "Data-Text-Internal.html#t:Text" $ "Text"
                                ", satisfying"
                            ul $ li $ pre $ do
                                code $ a ! href "Data-Text-Lazy-Builder.html#v:toLazyText" $ "toLazyText"
                                "("
                                code $ a ! href "Data-Text-Lazy-Builder.html#v:fromText" $ "fromText"
                                "t) ="
                                code $ a ! href "Data-Text-Lazy.html#v:fromChunks" $ "fromChunks"
                                "[t]"
                    H.div ! class_ "top" $ do
                        p ! class_ "src" $ do
                            a ! name "v:fromLazyText" ! class_ "def" $ "fromLazyText"
                            "::"
                            a ! href "Data-Text-Lazy-Internal.html#t:Text" $ "Text"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                            a ! href "src/Data-Text-Lazy-Builder.html#fromLazyText" ! class_ "link" $ "Source"
                        H.div ! class_ "doc" $ do
                            p $ do
                                em "O(1)."
                                "A"
                                code "Builder"
                                "taking a lazy"
                                code "Text"
                                ", satisfying"
                            ul $ li $ pre $ do
                                code $ a ! href "Data-Text-Lazy-Builder.html#v:toLazyText" $ "toLazyText"
                                "("
                                code $ a ! href "Data-Text-Lazy-Builder.html#v:fromLazyText" $ "fromLazyText"
                                "t) = t"
                    H.div ! class_ "top" $ do
                        p ! class_ "src" $ do
                            a ! name "v:fromString" ! class_ "def" $ "fromString"
                            "::"
                            a ! href "/Users/markl/Projects/A/platform/hp-mac/src/macos/dist-x86_64/root/Library/Frameworks/GHC.framework/Versions/7.0.4-x86_64/usr/share/doc/ghc/html/libraries/base-4.3.1.0/Data-Char.html#t:String" $ "String"
                            "->"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                            a ! href "src/Data-Text-Lazy-Builder.html#fromString" ! class_ "link" $ "Source"
                        H.div ! class_ "doc" $ do
                            p $ do
                                em "O(1)."
                                "A Builder taking a"
                                code "String"
                                ", satisfying"
                            ul $ li $ pre $ do
                                code $ a ! href "Data-Text-Lazy-Builder.html#v:toLazyText" $ "toLazyText"
                                "("
                                code $ a ! href "Data-Text-Lazy-Builder.html#v:fromString" $ "fromString"
                                "s) ="
                                code $ a ! href "Data-Text-Lazy.html#v:fromChunks" $ "fromChunks"
                                "[S.pack s]"
                    h1 ! A.id "g:3" $ "Flushing the buffer state"
                    H.div ! class_ "top" $ do
                        p ! class_ "src" $ do
                            a ! name "v:flush" ! class_ "def" $ "flush"
                            "::"
                            a ! href "Data-Text-Lazy-Builder.html#t:Builder" $ "Builder"
                            a ! href "src/Data-Text-Lazy-Builder.html#flush" ! class_ "link" $ "Source"
                        H.div ! class_ "doc" $ p $ do
                            em "O(1)."
                            "Pop the strict"
                            code "Text"
                            "we have constructed so far, if any,\n yielding a new chunk in the result lazy"
                            code "Text"
                            "."
            H.div ! A.id "footer" $ p $ do
                "Produced by"
                a ! href "http://www.haskell.org/haddock/" $ "Haddock"
                "version 2.9.2"

