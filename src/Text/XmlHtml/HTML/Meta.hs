{-# OPTIONS_GHC -O0 -fno-case-merge -fno-strictness -fno-cse #-}
{-# LANGUAGE CPP                         #-}
{-# LANGUAGE OverloadedStrings           #-}

module Text.XmlHtml.HTML.Meta
  ( voidTags
  , rawTextTags
  , isRawText
  , endOmittableLast
  , endOmittableNext
  , explicitAttributes
  , predefinedRefs
  , reversePredefinedRefs
  , entNameMap
  , nameEntMap
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid
#endif
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

------------------------------------------------------------------------------
-- Metadata used for HTML5 quirks mode.                                     --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Void elements as defined by the HTML5 spec.
{-# NOINLINE voidTags #-}
voidTags :: HashSet Text
voidTags = S.fromList [
    "area", "base", "br", "col", "command", "embed", "hr", "img", "input",
    "keygen", "link", "meta", "param", "source", "track", "wbr"
    ]

------------------------------------------------------------------------------
-- | Elements that XmlHtml treats as raw text by default.  Raw text elements
-- are not allowed to have any other tags in them.  This is necessary to
-- support the Javascript less than operator inside a script tag, for example.
--
-- The library uses the 'isRawText' function everywhere instead of checking
-- this set directly because that gives us an escape hatch to avoid the
-- default behavior if necessary.
{-# NOINLINE rawTextTags #-}
rawTextTags :: HashSet Text
rawTextTags = S.fromList [ "script", "style" ]

------------------------------------------------------------------------------
-- | Determine whether a tag should be treated as raw text.  Raw text elements
-- are not allowed to have any other tags in them.  This is necessary to
-- support the Javascript less than operator inside a script tag, for example.
--
-- If a tag is in the 'rawTextTags' set, this function allows you to override
-- that behavior by adding the @xmlhtmlNotRaw@ attribute.  Conversely, if a
-- tag is not in the 'rawTextTags' set, this function allows you to override
-- that by adding the @xmlhtmlRaw@ attribute to the tag.
--
-- This is the function that is actually used in the parser and renderer.
-- 'rawTextTags' is not used any more, but is still provided for backwards
-- compatibility and to let you see which tags are treated as raw by default.
{-# NOINLINE isRawText #-}
isRawText :: Text -> [(Text, Text)] -> Bool
isRawText tag as =
  if tag `S.member` rawTextTags
    then ("xmlhtmlNotRaw", "") `notElem` as
    else ("xmlhtmlRaw", "") `elem` as

------------------------------------------------------------------------------
-- | List of elements with omittable end tags.
{-# NOINLINE endOmittableLast #-}
endOmittableLast :: HashSet Text
endOmittableLast = S.fromList [

    -- Tags which can be implicitly ended in case they are the last element in
    -- their parent.  This list actually includes all of the elements that
    -- have any kind of omittable end tag, since in general when an element
    -- with an omittable end tag isn't specified to be omittable in this way,
    -- it's just because in a complete document it isn't expected to ever be
    -- the last thing in its parent.  We aren't interested in enforcing
    -- element structure rules, so we'll allow it anyway.

    "body", "colgroup", "dd", "dt", "head", "html", "li", "optgroup",
    "option", "p", "rp", "rt", "tbody", "td", "tfoot", "th", "thead", "tr"
    ]

------------------------------------------------------------------------------
-- | Tags which should be considered automatically ended in case one of a
-- certain set of tags pops up.
{-# NOINLINE endOmittableNext #-}
endOmittableNext :: HashMap Text (HashSet Text)
endOmittableNext = M.fromList [
    ("colgroup", S.fromList ["caption", "colgroup", "tbody",
                             "thead", "tfoot", "tr"]),
    ("dd",       S.fromList ["dd", "dt"]),
    ("dt",       S.fromList ["dd", "dt"]),
    ("head",     S.fromList ["body"]),
    ("li",       S.fromList ["li"]),
    ("optgroup", S.fromList ["optgroup"]),
    ("option",   S.fromList ["optgroup", "option"]),
    ("p",        S.fromList ["address", "article", "aside", "blockquote",
                             "dir", "div", "dl", "fieldset", "footer",
                             "form", "h1", "h2", "h3", "h4", "h5", "h6",
                             "header", "hgroup", "hr", "menu", "nav", "ol",
                             "p", "pre", "section", "table", "ul"]),
    ("rp",       S.fromList ["rp", "rt"]),
    ("rt",       S.fromList ["rp", "rt"]),
    ("tbody",    S.fromList ["tbody", "tfoot", "thead"]),
    ("td",       S.fromList ["td", "th"]),
    ("tfoot",    S.fromList ["tbody", "tfoot", "thead"]),
    ("th",       S.fromList ["td", "th"]),
    ("thead",    S.fromList ["tbody", "tfoot", "thead"]),
    ("tr",       S.fromList ["tr"])
    ]

------------------------------------------------------------------------------
-- | Tags and attributes which should always be rendered with an explicit
-- value, even when the value is empty.  This is required by some web browsers
-- for tags that are typically non-empty.
{-# NOINLINE explicitAttributes #-}
explicitAttributes :: HashMap Text (HashSet Text)
explicitAttributes = M.fromList [
    ("a", S.fromList ["href"])
    ]

------------------------------------------------------------------------------
-- | Predefined character entity references as defined by the HTML5 spec.
{-# NOINLINE predefinedRefs #-}
predefinedRefs :: Map Text Text
predefinedRefs = mconcat $ map Map.fromList [
      reftab1
    , reftab2
    , reftab3
    , reftab4
    , reftab5
    , reftab6
    , reftab7
    , reftab8
    , reftab9
    , reftab10
    , reftab11
    , reftab12
    , reftab13
    , reftab14
    , reftab15
    , reftab16
    , reftab17
    , reftab18
    , reftab19
    , reftab20
    , reftab21
    , reftab22
    , reftab23
    , reftab24
    , reftab25
    , reftab26
    , reftab27
    , reftab28
    , reftab29
    , reftab30
    , reftab31
    , reftab32
    , reftab33
    , reftab34
    , reftab35
    , reftab36
    , reftab37
    , reftab38
    , reftab39
    , reftab40
    , reftab41
    , reftab42
    , reftab43
    , reftab44
    , reftab45
    , reftab46
    , reftab47
    , reftab48
    , reftab49
    , reftab50
    , reftab51
    , reftab52
    , reftab53
    , reftab54
    , reftab55
    , reftab56
    , reftab57
    , reftab58 ]


{-# NOINLINE reftab1 #-}
reftab1 :: [(Text,Text)]
reftab1 =
  [ ("AElig", "\x000C6"),
    ("AMP", "\x00026"),
    ("Aacute", "\x000C1"),
    ("Abreve", "\x00102"),
    ("Acirc", "\x000C2"),
    ("Acy", "\x00410"),
    ("Afr", "\x1D504"),
    ("Agrave", "\x000C0"),
    ("Alpha", "\x00391"),
    ("Amacr", "\x00100"),
    ("And", "\x02A53"),
    ("Aogon", "\x00104"),
    ("Aopf", "\x1D538"),
    ("ApplyFunction", "\x02061"),
    ("Aring", "\x000C5"),
    ("Ascr", "\x1D49C"),
    ("Assign", "\x02254"),
    ("Atilde", "\x000C3"),
    ("Auml", "\x000C4"),
    ("Backslash", "\x02216"),
    ("Barv", "\x02AE7"),
    ("Barwed", "\x02306"),
    ("Bcy", "\x00411"),
    ("Because", "\x02235"),
    ("Bernoullis", "\x0212C"),
    ("Beta", "\x00392"),
    ("Bfr", "\x1D505"),
    ("Bopf", "\x1D539"),
    ("Breve", "\x002D8"),
    ("Bscr", "\x0212C"),
    ("Bumpeq", "\x0224E"),
    ("CHcy", "\x00427"),
    ("COPY", "\x000A9"),
    ("Cacute", "\x00106"),
    ("Cap", "\x022D2"),
    ("CapitalDifferentialD", "\x02145"),
    ("Cayleys", "\x0212D") ]

{-# NOINLINE reftab2 #-}
reftab2 :: [(Text,Text)]
reftab2 =
  [ ("Ccaron", "\x0010C"),
    ("Ccedil", "\x000C7"),
    ("Ccirc", "\x00108"),
    ("Cconint", "\x02230"),
    ("Cdot", "\x0010A"),
    ("Cedilla", "\x000B8"),
    ("CenterDot", "\x000B7"),
    ("Cfr", "\x0212D"),
    ("Chi", "\x003A7"),
    ("CircleDot", "\x02299"),
    ("CircleMinus", "\x02296"),
    ("CirclePlus", "\x02295"),
    ("CircleTimes", "\x02297"),
    ("ClockwiseContourIntegral", "\x02232"),
    ("CloseCurlyDoubleQuote", "\x0201D"),
    ("CloseCurlyQuote", "\x02019"),
    ("Colon", "\x02237"),
    ("Colone", "\x02A74"),
    ("Congruent", "\x02261"),
    ("Conint", "\x0222F"),
    ("ContourIntegral", "\x0222E"),
    ("Copf", "\x02102"),
    ("Coproduct", "\x02210"),
    ("CounterClockwiseContourIntegral", "\x02233"),
    ("Cross", "\x02A2F"),
    ("Cscr", "\x1D49E"),
    ("Cup", "\x022D3"),
    ("CupCap", "\x0224D"),
    ("DD", "\x02145"),
    ("DDotrahd", "\x02911"),
    ("DJcy", "\x00402"),
    ("DScy", "\x00405"),
    ("DZcy", "\x0040F"),
    ("Dagger", "\x02021"),
    ("Darr", "\x021A1"),
    ("Dashv", "\x02AE4"),
    ("Dcaron", "\x0010E") ]

{-# NOINLINE reftab3 #-}
reftab3 :: [(Text,Text)]
reftab3 =
  [ ("Dcy", "\x00414"),
    ("Del", "\x02207"),
    ("Delta", "\x00394"),
    ("Dfr", "\x1D507"),
    ("DiacriticalAcute", "\x000B4"),
    ("DiacriticalDot", "\x002D9"),
    ("DiacriticalDoubleAcute", "\x002DD"),
    ("DiacriticalGrave", "\x00060"),
    ("DiacriticalTilde", "\x002DC"),
    ("Diamond", "\x022C4"),
    ("DifferentialD", "\x02146"),
    ("Dopf", "\x1D53B"),
    ("Dot", "\x000A8"),
    ("DotDot", "\x020DC"),
    ("DotEqual", "\x02250"),
    ("DoubleContourIntegral", "\x0222F"),
    ("DoubleDot", "\x000A8"),
    ("DoubleDownArrow", "\x021D3"),
    ("DoubleLeftArrow", "\x021D0"),
    ("DoubleLeftRightArrow", "\x021D4"),
    ("DoubleLeftTee", "\x02AE4"),
    ("DoubleLongLeftArrow", "\x027F8"),
    ("DoubleLongLeftRightArrow", "\x027FA"),
    ("DoubleLongRightArrow", "\x027F9"),
    ("DoubleRightArrow", "\x021D2"),
    ("DoubleRightTee", "\x022A8"),
    ("DoubleUpArrow", "\x021D1"),
    ("DoubleUpDownArrow", "\x021D5"),
    ("DoubleVerticalBar", "\x02225"),
    ("DownArrow", "\x02193"),
    ("DownArrowBar", "\x02913"),
    ("DownArrowUpArrow", "\x021F5"),
    ("DownBreve", "\x00311"),
    ("DownLeftRightVector", "\x02950"),
    ("DownLeftTeeVector", "\x0295E"),
    ("DownLeftVector", "\x021BD"),
    ("DownLeftVectorBar", "\x02956") ]

{-# NOINLINE reftab4 #-}
reftab4 :: [(Text,Text)]
reftab4 =
  [ ("DownRightTeeVector", "\x0295F"),
    ("DownRightVector", "\x021C1"),
    ("DownRightVectorBar", "\x02957"),
    ("DownTee", "\x022A4"),
    ("DownTeeArrow", "\x021A7"),
    ("Downarrow", "\x021D3"),
    ("Dscr", "\x1D49F"),
    ("Dstrok", "\x00110"),
    ("ENG", "\x0014A"),
    ("ETH", "\x000D0"),
    ("Eacute", "\x000C9"),
    ("Ecaron", "\x0011A"),
    ("Ecirc", "\x000CA"),
    ("Ecy", "\x0042D"),
    ("Edot", "\x00116"),
    ("Efr", "\x1D508"),
    ("Egrave", "\x000C8"),
    ("Element", "\x02208"),
    ("Emacr", "\x00112"),
    ("EmptySmallSquare", "\x025FB"),
    ("EmptyVerySmallSquare", "\x025AB"),
    ("Eogon", "\x00118"),
    ("Eopf", "\x1D53C"),
    ("Epsilon", "\x00395"),
    ("Equal", "\x02A75"),
    ("EqualTilde", "\x02242"),
    ("Equilibrium", "\x021CC"),
    ("Escr", "\x02130"),
    ("Esim", "\x02A73"),
    ("Eta", "\x00397"),
    ("Euml", "\x000CB"),
    ("Exists", "\x02203"),
    ("ExponentialE", "\x02147"),
    ("Fcy", "\x00424"),
    ("Ffr", "\x1D509"),
    ("FilledSmallSquare", "\x025FC"),
    ("FilledVerySmallSquare", "\x025AA") ]

{-# NOINLINE reftab5 #-}
reftab5 :: [(Text,Text)]
reftab5 =
  [ ("Fopf", "\x1D53D"),
    ("ForAll", "\x02200"),
    ("Fouriertrf", "\x02131"),
    ("Fscr", "\x02131"),
    ("GJcy", "\x00403"),
    ("GT", "\x0003E"),
    ("Gamma", "\x00393"),
    ("Gammad", "\x003DC"),
    ("Gbreve", "\x0011E"),
    ("Gcedil", "\x00122"),
    ("Gcirc", "\x0011C"),
    ("Gcy", "\x00413"),
    ("Gdot", "\x00120"),
    ("Gfr", "\x1D50A"),
    ("Gg", "\x022D9"),
    ("Gopf", "\x1D53E"),
    ("GreaterEqual", "\x02265"),
    ("GreaterEqualLess", "\x022DB"),
    ("GreaterFullEqual", "\x02267"),
    ("GreaterGreater", "\x02AA2"),
    ("GreaterLess", "\x02277"),
    ("GreaterSlantEqual", "\x02A7E"),
    ("GreaterTilde", "\x02273"),
    ("Gscr", "\x1D4A2"),
    ("Gt", "\x0226B"),
    ("HARDcy", "\x0042A"),
    ("Hacek", "\x002C7"),
    ("Hat", "\x0005E"),
    ("Hcirc", "\x00124"),
    ("Hfr", "\x0210C"),
    ("HilbertSpace", "\x0210B"),
    ("Hopf", "\x0210D"),
    ("HorizontalLine", "\x02500"),
    ("Hscr", "\x0210B"),
    ("Hstrok", "\x00126"),
    ("HumpDownHump", "\x0224E"),
    ("HumpEqual", "\x0224F") ]

{-# NOINLINE reftab6 #-}
reftab6 :: [(Text,Text)]
reftab6 =
  [ ("IEcy", "\x00415"),
    ("IJlig", "\x00132"),
    ("IOcy", "\x00401"),
    ("Iacute", "\x000CD"),
    ("Icirc", "\x000CE"),
    ("Icy", "\x00418"),
    ("Idot", "\x00130"),
    ("Ifr", "\x02111"),
    ("Igrave", "\x000CC"),
    ("Im", "\x02111"),
    ("Imacr", "\x0012A"),
    ("ImaginaryI", "\x02148"),
    ("Implies", "\x021D2"),
    ("Int", "\x0222C"),
    ("Integral", "\x0222B"),
    ("Intersection", "\x022C2"),
    ("InvisibleComma", "\x02063"),
    ("InvisibleTimes", "\x02062"),
    ("Iogon", "\x0012E"),
    ("Iopf", "\x1D540"),
    ("Iota", "\x00399"),
    ("Iscr", "\x02110"),
    ("Itilde", "\x00128"),
    ("Iukcy", "\x00406"),
    ("Iuml", "\x000CF"),
    ("Jcirc", "\x00134"),
    ("Jcy", "\x00419"),
    ("Jfr", "\x1D50D"),
    ("Jopf", "\x1D541"),
    ("Jscr", "\x1D4A5"),
    ("Jsercy", "\x00408"),
    ("Jukcy", "\x00404"),
    ("KHcy", "\x00425"),
    ("KJcy", "\x0040C"),
    ("Kappa", "\x0039A"),
    ("Kcedil", "\x00136"),
    ("Kcy", "\x0041A") ]

{-# NOINLINE reftab7 #-}
reftab7 :: [(Text,Text)]
reftab7 =
  [ ("Kfr", "\x1D50E"),
    ("Kopf", "\x1D542"),
    ("Kscr", "\x1D4A6"),
    ("LJcy", "\x00409"),
    ("LT", "\x0003C"),
    ("Lacute", "\x00139"),
    ("Lambda", "\x0039B"),
    ("Lang", "\x027EA"),
    ("Laplacetrf", "\x02112"),
    ("Larr", "\x0219E"),
    ("Lcaron", "\x0013D"),
    ("Lcedil", "\x0013B"),
    ("Lcy", "\x0041B"),
    ("LeftAngleBracket", "\x027E8"),
    ("LeftArrow", "\x02190"),
    ("LeftArrowBar", "\x021E4"),
    ("LeftArrowRightArrow", "\x021C6"),
    ("LeftCeiling", "\x02308"),
    ("LeftDoubleBracket", "\x027E6"),
    ("LeftDownTeeVector", "\x02961"),
    ("LeftDownVector", "\x021C3"),
    ("LeftDownVectorBar", "\x02959"),
    ("LeftFloor", "\x0230A"),
    ("LeftRightArrow", "\x02194"),
    ("LeftRightVector", "\x0294E"),
    ("LeftTee", "\x022A3"),
    ("LeftTeeArrow", "\x021A4"),
    ("LeftTeeVector", "\x0295A"),
    ("LeftTriangle", "\x022B2"),
    ("LeftTriangleBar", "\x029CF"),
    ("LeftTriangleEqual", "\x022B4"),
    ("LeftUpDownVector", "\x02951"),
    ("LeftUpTeeVector", "\x02960"),
    ("LeftUpVector", "\x021BF"),
    ("LeftUpVectorBar", "\x02958"),
    ("LeftVector", "\x021BC"),
    ("LeftVectorBar", "\x02952") ]

{-# NOINLINE reftab8 #-}
reftab8 :: [(Text,Text)]
reftab8 =
  [ ("Leftarrow", "\x021D0"),
    ("Leftrightarrow", "\x021D4"),
    ("LessEqualGreater", "\x022DA"),
    ("LessFullEqual", "\x02266"),
    ("LessGreater", "\x02276"),
    ("LessLess", "\x02AA1"),
    ("LessSlantEqual", "\x02A7D"),
    ("LessTilde", "\x02272"),
    ("Lfr", "\x1D50F"),
    ("Ll", "\x022D8"),
    ("Lleftarrow", "\x021DA"),
    ("Lmidot", "\x0013F"),
    ("LongLeftArrow", "\x027F5"),
    ("LongLeftRightArrow", "\x027F7"),
    ("LongRightArrow", "\x027F6"),
    ("Longleftarrow", "\x027F8"),
    ("Longleftrightarrow", "\x027FA"),
    ("Longrightarrow", "\x027F9"),
    ("Lopf", "\x1D543"),
    ("LowerLeftArrow", "\x02199"),
    ("LowerRightArrow", "\x02198"),
    ("Lscr", "\x02112"),
    ("Lsh", "\x021B0"),
    ("Lstrok", "\x00141"),
    ("Lt", "\x0226A"),
    ("Map", "\x02905"),
    ("Mcy", "\x0041C"),
    ("MediumSpace", "\x0205F"),
    ("Mellintrf", "\x02133"),
    ("Mfr", "\x1D510"),
    ("MinusPlus", "\x02213"),
    ("Mopf", "\x1D544"),
    ("Mscr", "\x02133"),
    ("Mu", "\x0039C"),
    ("NJcy", "\x0040A"),
    ("Nacute", "\x00143"),
    ("Ncaron", "\x00147") ]

{-# NOINLINE reftab9 #-}
reftab9 :: [(Text,Text)]
reftab9 =
  [ ("Ncedil", "\x00145"),
    ("Ncy", "\x0041D"),
    ("NegativeMediumSpace", "\x0200B"),
    ("NegativeThickSpace", "\x0200B"),
    ("NegativeThinSpace", "\x0200B"),
    ("NegativeVeryThinSpace", "\x0200B"),
    ("NestedGreaterGreater", "\x0226B"),
    ("NestedLessLess", "\x0226A"),
    ("NewLine", "\x0000A"),
    ("Nfr", "\x1D511"),
    ("NoBreak", "\x02060"),
    ("NonBreakingSpace", "\x000A0"),
    ("Nopf", "\x02115"),
    ("Not", "\x02AEC"),
    ("NotCongruent", "\x02262"),
    ("NotCupCap", "\x0226D"),
    ("NotDoubleVerticalBar", "\x02226"),
    ("NotElement", "\x02209"),
    ("NotEqual", "\x02260"),
    ("NotEqualTilde", "\x02242\x00338"),
    ("NotExists", "\x02204"),
    ("NotGreater", "\x0226F"),
    ("NotGreaterEqual", "\x02271"),
    ("NotGreaterFullEqual", "\x02267\x00338"),
    ("NotGreaterGreater", "\x0226B\x00338"),
    ("NotGreaterLess", "\x02279"),
    ("NotGreaterSlantEqual", "\x02A7E\x00338"),
    ("NotGreaterTilde", "\x02275"),
    ("NotHumpDownHump", "\x0224E\x00338"),
    ("NotHumpEqual", "\x0224F\x00338"),
    ("NotLeftTriangle", "\x022EA"),
    ("NotLeftTriangleBar", "\x029CF\x00338"),
    ("NotLeftTriangleEqual", "\x022EC"),
    ("NotLess", "\x0226E"),
    ("NotLessEqual", "\x02270"),
    ("NotLessGreater", "\x02278"),
    ("NotLessLess", "\x0226A\x00338") ]

{-# NOINLINE reftab10 #-}
reftab10 :: [(Text,Text)]
reftab10 =
  [ ("NotLessSlantEqual", "\x02A7D\x00338"),
    ("NotLessTilde", "\x02274"),
    ("NotNestedGreaterGreater", "\x02AA2\x00338"),
    ("NotNestedLessLess", "\x02AA1\x00338"),
    ("NotPrecedes", "\x02280"),
    ("NotPrecedesEqual", "\x02AAF\x00338"),
    ("NotPrecedesSlantEqual", "\x022E0"),
    ("NotReverseElement", "\x0220C"),
    ("NotRightTriangle", "\x022EB"),
    ("NotRightTriangleBar", "\x029D0\x00338"),
    ("NotRightTriangleEqual", "\x022ED"),
    ("NotSquareSubset", "\x0228F\x00338"),
    ("NotSquareSubsetEqual", "\x022E2"),
    ("NotSquareSuperset", "\x02290\x00338"),
    ("NotSquareSupersetEqual", "\x022E3"),
    ("NotSubset", "\x02282\x020D2"),
    ("NotSubsetEqual", "\x02288"),
    ("NotSucceeds", "\x02281"),
    ("NotSucceedsEqual", "\x02AB0\x00338"),
    ("NotSucceedsSlantEqual", "\x022E1"),
    ("NotSucceedsTilde", "\x0227F\x00338"),
    ("NotSuperset", "\x02283\x020D2"),
    ("NotSupersetEqual", "\x02289"),
    ("NotTilde", "\x02241"),
    ("NotTildeEqual", "\x02244"),
    ("NotTildeFullEqual", "\x02247"),
    ("NotTildeTilde", "\x02249"),
    ("NotVerticalBar", "\x02224"),
    ("Nscr", "\x1D4A9"),
    ("Ntilde", "\x000D1"),
    ("Nu", "\x0039D"),
    ("OElig", "\x00152"),
    ("Oacute", "\x000D3"),
    ("Ocirc", "\x000D4"),
    ("Ocy", "\x0041E"),
    ("Odblac", "\x00150"),
    ("Ofr", "\x1D512") ]

{-# NOINLINE reftab11 #-}
reftab11 :: [(Text,Text)]
reftab11 =
  [ ("Ograve", "\x000D2"),
    ("Omacr", "\x0014C"),
    ("Omega", "\x003A9"),
    ("Omicron", "\x0039F"),
    ("Oopf", "\x1D546"),
    ("OpenCurlyDoubleQuote", "\x0201C"),
    ("OpenCurlyQuote", "\x02018"),
    ("Or", "\x02A54"),
    ("Oscr", "\x1D4AA"),
    ("Oslash", "\x000D8"),
    ("Otilde", "\x000D5"),
    ("Otimes", "\x02A37"),
    ("Ouml", "\x000D6"),
    ("OverBar", "\x0203E"),
    ("OverBrace", "\x023DE"),
    ("OverBracket", "\x023B4"),
    ("OverParenthesis", "\x023DC"),
    ("PartialD", "\x02202"),
    ("Pcy", "\x0041F"),
    ("Pfr", "\x1D513"),
    ("Phi", "\x003A6"),
    ("Pi", "\x003A0"),
    ("PlusMinus", "\x000B1"),
    ("Poincareplane", "\x0210C"),
    ("Popf", "\x02119"),
    ("Pr", "\x02ABB"),
    ("Precedes", "\x0227A"),
    ("PrecedesEqual", "\x02AAF"),
    ("PrecedesSlantEqual", "\x0227C"),
    ("PrecedesTilde", "\x0227E"),
    ("Prime", "\x02033"),
    ("Product", "\x0220F"),
    ("Proportion", "\x02237"),
    ("Proportional", "\x0221D"),
    ("Pscr", "\x1D4AB"),
    ("Psi", "\x003A8"),
    ("QUOT", "\x00022") ]

{-# NOINLINE reftab12 #-}
reftab12 :: [(Text,Text)]
reftab12 =
  [ ("Qfr", "\x1D514"),
    ("Qopf", "\x0211A"),
    ("Qscr", "\x1D4AC"),
    ("RBarr", "\x02910"),
    ("REG", "\x000AE"),
    ("Racute", "\x00154"),
    ("Rang", "\x027EB"),
    ("Rarr", "\x021A0"),
    ("Rarrtl", "\x02916"),
    ("Rcaron", "\x00158"),
    ("Rcedil", "\x00156"),
    ("Rcy", "\x00420"),
    ("Re", "\x0211C"),
    ("ReverseElement", "\x0220B"),
    ("ReverseEquilibrium", "\x021CB"),
    ("ReverseUpEquilibrium", "\x0296F"),
    ("Rfr", "\x0211C"),
    ("Rho", "\x003A1"),
    ("RightAngleBracket", "\x027E9"),
    ("RightArrow", "\x02192"),
    ("RightArrowBar", "\x021E5"),
    ("RightArrowLeftArrow", "\x021C4"),
    ("RightCeiling", "\x02309"),
    ("RightDoubleBracket", "\x027E7"),
    ("RightDownTeeVector", "\x0295D"),
    ("RightDownVector", "\x021C2"),
    ("RightDownVectorBar", "\x02955"),
    ("RightFloor", "\x0230B"),
    ("RightTee", "\x022A2"),
    ("RightTeeArrow", "\x021A6"),
    ("RightTeeVector", "\x0295B"),
    ("RightTriangle", "\x022B3"),
    ("RightTriangleBar", "\x029D0"),
    ("RightTriangleEqual", "\x022B5"),
    ("RightUpDownVector", "\x0294F"),
    ("RightUpTeeVector", "\x0295C"),
    ("RightUpVector", "\x021BE") ]

{-# NOINLINE reftab13 #-}
reftab13 :: [(Text,Text)]
reftab13 =
  [ ("RightUpVectorBar", "\x02954"),
    ("RightVector", "\x021C0"),
    ("RightVectorBar", "\x02953"),
    ("Rightarrow", "\x021D2"),
    ("Ropf", "\x0211D"),
    ("RoundImplies", "\x02970"),
    ("Rrightarrow", "\x021DB"),
    ("Rscr", "\x0211B"),
    ("Rsh", "\x021B1"),
    ("RuleDelayed", "\x029F4"),
    ("SHCHcy", "\x00429"),
    ("SHcy", "\x00428"),
    ("SOFTcy", "\x0042C"),
    ("Sacute", "\x0015A"),
    ("Sc", "\x02ABC"),
    ("Scaron", "\x00160"),
    ("Scedil", "\x0015E"),
    ("Scirc", "\x0015C"),
    ("Scy", "\x00421"),
    ("Sfr", "\x1D516"),
    ("ShortDownArrow", "\x02193"),
    ("ShortLeftArrow", "\x02190"),
    ("ShortRightArrow", "\x02192"),
    ("ShortUpArrow", "\x02191"),
    ("Sigma", "\x003A3"),
    ("SmallCircle", "\x02218"),
    ("Sopf", "\x1D54A"),
    ("Sqrt", "\x0221A"),
    ("Square", "\x025A1"),
    ("SquareIntersection", "\x02293"),
    ("SquareSubset", "\x0228F"),
    ("SquareSubsetEqual", "\x02291"),
    ("SquareSuperset", "\x02290"),
    ("SquareSupersetEqual", "\x02292"),
    ("SquareUnion", "\x02294"),
    ("Sscr", "\x1D4AE"),
    ("Star", "\x022C6") ]

{-# NOINLINE reftab14 #-}
reftab14 :: [(Text,Text)]
reftab14 =
  [ ("Sub", "\x022D0"),
    ("Subset", "\x022D0"),
    ("SubsetEqual", "\x02286"),
    ("Succeeds", "\x0227B"),
    ("SucceedsEqual", "\x02AB0"),
    ("SucceedsSlantEqual", "\x0227D"),
    ("SucceedsTilde", "\x0227F"),
    ("SuchThat", "\x0220B"),
    ("Sum", "\x02211"),
    ("Sup", "\x022D1"),
    ("Superset", "\x02283"),
    ("SupersetEqual", "\x02287"),
    ("Supset", "\x022D1"),
    ("THORN", "\x000DE"),
    ("TRADE", "\x02122"),
    ("TSHcy", "\x0040B"),
    ("TScy", "\x00426"),
    ("Tab", "\x00009"),
    ("Tau", "\x003A4"),
    ("Tcaron", "\x00164"),
    ("Tcedil", "\x00162"),
    ("Tcy", "\x00422"),
    ("Tfr", "\x1D517"),
    ("Therefore", "\x02234"),
    ("Theta", "\x00398"),
    ("ThickSpace", "\x0205F\x0200A"),
    ("ThinSpace", "\x02009"),
    ("Tilde", "\x0223C"),
    ("TildeEqual", "\x02243"),
    ("TildeFullEqual", "\x02245"),
    ("TildeTilde", "\x02248"),
    ("Topf", "\x1D54B"),
    ("TripleDot", "\x020DB"),
    ("Tscr", "\x1D4AF"),
    ("Tstrok", "\x00166"),
    ("Uacute", "\x000DA"),
    ("Uarr", "\x0219F") ]

{-# NOINLINE reftab15 #-}
reftab15 :: [(Text,Text)]
reftab15 =
  [ ("Uarrocir", "\x02949"),
    ("Ubrcy", "\x0040E"),
    ("Ubreve", "\x0016C"),
    ("Ucirc", "\x000DB"),
    ("Ucy", "\x00423"),
    ("Udblac", "\x00170"),
    ("Ufr", "\x1D518"),
    ("Ugrave", "\x000D9"),
    ("Umacr", "\x0016A"),
    ("UnderBar", "\x0005F"),
    ("UnderBrace", "\x023DF"),
    ("UnderBracket", "\x023B5"),
    ("UnderParenthesis", "\x023DD"),
    ("Union", "\x022C3"),
    ("UnionPlus", "\x0228E"),
    ("Uogon", "\x00172"),
    ("Uopf", "\x1D54C"),
    ("UpArrow", "\x02191"),
    ("UpArrowBar", "\x02912"),
    ("UpArrowDownArrow", "\x021C5"),
    ("UpDownArrow", "\x02195"),
    ("UpEquilibrium", "\x0296E"),
    ("UpTee", "\x022A5"),
    ("UpTeeArrow", "\x021A5"),
    ("Uparrow", "\x021D1"),
    ("Updownarrow", "\x021D5"),
    ("UpperLeftArrow", "\x02196"),
    ("UpperRightArrow", "\x02197"),
    ("Upsi", "\x003D2"),
    ("Upsilon", "\x003A5"),
    ("Uring", "\x0016E"),
    ("Uscr", "\x1D4B0"),
    ("Utilde", "\x00168"),
    ("Uuml", "\x000DC"),
    ("VDash", "\x022AB"),
    ("Vbar", "\x02AEB"),
    ("Vcy", "\x00412") ]

{-# NOINLINE reftab16 #-}
reftab16 :: [(Text,Text)]
reftab16 =
  [ ("Vdash", "\x022A9"),
    ("Vdashl", "\x02AE6"),
    ("Vee", "\x022C1"),
    ("Verbar", "\x02016"),
    ("Vert", "\x02016"),
    ("VerticalBar", "\x02223"),
    ("VerticalLine", "\x0007C"),
    ("VerticalSeparator", "\x02758"),
    ("VerticalTilde", "\x02240"),
    ("VeryThinSpace", "\x0200A"),
    ("Vfr", "\x1D519"),
    ("Vopf", "\x1D54D"),
    ("Vscr", "\x1D4B1"),
    ("Vvdash", "\x022AA"),
    ("Wcirc", "\x00174"),
    ("Wedge", "\x022C0"),
    ("Wfr", "\x1D51A"),
    ("Wopf", "\x1D54E"),
    ("Wscr", "\x1D4B2"),
    ("Xfr", "\x1D51B"),
    ("Xi", "\x0039E"),
    ("Xopf", "\x1D54F"),
    ("Xscr", "\x1D4B3"),
    ("YAcy", "\x0042F"),
    ("YIcy", "\x00407"),
    ("YUcy", "\x0042E"),
    ("Yacute", "\x000DD"),
    ("Ycirc", "\x00176"),
    ("Ycy", "\x0042B"),
    ("Yfr", "\x1D51C"),
    ("Yopf", "\x1D550"),
    ("Yscr", "\x1D4B4"),
    ("Yuml", "\x00178"),
    ("ZHcy", "\x00416"),
    ("Zacute", "\x00179"),
    ("Zcaron", "\x0017D"),
    ("Zcy", "\x00417") ]

{-# NOINLINE reftab17 #-}
reftab17 :: [(Text,Text)]
reftab17 =
  [ ("Zdot", "\x0017B"),
    ("ZeroWidthSpace", "\x0200B"),
    ("Zeta", "\x00396"),
    ("Zfr", "\x02128"),
    ("Zopf", "\x02124"),
    ("Zscr", "\x1D4B5"),
    ("aacute", "\x000E1"),
    ("abreve", "\x00103"),
    ("ac", "\x0223E"),
    ("acE", "\x0223E\x00333"),
    ("acd", "\x0223F"),
    ("acirc", "\x000E2"),
    ("acute", "\x000B4"),
    ("acy", "\x00430"),
    ("aelig", "\x000E6"),
    ("af", "\x02061"),
    ("afr", "\x1D51E"),
    ("agrave", "\x000E0"),
    ("alefsym", "\x02135"),
    ("aleph", "\x02135"),
    ("alpha", "\x003B1"),
    ("amacr", "\x00101"),
    ("amalg", "\x02A3F"),
    ("amp", "\x00026"),
    ("and", "\x02227"),
    ("andand", "\x02A55"),
    ("andd", "\x02A5C"),
    ("andslope", "\x02A58"),
    ("andv", "\x02A5A"),
    ("ang", "\x02220"),
    ("ange", "\x029A4"),
    ("angle", "\x02220"),
    ("angmsd", "\x02221"),
    ("angmsdaa", "\x029A8"),
    ("angmsdab", "\x029A9"),
    ("angmsdac", "\x029AA"),
    ("angmsdad", "\x029AB") ]

{-# NOINLINE reftab18 #-}
reftab18 :: [(Text,Text)]
reftab18 =
  [ ("angmsdae", "\x029AC"),
    ("angmsdaf", "\x029AD"),
    ("angmsdag", "\x029AE"),
    ("angmsdah", "\x029AF"),
    ("angrt", "\x0221F"),
    ("angrtvb", "\x022BE"),
    ("angrtvbd", "\x0299D"),
    ("angsph", "\x02222"),
    ("angst", "\x000C5"),
    ("angzarr", "\x0237C"),
    ("aogon", "\x00105"),
    ("aopf", "\x1D552"),
    ("ap", "\x02248"),
    ("apE", "\x02A70"),
    ("apacir", "\x02A6F"),
    ("ape", "\x0224A"),
    ("apid", "\x0224B"),
    ("apos", "\x00027"),
    ("approx", "\x02248"),
    ("approxeq", "\x0224A"),
    ("aring", "\x000E5"),
    ("ascr", "\x1D4B6"),
    ("ast", "\x0002A"),
    ("asymp", "\x02248"),
    ("asympeq", "\x0224D"),
    ("atilde", "\x000E3"),
    ("auml", "\x000E4"),
    ("awconint", "\x02233"),
    ("awint", "\x02A11"),
    ("bNot", "\x02AED"),
    ("backcong", "\x0224C"),
    ("backepsilon", "\x003F6"),
    ("backprime", "\x02035"),
    ("backsim", "\x0223D"),
    ("backsimeq", "\x022CD"),
    ("barvee", "\x022BD"),
    ("barwed", "\x02305") ]

{-# NOINLINE reftab19 #-}
reftab19 :: [(Text,Text)]
reftab19 =
  [ ("barwedge", "\x02305"),
    ("bbrk", "\x023B5"),
    ("bbrktbrk", "\x023B6"),
    ("bcong", "\x0224C"),
    ("bcy", "\x00431"),
    ("bdquo", "\x0201E"),
    ("becaus", "\x02235"),
    ("because", "\x02235"),
    ("bemptyv", "\x029B0"),
    ("bepsi", "\x003F6"),
    ("bernou", "\x0212C"),
    ("beta", "\x003B2"),
    ("beth", "\x02136"),
    ("between", "\x0226C"),
    ("bfr", "\x1D51F"),
    ("bigcap", "\x022C2"),
    ("bigcirc", "\x025EF"),
    ("bigcup", "\x022C3"),
    ("bigodot", "\x02A00"),
    ("bigoplus", "\x02A01"),
    ("bigotimes", "\x02A02"),
    ("bigsqcup", "\x02A06"),
    ("bigstar", "\x02605"),
    ("bigtriangledown", "\x025BD"),
    ("bigtriangleup", "\x025B3"),
    ("biguplus", "\x02A04"),
    ("bigvee", "\x022C1"),
    ("bigwedge", "\x022C0"),
    ("bkarow", "\x0290D"),
    ("blacklozenge", "\x029EB"),
    ("blacksquare", "\x025AA"),
    ("blacktriangle", "\x025B4"),
    ("blacktriangledown", "\x025BE"),
    ("blacktriangleleft", "\x025C2"),
    ("blacktriangleright", "\x025B8"),
    ("blank", "\x02423"),
    ("blk12", "\x02592") ]

{-# NOINLINE reftab20 #-}
reftab20 :: [(Text,Text)]
reftab20 =
  [ ("blk14", "\x02591"),
    ("blk34", "\x02593"),
    ("block", "\x02588"),
    ("bne", "\x0003D\x020E5"),
    ("bnequiv", "\x02261\x020E5"),
    ("bnot", "\x02310"),
    ("bopf", "\x1D553"),
    ("bot", "\x022A5"),
    ("bottom", "\x022A5"),
    ("bowtie", "\x022C8"),
    ("boxDL", "\x02557"),
    ("boxDR", "\x02554"),
    ("boxDl", "\x02556"),
    ("boxDr", "\x02553"),
    ("boxH", "\x02550"),
    ("boxHD", "\x02566"),
    ("boxHU", "\x02569"),
    ("boxHd", "\x02564"),
    ("boxHu", "\x02567"),
    ("boxUL", "\x0255D"),
    ("boxUR", "\x0255A"),
    ("boxUl", "\x0255C"),
    ("boxUr", "\x02559"),
    ("boxV", "\x02551"),
    ("boxVH", "\x0256C"),
    ("boxVL", "\x02563"),
    ("boxVR", "\x02560"),
    ("boxVh", "\x0256B"),
    ("boxVl", "\x02562"),
    ("boxVr", "\x0255F"),
    ("boxbox", "\x029C9"),
    ("boxdL", "\x02555"),
    ("boxdR", "\x02552"),
    ("boxdl", "\x02510"),
    ("boxdr", "\x0250C"),
    ("boxh", "\x02500"),
    ("boxhD", "\x02565") ]

{-# NOINLINE reftab21 #-}
reftab21 :: [(Text,Text)]
reftab21 =
  [ ("boxhU", "\x02568"),
    ("boxhd", "\x0252C"),
    ("boxhu", "\x02534"),
    ("boxminus", "\x0229F"),
    ("boxplus", "\x0229E"),
    ("boxtimes", "\x022A0"),
    ("boxuL", "\x0255B"),
    ("boxuR", "\x02558"),
    ("boxul", "\x02518"),
    ("boxur", "\x02514"),
    ("boxv", "\x02502"),
    ("boxvH", "\x0256A"),
    ("boxvL", "\x02561"),
    ("boxvR", "\x0255E"),
    ("boxvh", "\x0253C"),
    ("boxvl", "\x02524"),
    ("boxvr", "\x0251C"),
    ("bprime", "\x02035"),
    ("breve", "\x002D8"),
    ("brvbar", "\x000A6"),
    ("bscr", "\x1D4B7"),
    ("bsemi", "\x0204F"),
    ("bsim", "\x0223D"),
    ("bsime", "\x022CD"),
    ("bsol", "\x0005C"),
    ("bsolb", "\x029C5"),
    ("bsolhsub", "\x027C8"),
    ("bull", "\x02022"),
    ("bullet", "\x02022"),
    ("bump", "\x0224E"),
    ("bumpE", "\x02AAE"),
    ("bumpe", "\x0224F"),
    ("bumpeq", "\x0224F"),
    ("cacute", "\x00107"),
    ("cap", "\x02229"),
    ("capand", "\x02A44"),
    ("capbrcup", "\x02A49") ]

{-# NOINLINE reftab22 #-}
reftab22 :: [(Text,Text)]
reftab22 =
  [ ("capcap", "\x02A4B"),
    ("capcup", "\x02A47"),
    ("capdot", "\x02A40"),
    ("caps", "\x02229\x0FE00"),
    ("caret", "\x02041"),
    ("caron", "\x002C7"),
    ("ccaps", "\x02A4D"),
    ("ccaron", "\x0010D"),
    ("ccedil", "\x000E7"),
    ("ccirc", "\x00109"),
    ("ccups", "\x02A4C"),
    ("ccupssm", "\x02A50"),
    ("cdot", "\x0010B"),
    ("cedil", "\x000B8"),
    ("cemptyv", "\x029B2"),
    ("cent", "\x000A2"),
    ("centerdot", "\x000B7"),
    ("cfr", "\x1D520"),
    ("chcy", "\x00447"),
    ("check", "\x02713"),
    ("checkmark", "\x02713"),
    ("chi", "\x003C7"),
    ("cir", "\x025CB"),
    ("cirE", "\x029C3"),
    ("circ", "\x002C6"),
    ("circeq", "\x02257"),
    ("circlearrowleft", "\x021BA"),
    ("circlearrowright", "\x021BB"),
    ("circledR", "\x000AE"),
    ("circledS", "\x024C8"),
    ("circledast", "\x0229B"),
    ("circledcirc", "\x0229A"),
    ("circleddash", "\x0229D"),
    ("cire", "\x02257"),
    ("cirfnint", "\x02A10"),
    ("cirmid", "\x02AEF"),
    ("cirscir", "\x029C2") ]

{-# NOINLINE reftab23 #-}
reftab23 :: [(Text,Text)]
reftab23 =
  [ ("clubs", "\x02663"),
    ("clubsuit", "\x02663"),
    ("colon", "\x0003A"),
    ("colone", "\x02254"),
    ("coloneq", "\x02254"),
    ("comma", "\x0002C"),
    ("commat", "\x00040"),
    ("comp", "\x02201"),
    ("compfn", "\x02218"),
    ("complement", "\x02201"),
    ("complexes", "\x02102"),
    ("cong", "\x02245"),
    ("congdot", "\x02A6D"),
    ("conint", "\x0222E"),
    ("copf", "\x1D554"),
    ("coprod", "\x02210"),
    ("copy", "\x000A9"),
    ("copysr", "\x02117"),
    ("crarr", "\x021B5"),
    ("cross", "\x02717"),
    ("cscr", "\x1D4B8"),
    ("csub", "\x02ACF"),
    ("csube", "\x02AD1"),
    ("csup", "\x02AD0"),
    ("csupe", "\x02AD2"),
    ("ctdot", "\x022EF"),
    ("cudarrl", "\x02938"),
    ("cudarrr", "\x02935"),
    ("cuepr", "\x022DE"),
    ("cuesc", "\x022DF"),
    ("cularr", "\x021B6"),
    ("cularrp", "\x0293D"),
    ("cup", "\x0222A"),
    ("cupbrcap", "\x02A48"),
    ("cupcap", "\x02A46"),
    ("cupcup", "\x02A4A"),
    ("cupdot", "\x0228D") ]

{-# NOINLINE reftab24 #-}
reftab24 :: [(Text,Text)]
reftab24 =
  [ ("cupor", "\x02A45"),
    ("cups", "\x0222A\x0FE00"),
    ("curarr", "\x021B7"),
    ("curarrm", "\x0293C"),
    ("curlyeqprec", "\x022DE"),
    ("curlyeqsucc", "\x022DF"),
    ("curlyvee", "\x022CE"),
    ("curlywedge", "\x022CF"),
    ("curren", "\x000A4"),
    ("curvearrowleft", "\x021B6"),
    ("curvearrowright", "\x021B7"),
    ("cuvee", "\x022CE"),
    ("cuwed", "\x022CF"),
    ("cwconint", "\x02232"),
    ("cwint", "\x02231"),
    ("cylcty", "\x0232D"),
    ("dArr", "\x021D3"),
    ("dHar", "\x02965"),
    ("dagger", "\x02020"),
    ("daleth", "\x02138"),
    ("darr", "\x02193"),
    ("dash", "\x02010"),
    ("dashv", "\x022A3"),
    ("dbkarow", "\x0290F"),
    ("dblac", "\x002DD"),
    ("dcaron", "\x0010F"),
    ("dcy", "\x00434"),
    ("dd", "\x02146"),
    ("ddagger", "\x02021"),
    ("ddarr", "\x021CA"),
    ("ddotseq", "\x02A77"),
    ("deg", "\x000B0"),
    ("delta", "\x003B4"),
    ("demptyv", "\x029B1"),
    ("dfisht", "\x0297F"),
    ("dfr", "\x1D521"),
    ("dharl", "\x021C3") ]

{-# NOINLINE reftab25 #-}
reftab25 :: [(Text,Text)]
reftab25 =
  [ ("dharr", "\x021C2"),
    ("diam", "\x022C4"),
    ("diamond", "\x022C4"),
    ("diamondsuit", "\x02666"),
    ("diams", "\x02666"),
    ("die", "\x000A8"),
    ("digamma", "\x003DD"),
    ("disin", "\x022F2"),
    ("div", "\x000F7"),
    ("divide", "\x000F7"),
    ("divideontimes", "\x022C7"),
    ("divonx", "\x022C7"),
    ("djcy", "\x00452"),
    ("dlcorn", "\x0231E"),
    ("dlcrop", "\x0230D"),
    ("dollar", "\x00024"),
    ("dopf", "\x1D555"),
    ("dot", "\x002D9"),
    ("doteq", "\x02250"),
    ("doteqdot", "\x02251"),
    ("dotminus", "\x02238"),
    ("dotplus", "\x02214"),
    ("dotsquare", "\x022A1"),
    ("doublebarwedge", "\x02306"),
    ("downarrow", "\x02193"),
    ("downdownarrows", "\x021CA"),
    ("downharpoonleft", "\x021C3"),
    ("downharpoonright", "\x021C2"),
    ("drbkarow", "\x02910"),
    ("drcorn", "\x0231F"),
    ("drcrop", "\x0230C"),
    ("dscr", "\x1D4B9"),
    ("dscy", "\x00455"),
    ("dsol", "\x029F6"),
    ("dstrok", "\x00111"),
    ("dtdot", "\x022F1"),
    ("dtri", "\x025BF") ]

{-# NOINLINE reftab26 #-}
reftab26 :: [(Text,Text)]
reftab26 =
  [ ("dtrif", "\x025BE"),
    ("duarr", "\x021F5"),
    ("duhar", "\x0296F"),
    ("dwangle", "\x029A6"),
    ("dzcy", "\x0045F"),
    ("dzigrarr", "\x027FF"),
    ("eDDot", "\x02A77"),
    ("eDot", "\x02251"),
    ("eacute", "\x000E9"),
    ("easter", "\x02A6E"),
    ("ecaron", "\x0011B"),
    ("ecir", "\x02256"),
    ("ecirc", "\x000EA"),
    ("ecolon", "\x02255"),
    ("ecy", "\x0044D"),
    ("edot", "\x00117"),
    ("ee", "\x02147"),
    ("efDot", "\x02252"),
    ("efr", "\x1D522"),
    ("eg", "\x02A9A"),
    ("egrave", "\x000E8"),
    ("egs", "\x02A96"),
    ("egsdot", "\x02A98"),
    ("el", "\x02A99"),
    ("elinters", "\x023E7"),
    ("ell", "\x02113"),
    ("els", "\x02A95"),
    ("elsdot", "\x02A97"),
    ("emacr", "\x00113"),
    ("empty", "\x02205"),
    ("emptyset", "\x02205"),
    ("emptyv", "\x02205"),
    ("emsp", "\x02003"),
    ("emsp13", "\x02004"),
    ("emsp14", "\x02005"),
    ("eng", "\x0014B"),
    ("ensp", "\x02002") ]

{-# NOINLINE reftab27 #-}
reftab27 :: [(Text,Text)]
reftab27 =
  [ ("eogon", "\x00119"),
    ("eopf", "\x1D556"),
    ("epar", "\x022D5"),
    ("eparsl", "\x029E3"),
    ("eplus", "\x02A71"),
    ("epsi", "\x003B5"),
    ("epsilon", "\x003B5"),
    ("epsiv", "\x003F5"),
    ("eqcirc", "\x02256"),
    ("eqcolon", "\x02255"),
    ("eqsim", "\x02242"),
    ("eqslantgtr", "\x02A96"),
    ("eqslantless", "\x02A95"),
    ("equals", "\x0003D"),
    ("equest", "\x0225F"),
    ("equiv", "\x02261"),
    ("equivDD", "\x02A78"),
    ("eqvparsl", "\x029E5"),
    ("erDot", "\x02253"),
    ("erarr", "\x02971"),
    ("escr", "\x0212F"),
    ("esdot", "\x02250"),
    ("esim", "\x02242"),
    ("eta", "\x003B7"),
    ("eth", "\x000F0"),
    ("euml", "\x000EB"),
    ("euro", "\x020AC"),
    ("excl", "\x00021"),
    ("exist", "\x02203"),
    ("expectation", "\x02130"),
    ("exponentiale", "\x02147"),
    ("fallingdotseq", "\x02252"),
    ("fcy", "\x00444"),
    ("female", "\x02640"),
    ("ffilig", "\x0FB03"),
    ("fflig", "\x0FB00"),
    ("ffllig", "\x0FB04") ]

{-# NOINLINE reftab28 #-}
reftab28 :: [(Text,Text)]
reftab28 =
  [ ("ffr", "\x1D523"),
    ("filig", "\x0FB01"),
    ("fjlig", "\x00066\x0006A"),
    ("flat", "\x0266D"),
    ("fllig", "\x0FB02"),
    ("fltns", "\x025B1"),
    ("fnof", "\x00192"),
    ("fopf", "\x1D557"),
    ("forall", "\x02200"),
    ("fork", "\x022D4"),
    ("forkv", "\x02AD9"),
    ("fpartint", "\x02A0D"),
    ("frac12", "\x000BD"),
    ("frac13", "\x02153"),
    ("frac14", "\x000BC"),
    ("frac15", "\x02155"),
    ("frac16", "\x02159"),
    ("frac18", "\x0215B"),
    ("frac23", "\x02154"),
    ("frac25", "\x02156"),
    ("frac34", "\x000BE"),
    ("frac35", "\x02157"),
    ("frac38", "\x0215C"),
    ("frac45", "\x02158"),
    ("frac56", "\x0215A"),
    ("frac58", "\x0215D"),
    ("frac78", "\x0215E"),
    ("frasl", "\x02044"),
    ("frown", "\x02322"),
    ("fscr", "\x1D4BB"),
    ("gE", "\x02267"),
    ("gEl", "\x02A8C"),
    ("gacute", "\x001F5"),
    ("gamma", "\x003B3"),
    ("gammad", "\x003DD"),
    ("gap", "\x02A86"),
    ("gbreve", "\x0011F") ]

{-# NOINLINE reftab29 #-}
reftab29 :: [(Text,Text)]
reftab29 =
  [ ("gcirc", "\x0011D"),
    ("gcy", "\x00433"),
    ("gdot", "\x00121"),
    ("ge", "\x02265"),
    ("gel", "\x022DB"),
    ("geq", "\x02265"),
    ("geqq", "\x02267"),
    ("geqslant", "\x02A7E"),
    ("ges", "\x02A7E"),
    ("gescc", "\x02AA9"),
    ("gesdot", "\x02A80"),
    ("gesdoto", "\x02A82"),
    ("gesdotol", "\x02A84"),
    ("gesl", "\x022DB\x0FE00"),
    ("gesles", "\x02A94"),
    ("gfr", "\x1D524"),
    ("gg", "\x0226B"),
    ("ggg", "\x022D9"),
    ("gimel", "\x02137"),
    ("gjcy", "\x00453"),
    ("gl", "\x02277"),
    ("glE", "\x02A92"),
    ("gla", "\x02AA5"),
    ("glj", "\x02AA4"),
    ("gnE", "\x02269"),
    ("gnap", "\x02A8A"),
    ("gnapprox", "\x02A8A"),
    ("gne", "\x02A88"),
    ("gneq", "\x02A88"),
    ("gneqq", "\x02269"),
    ("gnsim", "\x022E7"),
    ("gopf", "\x1D558"),
    ("grave", "\x00060"),
    ("gscr", "\x0210A"),
    ("gsim", "\x02273"),
    ("gsime", "\x02A8E"),
    ("gsiml", "\x02A90") ]

{-# NOINLINE reftab30 #-}
reftab30 :: [(Text,Text)]
reftab30 =
  [ ("gt", "\x0003E"),
    ("gtcc", "\x02AA7"),
    ("gtcir", "\x02A7A"),
    ("gtdot", "\x022D7"),
    ("gtlPar", "\x02995"),
    ("gtquest", "\x02A7C"),
    ("gtrapprox", "\x02A86"),
    ("gtrarr", "\x02978"),
    ("gtrdot", "\x022D7"),
    ("gtreqless", "\x022DB"),
    ("gtreqqless", "\x02A8C"),
    ("gtrless", "\x02277"),
    ("gtrsim", "\x02273"),
    ("gvertneqq", "\x02269\x0FE00"),
    ("gvnE", "\x02269\x0FE00"),
    ("hArr", "\x021D4"),
    ("hairsp", "\x0200A"),
    ("half", "\x000BD"),
    ("hamilt", "\x0210B"),
    ("hardcy", "\x0044A"),
    ("harr", "\x02194"),
    ("harrcir", "\x02948"),
    ("harrw", "\x021AD"),
    ("hbar", "\x0210F"),
    ("hcirc", "\x00125"),
    ("hearts", "\x02665"),
    ("heartsuit", "\x02665"),
    ("hellip", "\x02026"),
    ("hercon", "\x022B9"),
    ("hfr", "\x1D525"),
    ("hksearow", "\x02925"),
    ("hkswarow", "\x02926"),
    ("hoarr", "\x021FF"),
    ("homtht", "\x0223B"),
    ("hookleftarrow", "\x021A9"),
    ("hookrightarrow", "\x021AA"),
    ("hopf", "\x1D559") ]

{-# NOINLINE reftab31 #-}
reftab31 :: [(Text,Text)]
reftab31 =
  [ ("horbar", "\x02015"),
    ("hscr", "\x1D4BD"),
    ("hslash", "\x0210F"),
    ("hstrok", "\x00127"),
    ("hybull", "\x02043"),
    ("hyphen", "\x02010"),
    ("iacute", "\x000ED"),
    ("ic", "\x02063"),
    ("icirc", "\x000EE"),
    ("icy", "\x00438"),
    ("iecy", "\x00435"),
    ("iexcl", "\x000A1"),
    ("iff", "\x021D4"),
    ("ifr", "\x1D526"),
    ("igrave", "\x000EC"),
    ("ii", "\x02148"),
    ("iiiint", "\x02A0C"),
    ("iiint", "\x0222D"),
    ("iinfin", "\x029DC"),
    ("iiota", "\x02129"),
    ("ijlig", "\x00133"),
    ("imacr", "\x0012B"),
    ("image", "\x02111"),
    ("imagline", "\x02110"),
    ("imagpart", "\x02111"),
    ("imath", "\x00131"),
    ("imof", "\x022B7"),
    ("imped", "\x001B5"),
    ("in", "\x02208"),
    ("incare", "\x02105"),
    ("infin", "\x0221E"),
    ("infintie", "\x029DD"),
    ("inodot", "\x00131"),
    ("int", "\x0222B"),
    ("intcal", "\x022BA"),
    ("integers", "\x02124"),
    ("intercal", "\x022BA") ]

{-# NOINLINE reftab32 #-}
reftab32 :: [(Text,Text)]
reftab32 =
  [ ("intlarhk", "\x02A17"),
    ("intprod", "\x02A3C"),
    ("iocy", "\x00451"),
    ("iogon", "\x0012F"),
    ("iopf", "\x1D55A"),
    ("iota", "\x003B9"),
    ("iprod", "\x02A3C"),
    ("iquest", "\x000BF"),
    ("iscr", "\x1D4BE"),
    ("isin", "\x02208"),
    ("isinE", "\x022F9"),
    ("isindot", "\x022F5"),
    ("isins", "\x022F4"),
    ("isinsv", "\x022F3"),
    ("isinv", "\x02208"),
    ("it", "\x02062"),
    ("itilde", "\x00129"),
    ("iukcy", "\x00456"),
    ("iuml", "\x000EF"),
    ("jcirc", "\x00135"),
    ("jcy", "\x00439"),
    ("jfr", "\x1D527"),
    ("jmath", "\x00237"),
    ("jopf", "\x1D55B"),
    ("jscr", "\x1D4BF"),
    ("jsercy", "\x00458"),
    ("jukcy", "\x00454"),
    ("kappa", "\x003BA"),
    ("kappav", "\x003F0"),
    ("kcedil", "\x00137"),
    ("kcy", "\x0043A"),
    ("kfr", "\x1D528"),
    ("kgreen", "\x00138"),
    ("khcy", "\x00445"),
    ("kjcy", "\x0045C"),
    ("kopf", "\x1D55C"),
    ("kscr", "\x1D4C0") ]

{-# NOINLINE reftab33 #-}
reftab33 :: [(Text,Text)]
reftab33 =
  [ ("lAarr", "\x021DA"),
    ("lArr", "\x021D0"),
    ("lAtail", "\x0291B"),
    ("lBarr", "\x0290E"),
    ("lE", "\x02266"),
    ("lEg", "\x02A8B"),
    ("lHar", "\x02962"),
    ("lacute", "\x0013A"),
    ("laemptyv", "\x029B4"),
    ("lagran", "\x02112"),
    ("lambda", "\x003BB"),
    ("lang", "\x027E8"),
    ("langd", "\x02991"),
    ("langle", "\x027E8"),
    ("lap", "\x02A85"),
    ("laquo", "\x000AB"),
    ("larr", "\x02190"),
    ("larrb", "\x021E4"),
    ("larrbfs", "\x0291F"),
    ("larrfs", "\x0291D"),
    ("larrhk", "\x021A9"),
    ("larrlp", "\x021AB"),
    ("larrpl", "\x02939"),
    ("larrsim", "\x02973"),
    ("larrtl", "\x021A2"),
    ("lat", "\x02AAB"),
    ("latail", "\x02919"),
    ("late", "\x02AAD"),
    ("lates", "\x02AAD\x0FE00"),
    ("lbarr", "\x0290C"),
    ("lbbrk", "\x02772"),
    ("lbrace", "\x0007B"),
    ("lbrack", "\x0005B"),
    ("lbrke", "\x0298B"),
    ("lbrksld", "\x0298F"),
    ("lbrkslu", "\x0298D"),
    ("lcaron", "\x0013E") ]

{-# NOINLINE reftab34 #-}
reftab34 :: [(Text,Text)]
reftab34 =
  [ ("lcedil", "\x0013C"),
    ("lceil", "\x02308"),
    ("lcub", "\x0007B"),
    ("lcy", "\x0043B"),
    ("ldca", "\x02936"),
    ("ldquo", "\x0201C"),
    ("ldquor", "\x0201E"),
    ("ldrdhar", "\x02967"),
    ("ldrushar", "\x0294B"),
    ("ldsh", "\x021B2"),
    ("le", "\x02264"),
    ("leftarrow", "\x02190"),
    ("leftarrowtail", "\x021A2"),
    ("leftharpoondown", "\x021BD"),
    ("leftharpoonup", "\x021BC"),
    ("leftleftarrows", "\x021C7"),
    ("leftrightarrow", "\x02194"),
    ("leftrightarrows", "\x021C6"),
    ("leftrightharpoons", "\x021CB"),
    ("leftrightsquigarrow", "\x021AD"),
    ("leftthreetimes", "\x022CB"),
    ("leg", "\x022DA"),
    ("leq", "\x02264"),
    ("leqq", "\x02266"),
    ("leqslant", "\x02A7D"),
    ("les", "\x02A7D"),
    ("lescc", "\x02AA8"),
    ("lesdot", "\x02A7F"),
    ("lesdoto", "\x02A81"),
    ("lesdotor", "\x02A83"),
    ("lesg", "\x022DA\x0FE00"),
    ("lesges", "\x02A93"),
    ("lessapprox", "\x02A85"),
    ("lessdot", "\x022D6"),
    ("lesseqgtr", "\x022DA"),
    ("lesseqqgtr", "\x02A8B"),
    ("lessgtr", "\x02276") ]

{-# NOINLINE reftab35 #-}
reftab35 :: [(Text,Text)]
reftab35 =
  [ ("lesssim", "\x02272"),
    ("lfisht", "\x0297C"),
    ("lfloor", "\x0230A"),
    ("lfr", "\x1D529"),
    ("lg", "\x02276"),
    ("lgE", "\x02A91"),
    ("lhard", "\x021BD"),
    ("lharu", "\x021BC"),
    ("lharul", "\x0296A"),
    ("lhblk", "\x02584"),
    ("ljcy", "\x00459"),
    ("ll", "\x0226A"),
    ("llarr", "\x021C7"),
    ("llcorner", "\x0231E"),
    ("llhard", "\x0296B"),
    ("lltri", "\x025FA"),
    ("lmidot", "\x00140"),
    ("lmoust", "\x023B0"),
    ("lmoustache", "\x023B0"),
    ("lnE", "\x02268"),
    ("lnap", "\x02A89"),
    ("lnapprox", "\x02A89"),
    ("lne", "\x02A87"),
    ("lneq", "\x02A87"),
    ("lneqq", "\x02268"),
    ("lnsim", "\x022E6"),
    ("loang", "\x027EC"),
    ("loarr", "\x021FD"),
    ("lobrk", "\x027E6"),
    ("longleftarrow", "\x027F5"),
    ("longleftrightarrow", "\x027F7"),
    ("longmapsto", "\x027FC"),
    ("longrightarrow", "\x027F6"),
    ("looparrowleft", "\x021AB"),
    ("looparrowright", "\x021AC"),
    ("lopar", "\x02985"),
    ("lopf", "\x1D55D") ]

{-# NOINLINE reftab36 #-}
reftab36 :: [(Text,Text)]
reftab36 =
  [ ("loplus", "\x02A2D"),
    ("lotimes", "\x02A34"),
    ("lowast", "\x02217"),
    ("lowbar", "\x0005F"),
    ("loz", "\x025CA"),
    ("lozenge", "\x025CA"),
    ("lozf", "\x029EB"),
    ("lpar", "\x00028"),
    ("lparlt", "\x02993"),
    ("lrarr", "\x021C6"),
    ("lrcorner", "\x0231F"),
    ("lrhar", "\x021CB"),
    ("lrhard", "\x0296D"),
    ("lrm", "\x0200E"),
    ("lrtri", "\x022BF"),
    ("lsaquo", "\x02039"),
    ("lscr", "\x1D4C1"),
    ("lsh", "\x021B0"),
    ("lsim", "\x02272"),
    ("lsime", "\x02A8D"),
    ("lsimg", "\x02A8F"),
    ("lsqb", "\x0005B"),
    ("lsquo", "\x02018"),
    ("lsquor", "\x0201A"),
    ("lstrok", "\x00142"),
    ("lt", "\x0003C"),
    ("ltcc", "\x02AA6"),
    ("ltcir", "\x02A79"),
    ("ltdot", "\x022D6"),
    ("lthree", "\x022CB"),
    ("ltimes", "\x022C9"),
    ("ltlarr", "\x02976"),
    ("ltquest", "\x02A7B"),
    ("ltrPar", "\x02996"),
    ("ltri", "\x025C3"),
    ("ltrie", "\x022B4"),
    ("ltrif", "\x025C2") ]

{-# NOINLINE reftab37 #-}
reftab37 :: [(Text,Text)]
reftab37 =
  [ ("lurdshar", "\x0294A"),
    ("luruhar", "\x02966"),
    ("lvertneqq", "\x02268\x0FE00"),
    ("lvnE", "\x02268\x0FE00"),
    ("mDDot", "\x0223A"),
    ("macr", "\x000AF"),
    ("male", "\x02642"),
    ("malt", "\x02720"),
    ("maltese", "\x02720"),
    ("map", "\x021A6"),
    ("mapsto", "\x021A6"),
    ("mapstodown", "\x021A7"),
    ("mapstoleft", "\x021A4"),
    ("mapstoup", "\x021A5"),
    ("marker", "\x025AE"),
    ("mcomma", "\x02A29"),
    ("mcy", "\x0043C"),
    ("mdash", "\x02014"),
    ("measuredangle", "\x02221"),
    ("mfr", "\x1D52A"),
    ("mho", "\x02127"),
    ("micro", "\x000B5"),
    ("mid", "\x02223"),
    ("midast", "\x0002A"),
    ("midcir", "\x02AF0"),
    ("middot", "\x000B7"),
    ("minus", "\x02212"),
    ("minusb", "\x0229F"),
    ("minusd", "\x02238"),
    ("minusdu", "\x02A2A"),
    ("mlcp", "\x02ADB"),
    ("mldr", "\x02026"),
    ("mnplus", "\x02213"),
    ("models", "\x022A7"),
    ("mopf", "\x1D55E"),
    ("mp", "\x02213"),
    ("mscr", "\x1D4C2") ]

{-# NOINLINE reftab38 #-}
reftab38 :: [(Text,Text)]
reftab38 =
  [ ("mstpos", "\x0223E"),
    ("mu", "\x003BC"),
    ("multimap", "\x022B8"),
    ("mumap", "\x022B8"),
    ("nGg", "\x022D9\x00338"),
    ("nGt", "\x0226B\x020D2"),
    ("nGtv", "\x0226B\x00338"),
    ("nLeftarrow", "\x021CD"),
    ("nLeftrightarrow", "\x021CE"),
    ("nLl", "\x022D8\x00338"),
    ("nLt", "\x0226A\x020D2"),
    ("nLtv", "\x0226A\x00338"),
    ("nRightarrow", "\x021CF"),
    ("nVDash", "\x022AF"),
    ("nVdash", "\x022AE"),
    ("nabla", "\x02207"),
    ("nacute", "\x00144"),
    ("nang", "\x02220\x020D2"),
    ("nap", "\x02249"),
    ("napE", "\x02A70\x00338"),
    ("napid", "\x0224B\x00338"),
    ("napos", "\x00149"),
    ("napprox", "\x02249"),
    ("natur", "\x0266E"),
    ("natural", "\x0266E"),
    ("naturals", "\x02115"),
    ("nbsp", "\x000A0"),
    ("nbump", "\x0224E\x00338"),
    ("nbumpe", "\x0224F\x00338"),
    ("ncap", "\x02A43"),
    ("ncaron", "\x00148"),
    ("ncedil", "\x00146"),
    ("ncong", "\x02247"),
    ("ncongdot", "\x02A6D\x00338"),
    ("ncup", "\x02A42"),
    ("ncy", "\x0043D"),
    ("ndash", "\x02013") ]

{-# NOINLINE reftab39 #-}
reftab39 :: [(Text,Text)]
reftab39 =
  [ ("ne", "\x02260"),
    ("neArr", "\x021D7"),
    ("nearhk", "\x02924"),
    ("nearr", "\x02197"),
    ("nearrow", "\x02197"),
    ("nedot", "\x02250\x00338"),
    ("nequiv", "\x02262"),
    ("nesear", "\x02928"),
    ("nesim", "\x02242\x00338"),
    ("nexist", "\x02204"),
    ("nexists", "\x02204"),
    ("nfr", "\x1D52B"),
    ("ngE", "\x02267\x00338"),
    ("nge", "\x02271"),
    ("ngeq", "\x02271"),
    ("ngeqq", "\x02267\x00338"),
    ("ngeqslant", "\x02A7E\x00338"),
    ("nges", "\x02A7E\x00338"),
    ("ngsim", "\x02275"),
    ("ngt", "\x0226F"),
    ("ngtr", "\x0226F"),
    ("nhArr", "\x021CE"),
    ("nharr", "\x021AE"),
    ("nhpar", "\x02AF2"),
    ("ni", "\x0220B"),
    ("nis", "\x022FC"),
    ("nisd", "\x022FA"),
    ("niv", "\x0220B"),
    ("njcy", "\x0045A"),
    ("nlArr", "\x021CD"),
    ("nlE", "\x02266\x00338"),
    ("nlarr", "\x0219A"),
    ("nldr", "\x02025"),
    ("nle", "\x02270"),
    ("nleftarrow", "\x0219A"),
    ("nleftrightarrow", "\x021AE"),
    ("nleq", "\x02270") ]

{-# NOINLINE reftab40 #-}
reftab40 :: [(Text,Text)]
reftab40 =
  [ ("nleqq", "\x02266\x00338"),
    ("nleqslant", "\x02A7D\x00338"),
    ("nles", "\x02A7D\x00338"),
    ("nless", "\x0226E"),
    ("nlsim", "\x02274"),
    ("nlt", "\x0226E"),
    ("nltri", "\x022EA"),
    ("nltrie", "\x022EC"),
    ("nmid", "\x02224"),
    ("nopf", "\x1D55F"),
    ("not", "\x000AC"),
    ("notin", "\x02209"),
    ("notinE", "\x022F9\x00338"),
    ("notindot", "\x022F5\x00338"),
    ("notinva", "\x02209"),
    ("notinvb", "\x022F7"),
    ("notinvc", "\x022F6"),
    ("notni", "\x0220C"),
    ("notniva", "\x0220C"),
    ("notnivb", "\x022FE"),
    ("notnivc", "\x022FD"),
    ("npar", "\x02226"),
    ("nparallel", "\x02226"),
    ("nparsl", "\x02AFD\x020E5"),
    ("npart", "\x02202\x00338"),
    ("npolint", "\x02A14"),
    ("npr", "\x02280"),
    ("nprcue", "\x022E0"),
    ("npre", "\x02AAF\x00338"),
    ("nprec", "\x02280"),
    ("npreceq", "\x02AAF\x00338"),
    ("nrArr", "\x021CF"),
    ("nrarr", "\x0219B"),
    ("nrarrc", "\x02933\x00338"),
    ("nrarrw", "\x0219D\x00338"),
    ("nrightarrow", "\x0219B"),
    ("nrtri", "\x022EB") ]

{-# NOINLINE reftab41 #-}
reftab41 :: [(Text,Text)]
reftab41 =
  [ ("nrtrie", "\x022ED"),
    ("nsc", "\x02281"),
    ("nsccue", "\x022E1"),
    ("nsce", "\x02AB0\x00338"),
    ("nscr", "\x1D4C3"),
    ("nshortmid", "\x02224"),
    ("nshortparallel", "\x02226"),
    ("nsim", "\x02241"),
    ("nsime", "\x02244"),
    ("nsimeq", "\x02244"),
    ("nsmid", "\x02224"),
    ("nspar", "\x02226"),
    ("nsqsube", "\x022E2"),
    ("nsqsupe", "\x022E3"),
    ("nsub", "\x02284"),
    ("nsubE", "\x02AC5\x00338"),
    ("nsube", "\x02288"),
    ("nsubset", "\x02282\x020D2"),
    ("nsubseteq", "\x02288"),
    ("nsubseteqq", "\x02AC5\x00338"),
    ("nsucc", "\x02281"),
    ("nsucceq", "\x02AB0\x00338"),
    ("nsup", "\x02285"),
    ("nsupE", "\x02AC6\x00338"),
    ("nsupe", "\x02289"),
    ("nsupset", "\x02283\x020D2"),
    ("nsupseteq", "\x02289"),
    ("nsupseteqq", "\x02AC6\x00338"),
    ("ntgl", "\x02279"),
    ("ntilde", "\x000F1"),
    ("ntlg", "\x02278"),
    ("ntriangleleft", "\x022EA"),
    ("ntrianglelefteq", "\x022EC"),
    ("ntriangleright", "\x022EB"),
    ("ntrianglerighteq", "\x022ED"),
    ("nu", "\x003BD"),
    ("num", "\x00023") ]

{-# NOINLINE reftab42 #-}
reftab42 :: [(Text,Text)]
reftab42 =
  [ ("numero", "\x02116"),
    ("numsp", "\x02007"),
    ("nvDash", "\x022AD"),
    ("nvHarr", "\x02904"),
    ("nvap", "\x0224D\x020D2"),
    ("nvdash", "\x022AC"),
    ("nvge", "\x02265\x020D2"),
    ("nvgt", "\x0003E\x020D2"),
    ("nvinfin", "\x029DE"),
    ("nvlArr", "\x02902"),
    ("nvle", "\x02264\x020D2"),
    ("nvlt", "\x0003C\x020D2"),
    ("nvltrie", "\x022B4\x020D2"),
    ("nvrArr", "\x02903"),
    ("nvrtrie", "\x022B5\x020D2"),
    ("nvsim", "\x0223C\x020D2"),
    ("nwArr", "\x021D6"),
    ("nwarhk", "\x02923"),
    ("nwarr", "\x02196"),
    ("nwarrow", "\x02196"),
    ("nwnear", "\x02927"),
    ("oS", "\x024C8"),
    ("oacute", "\x000F3"),
    ("oast", "\x0229B"),
    ("ocir", "\x0229A"),
    ("ocirc", "\x000F4"),
    ("ocy", "\x0043E"),
    ("odash", "\x0229D"),
    ("odblac", "\x00151"),
    ("odiv", "\x02A38"),
    ("odot", "\x02299"),
    ("odsold", "\x029BC"),
    ("oelig", "\x00153"),
    ("ofcir", "\x029BF"),
    ("ofr", "\x1D52C"),
    ("ogon", "\x002DB"),
    ("ograve", "\x000F2") ]

{-# NOINLINE reftab43 #-}
reftab43 :: [(Text,Text)]
reftab43 =
  [ ("ogt", "\x029C1"),
    ("ohbar", "\x029B5"),
    ("ohm", "\x003A9"),
    ("oint", "\x0222E"),
    ("olarr", "\x021BA"),
    ("olcir", "\x029BE"),
    ("olcross", "\x029BB"),
    ("oline", "\x0203E"),
    ("olt", "\x029C0"),
    ("omacr", "\x0014D"),
    ("omega", "\x003C9"),
    ("omicron", "\x003BF"),
    ("omid", "\x029B6"),
    ("ominus", "\x02296"),
    ("oopf", "\x1D560"),
    ("opar", "\x029B7"),
    ("operp", "\x029B9"),
    ("oplus", "\x02295"),
    ("or", "\x02228"),
    ("orarr", "\x021BB"),
    ("ord", "\x02A5D"),
    ("order", "\x02134"),
    ("orderof", "\x02134"),
    ("ordf", "\x000AA"),
    ("ordm", "\x000BA"),
    ("origof", "\x022B6"),
    ("oror", "\x02A56"),
    ("orslope", "\x02A57"),
    ("orv", "\x02A5B"),
    ("oscr", "\x02134"),
    ("oslash", "\x000F8"),
    ("osol", "\x02298"),
    ("otilde", "\x000F5"),
    ("otimes", "\x02297"),
    ("otimesas", "\x02A36"),
    ("ouml", "\x000F6"),
    ("ovbar", "\x0233D") ]

{-# NOINLINE reftab44 #-}
reftab44 :: [(Text,Text)]
reftab44 =
  [ ("par", "\x02225"),
    ("para", "\x000B6"),
    ("parallel", "\x02225"),
    ("parsim", "\x02AF3"),
    ("parsl", "\x02AFD"),
    ("part", "\x02202"),
    ("pcy", "\x0043F"),
    ("percnt", "\x00025"),
    ("period", "\x0002E"),
    ("permil", "\x02030"),
    ("perp", "\x022A5"),
    ("pertenk", "\x02031"),
    ("pfr", "\x1D52D"),
    ("phi", "\x003C6"),
    ("phiv", "\x003D5"),
    ("phmmat", "\x02133"),
    ("phone", "\x0260E"),
    ("pi", "\x003C0"),
    ("pitchfork", "\x022D4"),
    ("piv", "\x003D6"),
    ("planck", "\x0210F"),
    ("planckh", "\x0210E"),
    ("plankv", "\x0210F"),
    ("plus", "\x0002B"),
    ("plusacir", "\x02A23"),
    ("plusb", "\x0229E"),
    ("pluscir", "\x02A22"),
    ("plusdo", "\x02214"),
    ("plusdu", "\x02A25"),
    ("pluse", "\x02A72"),
    ("plusmn", "\x000B1"),
    ("plussim", "\x02A26"),
    ("plustwo", "\x02A27"),
    ("pm", "\x000B1"),
    ("pointint", "\x02A15"),
    ("popf", "\x1D561"),
    ("pound", "\x000A3") ]

{-# NOINLINE reftab45 #-}
reftab45 :: [(Text,Text)]
reftab45 =
  [ ("pr", "\x0227A"),
    ("prE", "\x02AB3"),
    ("prap", "\x02AB7"),
    ("prcue", "\x0227C"),
    ("pre", "\x02AAF"),
    ("prec", "\x0227A"),
    ("precapprox", "\x02AB7"),
    ("preccurlyeq", "\x0227C"),
    ("preceq", "\x02AAF"),
    ("precnapprox", "\x02AB9"),
    ("precneqq", "\x02AB5"),
    ("precnsim", "\x022E8"),
    ("precsim", "\x0227E"),
    ("prime", "\x02032"),
    ("primes", "\x02119"),
    ("prnE", "\x02AB5"),
    ("prnap", "\x02AB9"),
    ("prnsim", "\x022E8"),
    ("prod", "\x0220F"),
    ("profalar", "\x0232E"),
    ("profline", "\x02312"),
    ("profsurf", "\x02313"),
    ("prop", "\x0221D"),
    ("propto", "\x0221D"),
    ("prsim", "\x0227E"),
    ("prurel", "\x022B0"),
    ("pscr", "\x1D4C5"),
    ("psi", "\x003C8"),
    ("puncsp", "\x02008"),
    ("qfr", "\x1D52E"),
    ("qint", "\x02A0C"),
    ("qopf", "\x1D562"),
    ("qprime", "\x02057"),
    ("qscr", "\x1D4C6"),
    ("quaternions", "\x0210D"),
    ("quatint", "\x02A16"),
    ("quest", "\x0003F") ]

{-# NOINLINE reftab46 #-}
reftab46 :: [(Text,Text)]
reftab46 =
  [ ("questeq", "\x0225F"),
    ("quot", "\x00022"),
    ("rAarr", "\x021DB"),
    ("rArr", "\x021D2"),
    ("rAtail", "\x0291C"),
    ("rBarr", "\x0290F"),
    ("rHar", "\x02964"),
    ("race", "\x0223D\x00331"),
    ("racute", "\x00155"),
    ("radic", "\x0221A"),
    ("raemptyv", "\x029B3"),
    ("rang", "\x027E9"),
    ("rangd", "\x02992"),
    ("range", "\x029A5"),
    ("rangle", "\x027E9"),
    ("raquo", "\x000BB"),
    ("rarr", "\x02192"),
    ("rarrap", "\x02975"),
    ("rarrb", "\x021E5"),
    ("rarrbfs", "\x02920"),
    ("rarrc", "\x02933"),
    ("rarrfs", "\x0291E"),
    ("rarrhk", "\x021AA"),
    ("rarrlp", "\x021AC"),
    ("rarrpl", "\x02945"),
    ("rarrsim", "\x02974"),
    ("rarrtl", "\x021A3"),
    ("rarrw", "\x0219D"),
    ("ratail", "\x0291A"),
    ("ratio", "\x02236"),
    ("rationals", "\x0211A"),
    ("rbarr", "\x0290D"),
    ("rbbrk", "\x02773"),
    ("rbrace", "\x0007D"),
    ("rbrack", "\x0005D"),
    ("rbrke", "\x0298C"),
    ("rbrksld", "\x0298E") ]

{-# NOINLINE reftab47 #-}
reftab47 :: [(Text,Text)]
reftab47 =
  [ ("rbrkslu", "\x02990"),
    ("rcaron", "\x00159"),
    ("rcedil", "\x00157"),
    ("rceil", "\x02309"),
    ("rcub", "\x0007D"),
    ("rcy", "\x00440"),
    ("rdca", "\x02937"),
    ("rdldhar", "\x02969"),
    ("rdquo", "\x0201D"),
    ("rdquor", "\x0201D"),
    ("rdsh", "\x021B3"),
    ("real", "\x0211C"),
    ("realine", "\x0211B"),
    ("realpart", "\x0211C"),
    ("reals", "\x0211D"),
    ("rect", "\x025AD"),
    ("reg", "\x000AE"),
    ("rfisht", "\x0297D"),
    ("rfloor", "\x0230B"),
    ("rfr", "\x1D52F"),
    ("rhard", "\x021C1"),
    ("rharu", "\x021C0"),
    ("rharul", "\x0296C"),
    ("rho", "\x003C1"),
    ("rhov", "\x003F1"),
    ("rightarrow", "\x02192"),
    ("rightarrowtail", "\x021A3"),
    ("rightharpoondown", "\x021C1"),
    ("rightharpoonup", "\x021C0"),
    ("rightleftarrows", "\x021C4"),
    ("rightleftharpoons", "\x021CC"),
    ("rightrightarrows", "\x021C9"),
    ("rightsquigarrow", "\x0219D"),
    ("rightthreetimes", "\x022CC"),
    ("ring", "\x002DA"),
    ("risingdotseq", "\x02253"),
    ("rlarr", "\x021C4") ]

{-# NOINLINE reftab48 #-}
reftab48 :: [(Text,Text)]
reftab48 =
  [ ("rlhar", "\x021CC"),
    ("rlm", "\x0200F"),
    ("rmoust", "\x023B1"),
    ("rmoustache", "\x023B1"),
    ("rnmid", "\x02AEE"),
    ("roang", "\x027ED"),
    ("roarr", "\x021FE"),
    ("robrk", "\x027E7"),
    ("ropar", "\x02986"),
    ("ropf", "\x1D563"),
    ("roplus", "\x02A2E"),
    ("rotimes", "\x02A35"),
    ("rpar", "\x00029"),
    ("rpargt", "\x02994"),
    ("rppolint", "\x02A12"),
    ("rrarr", "\x021C9"),
    ("rsaquo", "\x0203A"),
    ("rscr", "\x1D4C7"),
    ("rsh", "\x021B1"),
    ("rsqb", "\x0005D"),
    ("rsquo", "\x02019"),
    ("rsquor", "\x02019"),
    ("rthree", "\x022CC"),
    ("rtimes", "\x022CA"),
    ("rtri", "\x025B9"),
    ("rtrie", "\x022B5"),
    ("rtrif", "\x025B8"),
    ("rtriltri", "\x029CE"),
    ("ruluhar", "\x02968"),
    ("rx", "\x0211E"),
    ("sacute", "\x0015B"),
    ("sbquo", "\x0201A"),
    ("sc", "\x0227B"),
    ("scE", "\x02AB4"),
    ("scap", "\x02AB8"),
    ("scaron", "\x00161"),
    ("sccue", "\x0227D") ]

{-# NOINLINE reftab49 #-}
reftab49 :: [(Text,Text)]
reftab49 =
  [ ("sce", "\x02AB0"),
    ("scedil", "\x0015F"),
    ("scirc", "\x0015D"),
    ("scnE", "\x02AB6"),
    ("scnap", "\x02ABA"),
    ("scnsim", "\x022E9"),
    ("scpolint", "\x02A13"),
    ("scsim", "\x0227F"),
    ("scy", "\x00441"),
    ("sdot", "\x022C5"),
    ("sdotb", "\x022A1"),
    ("sdote", "\x02A66"),
    ("seArr", "\x021D8"),
    ("searhk", "\x02925"),
    ("searr", "\x02198"),
    ("searrow", "\x02198"),
    ("sect", "\x000A7"),
    ("semi", "\x0003B"),
    ("seswar", "\x02929"),
    ("setminus", "\x02216"),
    ("setmn", "\x02216"),
    ("sext", "\x02736"),
    ("sfr", "\x1D530"),
    ("sfrown", "\x02322"),
    ("sharp", "\x0266F"),
    ("shchcy", "\x00449"),
    ("shcy", "\x00448"),
    ("shortmid", "\x02223"),
    ("shortparallel", "\x02225"),
    ("shy", "\x000AD"),
    ("sigma", "\x003C3"),
    ("sigmaf", "\x003C2"),
    ("sigmav", "\x003C2"),
    ("sim", "\x0223C"),
    ("simdot", "\x02A6A"),
    ("sime", "\x02243"),
    ("simeq", "\x02243") ]

{-# NOINLINE reftab50 #-}
reftab50 :: [(Text,Text)]
reftab50 =
  [ ("simg", "\x02A9E"),
    ("simgE", "\x02AA0"),
    ("siml", "\x02A9D"),
    ("simlE", "\x02A9F"),
    ("simne", "\x02246"),
    ("simplus", "\x02A24"),
    ("simrarr", "\x02972"),
    ("slarr", "\x02190"),
    ("smallsetminus", "\x02216"),
    ("smashp", "\x02A33"),
    ("smeparsl", "\x029E4"),
    ("smid", "\x02223"),
    ("smile", "\x02323"),
    ("smt", "\x02AAA"),
    ("smte", "\x02AAC"),
    ("smtes", "\x02AAC\x0FE00"),
    ("softcy", "\x0044C"),
    ("sol", "\x0002F"),
    ("solb", "\x029C4"),
    ("solbar", "\x0233F"),
    ("sopf", "\x1D564"),
    ("spades", "\x02660"),
    ("spadesuit", "\x02660"),
    ("spar", "\x02225"),
    ("sqcap", "\x02293"),
    ("sqcaps", "\x02293\x0FE00"),
    ("sqcup", "\x02294"),
    ("sqcups", "\x02294\x0FE00"),
    ("sqsub", "\x0228F"),
    ("sqsube", "\x02291"),
    ("sqsubset", "\x0228F"),
    ("sqsubseteq", "\x02291"),
    ("sqsup", "\x02290"),
    ("sqsupe", "\x02292"),
    ("sqsupset", "\x02290"),
    ("sqsupseteq", "\x02292"),
    ("squ", "\x025A1") ]

{-# NOINLINE reftab51 #-}
reftab51 :: [(Text,Text)]
reftab51 =
  [ ("square", "\x025A1"),
    ("squarf", "\x025AA"),
    ("squf", "\x025AA"),
    ("srarr", "\x02192"),
    ("sscr", "\x1D4C8"),
    ("ssetmn", "\x02216"),
    ("ssmile", "\x02323"),
    ("sstarf", "\x022C6"),
    ("star", "\x02606"),
    ("starf", "\x02605"),
    ("straightepsilon", "\x003F5"),
    ("straightphi", "\x003D5"),
    ("strns", "\x000AF"),
    ("sub", "\x02282"),
    ("subE", "\x02AC5"),
    ("subdot", "\x02ABD"),
    ("sube", "\x02286"),
    ("subedot", "\x02AC3"),
    ("submult", "\x02AC1"),
    ("subnE", "\x02ACB"),
    ("subne", "\x0228A"),
    ("subplus", "\x02ABF"),
    ("subrarr", "\x02979"),
    ("subset", "\x02282"),
    ("subseteq", "\x02286"),
    ("subseteqq", "\x02AC5"),
    ("subsetneq", "\x0228A"),
    ("subsetneqq", "\x02ACB"),
    ("subsim", "\x02AC7"),
    ("subsub", "\x02AD5"),
    ("subsup", "\x02AD3"),
    ("succ", "\x0227B"),
    ("succapprox", "\x02AB8"),
    ("succcurlyeq", "\x0227D"),
    ("succeq", "\x02AB0"),
    ("succnapprox", "\x02ABA"),
    ("succneqq", "\x02AB6") ]

{-# NOINLINE reftab52 #-}
reftab52 :: [(Text,Text)]
reftab52 =
  [ ("succnsim", "\x022E9"),
    ("succsim", "\x0227F"),
    ("sum", "\x02211"),
    ("sung", "\x0266A"),
    ("sup", "\x02283"),
    ("sup1", "\x000B9"),
    ("sup2", "\x000B2"),
    ("sup3", "\x000B3"),
    ("supE", "\x02AC6"),
    ("supdot", "\x02ABE"),
    ("supdsub", "\x02AD8"),
    ("supe", "\x02287"),
    ("supedot", "\x02AC4"),
    ("suphsol", "\x027C9"),
    ("suphsub", "\x02AD7"),
    ("suplarr", "\x0297B"),
    ("supmult", "\x02AC2"),
    ("supnE", "\x02ACC"),
    ("supne", "\x0228B"),
    ("supplus", "\x02AC0"),
    ("supset", "\x02283"),
    ("supseteq", "\x02287"),
    ("supseteqq", "\x02AC6"),
    ("supsetneq", "\x0228B"),
    ("supsetneqq", "\x02ACC"),
    ("supsim", "\x02AC8"),
    ("supsub", "\x02AD4"),
    ("supsup", "\x02AD6"),
    ("swArr", "\x021D9"),
    ("swarhk", "\x02926"),
    ("swarr", "\x02199"),
    ("swarrow", "\x02199"),
    ("swnwar", "\x0292A"),
    ("szlig", "\x000DF"),
    ("target", "\x02316"),
    ("tau", "\x003C4"),
    ("tbrk", "\x023B4") ]

{-# NOINLINE reftab53 #-}
reftab53 :: [(Text,Text)]
reftab53 =
  [ ("tcaron", "\x00165"),
    ("tcedil", "\x00163"),
    ("tcy", "\x00442"),
    ("tdot", "\x020DB"),
    ("telrec", "\x02315"),
    ("tfr", "\x1D531"),
    ("there4", "\x02234"),
    ("therefore", "\x02234"),
    ("theta", "\x003B8"),
    ("thetasym", "\x003D1"),
    ("thetav", "\x003D1"),
    ("thickapprox", "\x02248"),
    ("thicksim", "\x0223C"),
    ("thinsp", "\x02009"),
    ("thkap", "\x02248"),
    ("thksim", "\x0223C"),
    ("thorn", "\x000FE"),
    ("tilde", "\x002DC"),
    ("times", "\x000D7"),
    ("timesb", "\x022A0"),
    ("timesbar", "\x02A31"),
    ("timesd", "\x02A30"),
    ("tint", "\x0222D"),
    ("toea", "\x02928"),
    ("top", "\x022A4"),
    ("topbot", "\x02336"),
    ("topcir", "\x02AF1"),
    ("topf", "\x1D565"),
    ("topfork", "\x02ADA"),
    ("tosa", "\x02929"),
    ("tprime", "\x02034"),
    ("trade", "\x02122"),
    ("triangle", "\x025B5"),
    ("triangledown", "\x025BF"),
    ("triangleleft", "\x025C3"),
    ("trianglelefteq", "\x022B4"),
    ("triangleq", "\x0225C") ]

{-# NOINLINE reftab54 #-}
reftab54 :: [(Text,Text)]
reftab54 =
  [ ("triangleright", "\x025B9"),
    ("trianglerighteq", "\x022B5"),
    ("tridot", "\x025EC"),
    ("trie", "\x0225C"),
    ("triminus", "\x02A3A"),
    ("triplus", "\x02A39"),
    ("trisb", "\x029CD"),
    ("tritime", "\x02A3B"),
    ("trpezium", "\x023E2"),
    ("tscr", "\x1D4C9"),
    ("tscy", "\x00446"),
    ("tshcy", "\x0045B"),
    ("tstrok", "\x00167"),
    ("twixt", "\x0226C"),
    ("twoheadleftarrow", "\x0219E"),
    ("twoheadrightarrow", "\x021A0"),
    ("uArr", "\x021D1"),
    ("uHar", "\x02963"),
    ("uacute", "\x000FA"),
    ("uarr", "\x02191"),
    ("ubrcy", "\x0045E"),
    ("ubreve", "\x0016D"),
    ("ucirc", "\x000FB"),
    ("ucy", "\x00443"),
    ("udarr", "\x021C5"),
    ("udblac", "\x00171"),
    ("udhar", "\x0296E"),
    ("ufisht", "\x0297E"),
    ("ufr", "\x1D532"),
    ("ugrave", "\x000F9"),
    ("uharl", "\x021BF"),
    ("uharr", "\x021BE"),
    ("uhblk", "\x02580"),
    ("ulcorn", "\x0231C"),
    ("ulcorner", "\x0231C"),
    ("ulcrop", "\x0230F"),
    ("ultri", "\x025F8") ]

{-# NOINLINE reftab55 #-}
reftab55 :: [(Text,Text)]
reftab55 =
  [ ("umacr", "\x0016B"),
    ("uml", "\x000A8"),
    ("uogon", "\x00173"),
    ("uopf", "\x1D566"),
    ("uparrow", "\x02191"),
    ("updownarrow", "\x02195"),
    ("upharpoonleft", "\x021BF"),
    ("upharpoonright", "\x021BE"),
    ("uplus", "\x0228E"),
    ("upsi", "\x003C5"),
    ("upsih", "\x003D2"),
    ("upsilon", "\x003C5"),
    ("upuparrows", "\x021C8"),
    ("urcorn", "\x0231D"),
    ("urcorner", "\x0231D"),
    ("urcrop", "\x0230E"),
    ("uring", "\x0016F"),
    ("urtri", "\x025F9"),
    ("uscr", "\x1D4CA"),
    ("utdot", "\x022F0"),
    ("utilde", "\x00169"),
    ("utri", "\x025B5"),
    ("utrif", "\x025B4"),
    ("uuarr", "\x021C8"),
    ("uuml", "\x000FC"),
    ("uwangle", "\x029A7"),
    ("vArr", "\x021D5"),
    ("vBar", "\x02AE8"),
    ("vBarv", "\x02AE9"),
    ("vDash", "\x022A8"),
    ("vangrt", "\x0299C"),
    ("varepsilon", "\x003F5"),
    ("varkappa", "\x003F0"),
    ("varnothing", "\x02205"),
    ("varphi", "\x003D5"),
    ("varpi", "\x003D6"),
    ("varpropto", "\x0221D") ]

{-# NOINLINE reftab56 #-}
reftab56 :: [(Text,Text)]
reftab56 =
  [ ("varr", "\x02195"),
    ("varrho", "\x003F1"),
    ("varsigma", "\x003C2"),
    ("varsubsetneq", "\x0228A\x0FE00"),
    ("varsubsetneqq", "\x02ACB\x0FE00"),
    ("varsupsetneq", "\x0228B\x0FE00"),
    ("varsupsetneqq", "\x02ACC\x0FE00"),
    ("vartheta", "\x003D1"),
    ("vartriangleleft", "\x022B2"),
    ("vartriangleright", "\x022B3"),
    ("vcy", "\x00432"),
    ("vdash", "\x022A2"),
    ("vee", "\x02228"),
    ("veebar", "\x022BB"),
    ("veeeq", "\x0225A"),
    ("vellip", "\x022EE"),
    ("verbar", "\x0007C"),
    ("vert", "\x0007C"),
    ("vfr", "\x1D533"),
    ("vltri", "\x022B2"),
    ("vnsub", "\x02282\x020D2"),
    ("vnsup", "\x02283\x020D2"),
    ("vopf", "\x1D567"),
    ("vprop", "\x0221D"),
    ("vrtri", "\x022B3"),
    ("vscr", "\x1D4CB"),
    ("vsubnE", "\x02ACB\x0FE00"),
    ("vsubne", "\x0228A\x0FE00"),
    ("vsupnE", "\x02ACC\x0FE00"),
    ("vsupne", "\x0228B\x0FE00"),
    ("vzigzag", "\x0299A"),
    ("wcirc", "\x00175"),
    ("wedbar", "\x02A5F"),
    ("wedge", "\x02227"),
    ("wedgeq", "\x02259"),
    ("weierp", "\x02118"),
    ("wfr", "\x1D534") ]

{-# NOINLINE reftab57 #-}
reftab57 :: [(Text,Text)]
reftab57 =
  [ ("wopf", "\x1D568"),
    ("wp", "\x02118"),
    ("wr", "\x02240"),
    ("wreath", "\x02240"),
    ("wscr", "\x1D4CC"),
    ("xcap", "\x022C2"),
    ("xcirc", "\x025EF"),
    ("xcup", "\x022C3"),
    ("xdtri", "\x025BD"),
    ("xfr", "\x1D535"),
    ("xhArr", "\x027FA"),
    ("xharr", "\x027F7"),
    ("xi", "\x003BE"),
    ("xlArr", "\x027F8"),
    ("xlarr", "\x027F5"),
    ("xmap", "\x027FC"),
    ("xnis", "\x022FB"),
    ("xodot", "\x02A00"),
    ("xopf", "\x1D569"),
    ("xoplus", "\x02A01"),
    ("xotime", "\x02A02"),
    ("xrArr", "\x027F9"),
    ("xrarr", "\x027F6"),
    ("xscr", "\x1D4CD"),
    ("xsqcup", "\x02A06"),
    ("xuplus", "\x02A04"),
    ("xutri", "\x025B3"),
    ("xvee", "\x022C1"),
    ("xwedge", "\x022C0"),
    ("yacute", "\x000FD"),
    ("yacy", "\x0044F"),
    ("ycirc", "\x00177"),
    ("ycy", "\x0044B"),
    ("yen", "\x000A5"),
    ("yfr", "\x1D536"),
    ("yicy", "\x00457"),
    ("yopf", "\x1D56A") ]

{-# NOINLINE reftab58 #-}
reftab58 :: [(Text,Text)]
reftab58 =
  [ ("yscr", "\x1D4CE"),
    ("yucy", "\x0044E"),
    ("yuml", "\x000FF"),
    ("zacute", "\x0017A"),
    ("zcaron", "\x0017E"),
    ("zcy", "\x00437"),
    ("zdot", "\x0017C"),
    ("zeetrf", "\x02128"),
    ("zeta", "\x003B6"),
    ("zfr", "\x1D537"),
    ("zhcy", "\x00436"),
    ("zigrarr", "\x021DD"),
    ("zopf", "\x1D56B"),
    ("zscr", "\x1D4CF"),
    ("zwj", "\x0200D"),
    ("zwnj", "\x0200C") ]


------------------------------------------------------------------------------
-- Reverse lookup of Html entities
reversePredefinedRefs :: Map Text Text
reversePredefinedRefs = Map.fromList . map (\(x,y) -> (y,x)) $
                        Map.toList predefinedRefs

-- predefinedRefs
nameEntMap :: Map Text Text
nameEntMap = Map.fromList $ concatMap f entNames
  where
    f (e,ns) = map (,e) ns

------------------------------------------------------------------------------
-- Reverse lookup of Html entities
-- reversePredefinedRefs
entNameMap :: Map Text Text
entNameMap = fmap head $ Map.fromList entNames

entNames :: [(Text,[Text])]
entNames = concat
  [ ent2names0
  , ent2names1
  , ent2names2
  , ent2names3
  , ent2names4
  , ent2names5
  , ent2names6
  , ent2names7
  , ent2names8
  , ent2names9
  , ent2names10
  , ent2names11
  , ent2names12
  , ent2names13
  , ent2names14
  , ent2names15
  , ent2names16
  , ent2names17
  , ent2names18
  , ent2names19
  , ent2names20
  , ent2names21
  , ent2names22
  , ent2names23
  , ent2names24
  , ent2names25
  , ent2names26
  , ent2names27
  , ent2names28
  , ent2names29
  , ent2names30
  , ent2names31
  , ent2names32
  , ent2names33
  , ent2names34
  , ent2names35
  , ent2names36
  , ent2names37
  ]

{-# NOINLINE ent2names0 #-}
ent2names0 :: [(Text,[Text])]
ent2names0 =
  [ ("\x00009",["Tab"])
  , ("\x0000A",["NewLine"])
  , ("\x00021",["excl"])
  , ("\x00022",["quot","QUOT"])
  , ("\x00023",["num"])
  , ("\x00024",["dollar"])
  , ("\x00025",["percnt"])
  , ("\x00026",["amp","AMP"])
  , ("\x00027",["apos"])
  , ("\x00028",["lpar"])
  , ("\x00029",["rpar"])
  , ("\x0002A",["ast","midast"])
  , ("\x0002B",["plus"])
  , ("\x0002C",["comma"])
  , ("\x0002E",["period"])
  , ("\x0002F",["sol"])
  , ("\x0003A",["colon"])
  , ("\x0003B",["semi"])
  , ("\x0003C",["lt","LT"])
  , ("\x0003D",["equals"])
  , ("\x0003E",["gt","GT"])
  , ("\x0003F",["quest"])
  , ("\x00040",["commat"])
  , ("\x0005B",["lsqb","lbrack"])
  , ("\x0005C",["bsol"])
  , ("\x0005D",["rsqb","rbrack"])
  , ("\x0005E",["Hat"])
  , ("\x0005F",["lowbar"])
  , ("\x00060",["grave","DiacriticalGrave"])
  , ("\x0007B",["lcub","lbrace"])
  , ("\x0007C",["verbar","vert","VerticalLine"])
  , ("\x0007D",["rcub","rbrace"])
  , ("\x000A0",["nbsp","NonBreakingSpace"])
  , ("\x000A1",["iexcl"])
  , ("\x000A2",["cent"])
  , ("\x000A3",["pound"])
  , ("\x000A4",["curren"])
  , ("\x000A5",["yen"])
  , ("\x000A6",["brvbar"])
  , ("\x000A7",["sect"])
  ]


{-# NOINLINE ent2names1 #-}
ent2names1 :: [(Text,[Text])]
ent2names1 =
  [ ("\x000A8",["Dot","die","DoubleDot","uml"])
  , ("\x000A9",["copy","COPY"])
  , ("\x000AA",["ordf"])
  , ("\x000AB",["laquo"])
  , ("\x000AC",["not"])
  , ("\x000AD",["shy"])
  , ("\x000AE",["reg","circledR","REG"])
  , ("\x000AF",["macr","OverBar","strns"])
  , ("\x000B0",["deg"])
  , ("\x000B1",["plusmn","pm","PlusMinus"])
  , ("\x000B2",["sup2"])
  , ("\x000B3",["sup3"])
  , ("\x000B4",["acute","DiacriticalAcute"])
  , ("\x000B5",["micro"])
  , ("\x000B6",["para"])
  , ("\x000B7",["middot","centerdot","CenterDot"])
  , ("\x000B8",["cedil","Cedilla"])
  , ("\x000B9",["sup1"])
  , ("\x000BA",["ordm"])
  , ("\x000BB",["raquo"])
  , ("\x000BC",["frac14"])
  , ("\x000BD",["frac12","half"])
  , ("\x000BE",["frac34"])
  , ("\x000BF",["iquest"])
  , ("\x000C0",["Agrave"])
  , ("\x000C1",["Aacute"])
  , ("\x000C2",["Acirc"])
  , ("\x000C3",["Atilde"])
  , ("\x000C4",["Auml"])
  , ("\x000C5",["Aring"])
  , ("\x000C6",["AElig"])
  , ("\x000C7",["Ccedil"])
  , ("\x000C8",["Egrave"])
  , ("\x000C9",["Eacute"])
  , ("\x000CA",["Ecirc"])
  , ("\x000CB",["Euml"])
  , ("\x000CC",["Igrave"])
  , ("\x000CD",["Iacute"])
  , ("\x000CE",["Icirc"])
  , ("\x000CF",["Iuml"])
  ]


{-# NOINLINE ent2names2 #-}
ent2names2 :: [(Text,[Text])]
ent2names2 =
  [ ("\x000D0",["ETH"])
  , ("\x000D1",["Ntilde"])
  , ("\x000D2",["Ograve"])
  , ("\x000D3",["Oacute"])
  , ("\x000D4",["Ocirc"])
  , ("\x000D5",["Otilde"])
  , ("\x000D6",["Ouml"])
  , ("\x000D7",["times"])
  , ("\x000D8",["Oslash"])
  , ("\x000D9",["Ugrave"])
  , ("\x000DA",["Uacute"])
  , ("\x000DB",["Ucirc"])
  , ("\x000DC",["Uuml"])
  , ("\x000DD",["Yacute"])
  , ("\x000DE",["THORN"])
  , ("\x000DF",["szlig"])
  , ("\x000E0",["agrave"])
  , ("\x000E1",["aacute"])
  , ("\x000E2",["acirc"])
  , ("\x000E3",["atilde"])
  , ("\x000E4",["auml"])
  , ("\x000E5",["aring"])
  , ("\x000E6",["aelig"])
  , ("\x000E7",["ccedil"])
  , ("\x000E8",["egrave"])
  , ("\x000E9",["eacute"])
  , ("\x000EA",["ecirc"])
  , ("\x000EB",["euml"])
  , ("\x000EC",["igrave"])
  , ("\x000ED",["iacute"])
  , ("\x000EE",["icirc"])
  , ("\x000EF",["iuml"])
  , ("\x000F0",["eth"])
  , ("\x000F1",["ntilde"])
  , ("\x000F2",["ograve"])
  , ("\x000F3",["oacute"])
  , ("\x000F4",["ocirc"])
  , ("\x000F5",["otilde"])
  , ("\x000F6",["ouml"])
  , ("\x000F7",["divide","div"])
  ]


{-# NOINLINE ent2names3 #-}
ent2names3 :: [(Text,[Text])]
ent2names3 =
  [ ("\x000F8",["oslash"])
  , ("\x000F9",["ugrave"])
  , ("\x000FA",["uacute"])
  , ("\x000FB",["ucirc"])
  , ("\x000FC",["uuml"])
  , ("\x000FD",["yacute"])
  , ("\x000FE",["thorn"])
  , ("\x000FF",["yuml"])
  , ("\x00100",["Amacr"])
  , ("\x00101",["amacr"])
  , ("\x00102",["Abreve"])
  , ("\x00103",["abreve"])
  , ("\x00104",["Aogon"])
  , ("\x00105",["aogon"])
  , ("\x00106",["Cacute"])
  , ("\x00107",["cacute"])
  , ("\x00108",["Ccirc"])
  , ("\x00109",["ccirc"])
  , ("\x0010A",["Cdot"])
  , ("\x0010B",["cdot"])
  , ("\x0010C",["Ccaron"])
  , ("\x0010D",["ccaron"])
  , ("\x0010E",["Dcaron"])
  , ("\x0010F",["dcaron"])
  , ("\x00110",["Dstrok"])
  , ("\x00111",["dstrok"])
  , ("\x00112",["Emacr"])
  , ("\x00113",["emacr"])
  , ("\x00116",["Edot"])
  , ("\x00117",["edot"])
  , ("\x00118",["Eogon"])
  , ("\x00119",["eogon"])
  , ("\x0011A",["Ecaron"])
  , ("\x0011B",["ecaron"])
  , ("\x0011C",["Gcirc"])
  , ("\x0011D",["gcirc"])
  , ("\x0011E",["Gbreve"])
  , ("\x0011F",["gbreve"])
  , ("\x00120",["Gdot"])
  , ("\x00121",["gdot"])
  ]


{-# NOINLINE ent2names4 #-}
ent2names4 :: [(Text,[Text])]
ent2names4 =
  [ ("\x00122",["Gcedil"])
  , ("\x00124",["Hcirc"])
  , ("\x00125",["hcirc"])
  , ("\x00126",["Hstrok"])
  , ("\x00127",["hstrok"])
  , ("\x00128",["Itilde"])
  , ("\x00129",["itilde"])
  , ("\x0012A",["Imacr"])
  , ("\x0012B",["imacr"])
  , ("\x0012E",["Iogon"])
  , ("\x0012F",["iogon"])
  , ("\x00130",["Idot"])
  , ("\x00131",["imath","inodot"])
  , ("\x00132",["IJlig"])
  , ("\x00133",["ijlig"])
  , ("\x00134",["Jcirc"])
  , ("\x00135",["jcirc"])
  , ("\x00136",["Kcedil"])
  , ("\x00137",["kcedil"])
  , ("\x00138",["kgreen"])
  , ("\x00139",["Lacute"])
  , ("\x0013A",["lacute"])
  , ("\x0013B",["Lcedil"])
  , ("\x0013C",["lcedil"])
  , ("\x0013D",["Lcaron"])
  , ("\x0013E",["lcaron"])
  , ("\x0013F",["Lmidot"])
  , ("\x00140",["lmidot"])
  , ("\x00141",["Lstrok"])
  , ("\x00142",["lstrok"])
  , ("\x00143",["Nacute"])
  , ("\x00144",["nacute"])
  , ("\x00145",["Ncedil"])
  , ("\x00146",["ncedil"])
  , ("\x00147",["Ncaron"])
  , ("\x00148",["ncaron"])
  , ("\x00149",["napos"])
  , ("\x0014A",["ENG"])
  , ("\x0014B",["eng"])
  , ("\x0014C",["Omacr"])
  ]


{-# NOINLINE ent2names5 #-}
ent2names5 :: [(Text,[Text])]
ent2names5 =
  [ ("\x0014D",["omacr"])
  , ("\x00150",["Odblac"])
  , ("\x00151",["odblac"])
  , ("\x00152",["OElig"])
  , ("\x00153",["oelig"])
  , ("\x00154",["Racute"])
  , ("\x00155",["racute"])
  , ("\x00156",["Rcedil"])
  , ("\x00157",["rcedil"])
  , ("\x00158",["Rcaron"])
  , ("\x00159",["rcaron"])
  , ("\x0015A",["Sacute"])
  , ("\x0015B",["sacute"])
  , ("\x0015C",["Scirc"])
  , ("\x0015D",["scirc"])
  , ("\x0015E",["Scedil"])
  , ("\x0015F",["scedil"])
  , ("\x00160",["Scaron"])
  , ("\x00161",["scaron"])
  , ("\x00162",["Tcedil"])
  , ("\x00163",["tcedil"])
  , ("\x00164",["Tcaron"])
  , ("\x00165",["tcaron"])
  , ("\x00166",["Tstrok"])
  , ("\x00167",["tstrok"])
  , ("\x00168",["Utilde"])
  , ("\x00169",["utilde"])
  , ("\x0016A",["Umacr"])
  , ("\x0016B",["umacr"])
  , ("\x0016C",["Ubreve"])
  , ("\x0016D",["ubreve"])
  , ("\x0016E",["Uring"])
  , ("\x0016F",["uring"])
  , ("\x00170",["Udblac"])
  , ("\x00171",["udblac"])
  , ("\x00172",["Uogon"])
  , ("\x00173",["uogon"])
  , ("\x00174",["Wcirc"])
  , ("\x00175",["wcirc"])
  , ("\x00176",["Ycirc"])
  ]


{-# NOINLINE ent2names6 #-}
ent2names6 :: [(Text,[Text])]
ent2names6 =
  [ ("\x00177",["ycirc"])
  , ("\x00178",["Yuml"])
  , ("\x00179",["Zacute"])
  , ("\x0017A",["zacute"])
  , ("\x0017B",["Zdot"])
  , ("\x0017C",["zdot"])
  , ("\x0017D",["Zcaron"])
  , ("\x0017E",["zcaron"])
  , ("\x00192",["fnof"])
  , ("\x001B5",["imped"])
  , ("\x001F5",["gacute"])
  , ("\x00237",["jmath"])
  , ("\x002C6",["circ"])
  , ("\x002C7",["caron","Hacek"])
  , ("\x002D8",["breve","Breve"])
  , ("\x002D9",["dot","DiacriticalDot"])
  , ("\x002DA",["ring"])
  , ("\x002DB",["ogon"])
  , ("\x002DC",["tilde","DiacriticalTilde"])
  , ("\x002DD",["dblac","DiacriticalDoubleAcute"])
  , ("\x00311",["DownBreve"])
  , ("\x00332",["UnderBar"])
  , ("\x00391",["Alpha"])
  , ("\x00392",["Beta"])
  , ("\x00393",["Gamma"])
  , ("\x00394",["Delta"])
  , ("\x00395",["Epsilon"])
  , ("\x00396",["Zeta"])
  , ("\x00397",["Eta"])
  , ("\x00398",["Theta"])
  , ("\x00399",["Iota"])
  , ("\x0039A",["Kappa"])
  , ("\x0039B",["Lambda"])
  , ("\x0039C",["Mu"])
  , ("\x0039D",["Nu"])
  , ("\x0039E",["Xi"])
  , ("\x0039F",["Omicron"])
  , ("\x003A0",["Pi"])
  , ("\x003A1",["Rho"])
  , ("\x003A3",["Sigma"])
  ]


{-# NOINLINE ent2names7 #-}
ent2names7 :: [(Text,[Text])]
ent2names7 =
  [ ("\x003A4",["Tau"])
  , ("\x003A5",["Upsilon"])
  , ("\x003A6",["Phi"])
  , ("\x003A7",["Chi"])
  , ("\x003A8",["Psi"])
  , ("\x003A9",["Omega"])
  , ("\x003B1",["alpha"])
  , ("\x003B2",["beta"])
  , ("\x003B3",["gamma"])
  , ("\x003B4",["delta"])
  , ("\x003B5",["epsiv","varepsilon","epsilon"])
  , ("\x003B6",["zeta"])
  , ("\x003B7",["eta"])
  , ("\x003B8",["theta"])
  , ("\x003B9",["iota"])
  , ("\x003BA",["kappa"])
  , ("\x003BB",["lambda"])
  , ("\x003BC",["mu"])
  , ("\x003BD",["nu"])
  , ("\x003BE",["xi"])
  , ("\x003BF",["omicron"])
  , ("\x003C0",["pi"])
  , ("\x003C1",["rho"])
  , ("\x003C2",["sigmav","varsigma","sigmaf"])
  , ("\x003C3",["sigma"])
  , ("\x003C4",["tau"])
  , ("\x003C5",["upsi","upsilon"])
  , ("\x003C6",["phi","phiv","varphi"])
  , ("\x003C7",["chi"])
  , ("\x003C8",["psi"])
  , ("\x003C9",["omega"])
  , ("\x003D1",["thetav","vartheta","thetasym"])
  , ("\x003D2",["Upsi","upsih"])
  , ("\x003D5",["straightphi"])
  , ("\x003D6",["piv","varpi"])
  , ("\x003DC",["Gammad"])
  , ("\x003DD",["gammad","digamma"])
  , ("\x003F0",["kappav","varkappa"])
  , ("\x003F1",["rhov","varrho"])
  , ("\x003F5",["epsi","straightepsilon"])
  ]


{-# NOINLINE ent2names8 #-}
ent2names8 :: [(Text,[Text])]
ent2names8 =
  [ ("\x003F6",["bepsi","backepsilon"])
  , ("\x00401",["IOcy"])
  , ("\x00402",["DJcy"])
  , ("\x00403",["GJcy"])
  , ("\x00404",["Jukcy"])
  , ("\x00405",["DScy"])
  , ("\x00406",["Iukcy"])
  , ("\x00407",["YIcy"])
  , ("\x00408",["Jsercy"])
  , ("\x00409",["LJcy"])
  , ("\x0040A",["NJcy"])
  , ("\x0040B",["TSHcy"])
  , ("\x0040C",["KJcy"])
  , ("\x0040E",["Ubrcy"])
  , ("\x0040F",["DZcy"])
  , ("\x00410",["Acy"])
  , ("\x00411",["Bcy"])
  , ("\x00412",["Vcy"])
  , ("\x00413",["Gcy"])
  , ("\x00414",["Dcy"])
  , ("\x00415",["IEcy"])
  , ("\x00416",["ZHcy"])
  , ("\x00417",["Zcy"])
  , ("\x00418",["Icy"])
  , ("\x00419",["Jcy"])
  , ("\x0041A",["Kcy"])
  , ("\x0041B",["Lcy"])
  , ("\x0041C",["Mcy"])
  , ("\x0041D",["Ncy"])
  , ("\x0041E",["Ocy"])
  , ("\x0041F",["Pcy"])
  , ("\x00420",["Rcy"])
  , ("\x00421",["Scy"])
  , ("\x00422",["Tcy"])
  , ("\x00423",["Ucy"])
  , ("\x00424",["Fcy"])
  , ("\x00425",["KHcy"])
  , ("\x00426",["TScy"])
  , ("\x00427",["CHcy"])
  , ("\x00428",["SHcy"])
  ]


{-# NOINLINE ent2names9 #-}
ent2names9 :: [(Text,[Text])]
ent2names9 =
  [ ("\x00429",["SHCHcy"])
  , ("\x0042A",["HARDcy"])
  , ("\x0042B",["Ycy"])
  , ("\x0042C",["SOFTcy"])
  , ("\x0042D",["Ecy"])
  , ("\x0042E",["YUcy"])
  , ("\x0042F",["YAcy"])
  , ("\x00430",["acy"])
  , ("\x00431",["bcy"])
  , ("\x00432",["vcy"])
  , ("\x00433",["gcy"])
  , ("\x00434",["dcy"])
  , ("\x00435",["iecy"])
  , ("\x00436",["zhcy"])
  , ("\x00437",["zcy"])
  , ("\x00438",["icy"])
  , ("\x00439",["jcy"])
  , ("\x0043A",["kcy"])
  , ("\x0043B",["lcy"])
  , ("\x0043C",["mcy"])
  , ("\x0043D",["ncy"])
  , ("\x0043E",["ocy"])
  , ("\x0043F",["pcy"])
  , ("\x00440",["rcy"])
  , ("\x00441",["scy"])
  , ("\x00442",["tcy"])
  , ("\x00443",["ucy"])
  , ("\x00444",["fcy"])
  , ("\x00445",["khcy"])
  , ("\x00446",["tscy"])
  , ("\x00447",["chcy"])
  , ("\x00448",["shcy"])
  , ("\x00449",["shchcy"])
  , ("\x0044A",["hardcy"])
  , ("\x0044B",["ycy"])
  , ("\x0044C",["softcy"])
  , ("\x0044D",["ecy"])
  , ("\x0044E",["yucy"])
  , ("\x0044F",["yacy"])
  , ("\x00451",["iocy"])
  ]


{-# NOINLINE ent2names10 #-}
ent2names10 :: [(Text,[Text])]
ent2names10 =
  [ ("\x00452",["djcy"])
  , ("\x00453",["gjcy"])
  , ("\x00454",["jukcy"])
  , ("\x00455",["dscy"])
  , ("\x00456",["iukcy"])
  , ("\x00457",["yicy"])
  , ("\x00458",["jsercy"])
  , ("\x00459",["ljcy"])
  , ("\x0045A",["njcy"])
  , ("\x0045B",["tshcy"])
  , ("\x0045C",["kjcy"])
  , ("\x0045E",["ubrcy"])
  , ("\x0045F",["dzcy"])
  , ("\x02002",["ensp"])
  , ("\x02003",["emsp"])
  , ("\x02004",["emsp13"])
  , ("\x02005",["emsp14"])
  , ("\x02007",["numsp"])
  , ("\x02008",["puncsp"])
  , ("\x02009",["thinsp","ThinSpace"])
  , ("\x0200A",["hairsp","VeryThinSpace"])
  , ("\x0200B",["ZeroWidthSpace","NegativeVeryThinSpace","NegativeThinSpace","NegativeMediumSpace","NegativeThickSpace"])
  , ("\x0200C",["zwnj"])
  , ("\x0200D",["zwj"])
  , ("\x0200E",["lrm"])
  , ("\x0200F",["rlm"])
  , ("\x02010",["hyphen","dash"])
  , ("\x02013",["ndash"])
  , ("\x02014",["mdash"])
  , ("\x02015",["horbar"])
  , ("\x02016",["Verbar","Vert"])
  , ("\x02018",["lsquo","OpenCurlyQuote"])
  , ("\x02019",["rsquo","rsquor","CloseCurlyQuote"])
  , ("\x0201A",["lsquor","sbquo"])
  , ("\x0201C",["ldquo","OpenCurlyDoubleQuote"])
  , ("\x0201D",["rdquo","rdquor","CloseCurlyDoubleQuote"])
  , ("\x0201E",["ldquor","bdquo"])
  , ("\x02020",["dagger"])
  , ("\x02021",["Dagger","ddagger"])
  , ("\x02022",["bull","bullet"])
  ]


{-# NOINLINE ent2names11 #-}
ent2names11 :: [(Text,[Text])]
ent2names11 =
  [ ("\x02025",["nldr"])
  , ("\x02026",["hellip","mldr"])
  , ("\x02030",["permil"])
  , ("\x02031",["pertenk"])
  , ("\x02032",["prime"])
  , ("\x02033",["Prime"])
  , ("\x02034",["tprime"])
  , ("\x02035",["bprime","backprime"])
  , ("\x02039",["lsaquo"])
  , ("\x0203A",["rsaquo"])
  , ("\x0203E",["oline"])
  , ("\x02041",["caret"])
  , ("\x02043",["hybull"])
  , ("\x02044",["frasl"])
  , ("\x0204F",["bsemi"])
  , ("\x02057",["qprime"])
  , ("\x0205F",["MediumSpace"])
  , ("\x02060",["NoBreak"])
  , ("\x02061",["ApplyFunction","af"])
  , ("\x02062",["InvisibleTimes","it"])
  , ("\x02063",["InvisibleComma","ic"])
  , ("\x020AC",["euro"])
  , ("\x020DB",["tdot","TripleDot"])
  , ("\x020DC",["DotDot"])
  , ("\x02102",["Copf","complexes"])
  , ("\x02105",["incare"])
  , ("\x0210A",["gscr"])
  , ("\x0210B",["hamilt","HilbertSpace","Hscr"])
  , ("\x0210C",["Hfr","Poincareplane"])
  , ("\x0210D",["quaternions","Hopf"])
  , ("\x0210E",["planckh"])
  , ("\x0210F",["planck","hbar","plankv","hslash"])
  , ("\x02110",["Iscr","imagline"])
  , ("\x02111",["image","Im","imagpart","Ifr"])
  , ("\x02112",["Lscr","lagran","Laplacetrf"])
  , ("\x02113",["ell"])
  , ("\x02115",["Nopf","naturals"])
  , ("\x02116",["numero"])
  , ("\x02117",["copysr"])
  , ("\x02118",["weierp","wp"])
  ]


{-# NOINLINE ent2names12 #-}
ent2names12 :: [(Text,[Text])]
ent2names12 =
  [ ("\x02119",["Popf","primes"])
  , ("\x0211A",["rationals","Qopf"])
  , ("\x0211B",["Rscr","realine"])
  , ("\x0211C",["real","Re","realpart","Rfr"])
  , ("\x0211D",["reals","Ropf"])
  , ("\x0211E",["rx"])
  , ("\x02122",["trade","TRADE"])
  , ("\x02124",["integers","Zopf"])
  , ("\x02126",["ohm"])
  , ("\x02127",["mho"])
  , ("\x02128",["Zfr","zeetrf"])
  , ("\x02129",["iiota"])
  , ("\x0212B",["angst"])
  , ("\x0212C",["bernou","Bernoullis","Bscr"])
  , ("\x0212D",["Cfr","Cayleys"])
  , ("\x0212F",["escr"])
  , ("\x02130",["Escr","expectation"])
  , ("\x02131",["Fscr","Fouriertrf"])
  , ("\x02133",["phmmat","Mellintrf","Mscr"])
  , ("\x02134",["order","orderof","oscr"])
  , ("\x02135",["alefsym","aleph"])
  , ("\x02136",["beth"])
  , ("\x02137",["gimel"])
  , ("\x02138",["daleth"])
  , ("\x02145",["CapitalDifferentialD","DD"])
  , ("\x02146",["DifferentialD","dd"])
  , ("\x02147",["ExponentialE","exponentiale","ee"])
  , ("\x02148",["ImaginaryI","ii"])
  , ("\x02153",["frac13"])
  , ("\x02154",["frac23"])
  , ("\x02155",["frac15"])
  , ("\x02156",["frac25"])
  , ("\x02157",["frac35"])
  , ("\x02158",["frac45"])
  , ("\x02159",["frac16"])
  , ("\x0215A",["frac56"])
  , ("\x0215B",["frac18"])
  , ("\x0215C",["frac38"])
  , ("\x0215D",["frac58"])
  , ("\x0215E",["frac78"])
  ]


{-# NOINLINE ent2names13 #-}
ent2names13 :: [(Text,[Text])]
ent2names13 =
  [ ("\x02190",["larr","leftarrow","LeftArrow","slarr","ShortLeftArrow"])
  , ("\x02191",["uarr","uparrow","UpArrow","ShortUpArrow"])
  , ("\x02192",["rarr","rightarrow","RightArrow","srarr","ShortRightArrow"])
  , ("\x02193",["darr","downarrow","DownArrow","ShortDownArrow"])
  , ("\x02194",["harr","leftrightarrow","LeftRightArrow"])
  , ("\x02195",["varr","updownarrow","UpDownArrow"])
  , ("\x02196",["nwarr","UpperLeftArrow","nwarrow"])
  , ("\x02197",["nearr","UpperRightArrow","nearrow"])
  , ("\x02198",["searr","searrow","LowerRightArrow"])
  , ("\x02199",["swarr","swarrow","LowerLeftArrow"])
  , ("\x0219A",["nlarr","nleftarrow"])
  , ("\x0219B",["nrarr","nrightarrow"])
  , ("\x0219D",["rarrw","rightsquigarrow"])
  , ("\x0219E",["Larr","twoheadleftarrow"])
  , ("\x0219F",["Uarr"])
  , ("\x021A0",["Rarr","twoheadrightarrow"])
  , ("\x021A1",["Darr"])
  , ("\x021A2",["larrtl","leftarrowtail"])
  , ("\x021A3",["rarrtl","rightarrowtail"])
  , ("\x021A4",["LeftTeeArrow","mapstoleft"])
  , ("\x021A5",["UpTeeArrow","mapstoup"])
  , ("\x021A6",["map","RightTeeArrow","mapsto"])
  , ("\x021A7",["DownTeeArrow","mapstodown"])
  , ("\x021A9",["larrhk","hookleftarrow"])
  , ("\x021AA",["rarrhk","hookrightarrow"])
  , ("\x021AB",["larrlp","looparrowleft"])
  , ("\x021AC",["rarrlp","looparrowright"])
  , ("\x021AD",["harrw","leftrightsquigarrow"])
  , ("\x021AE",["nharr","nleftrightarrow"])
  , ("\x021B0",["lsh","Lsh"])
  , ("\x021B1",["rsh","Rsh"])
  , ("\x021B2",["ldsh"])
  , ("\x021B3",["rdsh"])
  , ("\x021B5",["crarr"])
  , ("\x021B6",["cularr","curvearrowleft"])
  , ("\x021B7",["curarr","curvearrowright"])
  , ("\x021BA",["olarr","circlearrowleft"])
  , ("\x021BB",["orarr","circlearrowright"])
  , ("\x021BC",["lharu","LeftVector","leftharpoonup"])
  , ("\x021BD",["lhard","leftharpoondown","DownLeftVector"])
  ]


{-# NOINLINE ent2names14 #-}
ent2names14 :: [(Text,[Text])]
ent2names14 =
  [ ("\x021BE",["uharr","upharpoonright","RightUpVector"])
  , ("\x021BF",["uharl","upharpoonleft","LeftUpVector"])
  , ("\x021C0",["rharu","RightVector","rightharpoonup"])
  , ("\x021C1",["rhard","rightharpoondown","DownRightVector"])
  , ("\x021C2",["dharr","RightDownVector","downharpoonright"])
  , ("\x021C3",["dharl","LeftDownVector","downharpoonleft"])
  , ("\x021C4",["rlarr","rightleftarrows","RightArrowLeftArrow"])
  , ("\x021C5",["udarr","UpArrowDownArrow"])
  , ("\x021C6",["lrarr","leftrightarrows","LeftArrowRightArrow"])
  , ("\x021C7",["llarr","leftleftarrows"])
  , ("\x021C8",["uuarr","upuparrows"])
  , ("\x021C9",["rrarr","rightrightarrows"])
  , ("\x021CA",["ddarr","downdownarrows"])
  , ("\x021CB",["lrhar","ReverseEquilibrium","leftrightharpoons"])
  , ("\x021CC",["rlhar","rightleftharpoons","Equilibrium"])
  , ("\x021CD",["nlArr","nLeftarrow"])
  , ("\x021CE",["nhArr","nLeftrightarrow"])
  , ("\x021CF",["nrArr","nRightarrow"])
  , ("\x021D0",["lArr","Leftarrow","DoubleLeftArrow"])
  , ("\x021D1",["uArr","Uparrow","DoubleUpArrow"])
  , ("\x021D2",["rArr","Rightarrow","Implies","DoubleRightArrow"])
  , ("\x021D3",["dArr","Downarrow","DoubleDownArrow"])
  , ("\x021D4",["hArr","Leftrightarrow","DoubleLeftRightArrow","iff"])
  , ("\x021D5",["vArr","Updownarrow","DoubleUpDownArrow"])
  , ("\x021D6",["nwArr"])
  , ("\x021D7",["neArr"])
  , ("\x021D8",["seArr"])
  , ("\x021D9",["swArr"])
  , ("\x021DA",["lAarr","Lleftarrow"])
  , ("\x021DB",["rAarr","Rrightarrow"])
  , ("\x021DD",["zigrarr"])
  , ("\x021E4",["larrb","LeftArrowBar"])
  , ("\x021E5",["rarrb","RightArrowBar"])
  , ("\x021F5",["duarr","DownArrowUpArrow"])
  , ("\x021FD",["loarr"])
  , ("\x021FE",["roarr"])
  , ("\x021FF",["hoarr"])
  , ("\x02200",["forall","ForAll"])
  , ("\x02201",["comp","complement"])
  , ("\x02202",["part","PartialD"])
  ]


{-# NOINLINE ent2names15 #-}
ent2names15 :: [(Text,[Text])]
ent2names15 =
  [ ("\x02203",["exist","Exists"])
  , ("\x02204",["nexist","NotExists","nexists"])
  , ("\x02205",["empty","emptyset","emptyv","varnothing"])
  , ("\x02207",["nabla","Del"])
  , ("\x02208",["isin","isinv","Element","in"])
  , ("\x02209",["notin","NotElement","notinva"])
  , ("\x0220B",["niv","ReverseElement","ni","SuchThat"])
  , ("\x0220C",["notni","notniva","NotReverseElement"])
  , ("\x0220F",["prod","Product"])
  , ("\x02210",["coprod","Coproduct"])
  , ("\x02211",["sum","Sum"])
  , ("\x02212",["minus"])
  , ("\x02213",["mnplus","mp","MinusPlus"])
  , ("\x02214",["plusdo","dotplus"])
  , ("\x02216",["setmn","setminus","Backslash","ssetmn","smallsetminus"])
  , ("\x02217",["lowast"])
  , ("\x02218",["compfn","SmallCircle"])
  , ("\x0221A",["radic","Sqrt"])
  , ("\x0221D",["prop","propto","Proportional","vprop","varpropto"])
  , ("\x0221E",["infin"])
  , ("\x0221F",["angrt"])
  , ("\x02220",["ang","angle"])
  , ("\x02221",["angmsd","measuredangle"])
  , ("\x02222",["angsph"])
  , ("\x02223",["mid","VerticalBar","smid","shortmid"])
  , ("\x02224",["nmid","NotVerticalBar","nsmid","nshortmid"])
  , ("\x02225",["par","parallel","DoubleVerticalBar","spar","shortparallel"])
  , ("\x02226",["npar","nparallel","NotDoubleVerticalBar","nspar","nshortparallel"])
  , ("\x02227",["and","wedge"])
  , ("\x02228",["or","vee"])
  , ("\x02229",["cap"])
  , ("\x0222A",["cup"])
  , ("\x0222B",["int","Integral"])
  , ("\x0222C",["Int"])
  , ("\x0222D",["tint","iiint"])
  , ("\x0222E",["conint","oint","ContourIntegral"])
  , ("\x0222F",["Conint","DoubleContourIntegral"])
  , ("\x02230",["Cconint"])
  , ("\x02231",["cwint"])
  , ("\x02232",["cwconint","ClockwiseContourIntegral"])
  ]


{-# NOINLINE ent2names16 #-}
ent2names16 :: [(Text,[Text])]
ent2names16 =
  [ ("\x02233",["awconint","CounterClockwiseContourIntegral"])
  , ("\x02234",["there4","therefore","Therefore"])
  , ("\x02235",["becaus","because","Because"])
  , ("\x02236",["ratio"])
  , ("\x02237",["Colon","Proportion"])
  , ("\x02238",["minusd","dotminus"])
  , ("\x0223A",["mDDot"])
  , ("\x0223B",["homtht"])
  , ("\x0223C",["sim","Tilde","thksim","thicksim"])
  , ("\x0223D",["bsim","backsim"])
  , ("\x0223E",["ac","mstpos"])
  , ("\x0223F",["acd"])
  , ("\x02240",["wreath","VerticalTilde","wr"])
  , ("\x02241",["nsim","NotTilde"])
  , ("\x02242",["esim","EqualTilde","eqsim"])
  , ("\x02243",["sime","TildeEqual","simeq"])
  , ("\x02244",["nsime","nsimeq","NotTildeEqual"])
  , ("\x02245",["cong","TildeFullEqual"])
  , ("\x02246",["simne"])
  , ("\x02247",["ncong","NotTildeFullEqual"])
  , ("\x02248",["asymp","ap","TildeTilde","approx","thkap","thickapprox"])
  , ("\x02249",["nap","NotTildeTilde","napprox"])
  , ("\x0224A",["ape","approxeq"])
  , ("\x0224B",["apid"])
  , ("\x0224C",["bcong","backcong"])
  , ("\x0224D",["asympeq","CupCap"])
  , ("\x0224E",["bump","HumpDownHump","Bumpeq"])
  , ("\x0224F",["bumpe","HumpEqual","bumpeq"])
  , ("\x02250",["esdot","DotEqual","doteq"])
  , ("\x02251",["eDot","doteqdot"])
  , ("\x02252",["efDot","fallingdotseq"])
  , ("\x02253",["erDot","risingdotseq"])
  , ("\x02254",["colone","coloneq","Assign"])
  , ("\x02255",["ecolon","eqcolon"])
  , ("\x02256",["ecir","eqcirc"])
  , ("\x02257",["cire","circeq"])
  , ("\x02259",["wedgeq"])
  , ("\x0225A",["veeeq"])
  , ("\x0225C",["trie","triangleq"])
  , ("\x0225F",["equest","questeq"])
  ]


{-# NOINLINE ent2names17 #-}
ent2names17 :: [(Text,[Text])]
ent2names17 =
  [ ("\x02260",["ne","NotEqual"])
  , ("\x02242\x00338", ["NotEqualTilde"])
  , ("\x02261",["equiv","Congruent"])
  , ("\x02262",["nequiv","NotCongruent"])
  , ("\x02264",["le","leq"])
  , ("\x02265",["ge","GreaterEqual","geq"])
  , ("\x02266",["lE","LessFullEqual","leqq"])
  , ("\x02267",["gE","GreaterFullEqual","geqq"])
  , ("\x02268",["lnE","lneqq"])
  , ("\x02269",["gnE","gneqq"])
  , ("\x0226A",["Lt","NestedLessLess","ll"])
  , ("\x0226B",["Gt","NestedGreaterGreater","gg"])
  , ("\x0226C",["twixt","between"])
  , ("\x0226D",["NotCupCap"])
  , ("\x0226E",["nlt","NotLess","nless"])
  , ("\x0226F",["ngt","NotGreater","ngtr"])
  , ("\x02270",["nle","NotLessEqual","nleq"])
  , ("\x02271",["nge","NotGreaterEqual","ngeq"])
  , ("\x02267\x00338",["NotGreaterFullEqual"])
  , ("\x02272",["lsim","LessTilde","lesssim"])
  , ("\x02273",["gsim","gtrsim","GreaterTilde"])
  , ("\x02274",["nlsim","NotLessTilde"])
  , ("\x02275",["ngsim","NotGreaterTilde"])
  , ("\x02276",["lg","lessgtr","LessGreater"])
  , ("\x02277",["gl","gtrless","GreaterLess"])
  , ("\x02278",["ntlg","NotLessGreater"])
  , ("\x02279",["ntgl","NotGreaterLess"])
  , ("\x0227A",["pr","Precedes","prec"])
  , ("\x0227B",["sc","Succeeds","succ"])
  , ("\x0227C",["prcue","PrecedesSlantEqual","preccurlyeq"])
  , ("\x0227D",["sccue","SucceedsSlantEqual","succcurlyeq"])
  , ("\x0227E",["prsim","precsim","PrecedesTilde"])
  , ("\x0227F",["scsim","succsim","SucceedsTilde"])
  , ("\x02280",["npr","nprec","NotPrecedes"])
  , ("\x02281",["nsc","nsucc","NotSucceeds"])
  , ("\x02282",["sub","subset"])
  , ("\x02283",["sup","supset","Superset"])
  , ("\x02284",["nsub"])
  , ("\x02285",["nsup"])
  , ("\x02286",["sube","SubsetEqual","subseteq"])
  , ("\x02287",["supe","supseteq","SupersetEqual"])
  , ("\x02288",["nsube","nsubseteq","NotSubsetEqual"])
  ]


{-# NOINLINE ent2names18 #-}
ent2names18 :: [(Text,[Text])]
ent2names18 =
  [ ("\x02289",["nsupe","nsupseteq","NotSupersetEqual"])
  , ("\x0228A",["subne","subsetneq"])
  , ("\x0228B",["supne","supsetneq"])
  , ("\x0228D",["cupdot"])
  , ("\x0228E",["uplus","UnionPlus"])
  , ("\x0228F",["sqsub","SquareSubset","sqsubset"])
  , ("\x02290",["sqsup","SquareSuperset","sqsupset"])
  , ("\x02291",["sqsube","SquareSubsetEqual","sqsubseteq"])
  , ("\x02292",["sqsupe","SquareSupersetEqual","sqsupseteq"])
  , ("\x02293",["sqcap","SquareIntersection"])
  , ("\x02294",["sqcup","SquareUnion"])
  , ("\x02295",["oplus","CirclePlus"])
  , ("\x02296",["ominus","CircleMinus"])
  , ("\x02297",["otimes","CircleTimes"])
  , ("\x02298",["osol"])
  , ("\x02299",["odot","CircleDot"])
  , ("\x0229A",["ocir","circledcirc"])
  , ("\x0229B",["oast","circledast"])
  , ("\x0229D",["odash","circleddash"])
  , ("\x0229E",["plusb","boxplus"])
  , ("\x0229F",["minusb","boxminus"])
  , ("\x022A0",["timesb","boxtimes"])
  , ("\x022A1",["sdotb","dotsquare"])
  , ("\x022A2",["vdash","RightTee"])
  , ("\x022A3",["dashv","LeftTee"])
  , ("\x022A4",["top","DownTee"])
  , ("\x022A5",["bottom","bot","perp","UpTee"])
  , ("\x022A7",["models"])
  , ("\x022A8",["vDash","DoubleRightTee"])
  , ("\x022A9",["Vdash"])
  , ("\x022AA",["Vvdash"])
  , ("\x022AB",["VDash"])
  , ("\x022AC",["nvdash"])
  , ("\x022AD",["nvDash"])
  , ("\x022AE",["nVdash"])
  , ("\x022AF",["nVDash"])
  , ("\x022B0",["prurel"])
  , ("\x022B2",["vltri","vartriangleleft","LeftTriangle"])
  , ("\x022B3",["vrtri","vartriangleright","RightTriangle"])
  , ("\x022B4",["ltrie","trianglelefteq","LeftTriangleEqual"])
  ]


{-# NOINLINE ent2names19 #-}
ent2names19 :: [(Text,[Text])]
ent2names19 =
  [ ("\x022B5",["rtrie","trianglerighteq","RightTriangleEqual"])
  , ("\x022B6",["origof"])
  , ("\x022B7",["imof"])
  , ("\x022B8",["mumap","multimap"])
  , ("\x022B9",["hercon"])
  , ("\x022BA",["intcal","intercal"])
  , ("\x022BB",["veebar"])
  , ("\x022BD",["barvee"])
  , ("\x022BE",["angrtvb"])
  , ("\x022BF",["lrtri"])
  , ("\x022C0",["xwedge","Wedge","bigwedge"])
  , ("\x022C1",["xvee","Vee","bigvee"])
  , ("\x022C2",["xcap","Intersection","bigcap"])
  , ("\x022C3",["xcup","Union","bigcup"])
  , ("\x022C4",["diam","diamond","Diamond"])
  , ("\x022C5",["sdot"])
  , ("\x022C6",["sstarf","Star"])
  , ("\x022C7",["divonx","divideontimes"])
  , ("\x022C8",["bowtie"])
  , ("\x022C9",["ltimes"])
  , ("\x022CA",["rtimes"])
  , ("\x022CB",["lthree","leftthreetimes"])
  , ("\x022CC",["rthree","rightthreetimes"])
  , ("\x022CD",["bsime","backsimeq"])
  , ("\x022CE",["cuvee","curlyvee"])
  , ("\x022CF",["cuwed","curlywedge"])
  , ("\x022D0",["Sub","Subset"])
  , ("\x022D1",["Sup","Supset"])
  , ("\x022D2",["Cap"])
  , ("\x022D3",["Cup"])
  , ("\x022D4",["fork","pitchfork"])
  , ("\x022D5",["epar"])
  , ("\x022D6",["ltdot","lessdot"])
  , ("\x022D7",["gtdot","gtrdot"])
  , ("\x022D8",["Ll"])
  , ("\x022D9",["Gg","ggg"])
  , ("\x022DA",["leg","LessEqualGreater","lesseqgtr"])
  , ("\x022DB",["gel","gtreqless","GreaterEqualLess"])
  , ("\x022DE",["cuepr","curlyeqprec"])
  , ("\x022DF",["cuesc","curlyeqsucc"])
  ]


{-# NOINLINE ent2names20 #-}
ent2names20 :: [(Text,[Text])]
ent2names20 =
  [ ("\x022E0",["nprcue","NotPrecedesSlantEqual"])
  , ("\x022E1",["nsccue","NotSucceedsSlantEqual"])
  , ("\x022E2",["nsqsube","NotSquareSubsetEqual"])
  , ("\x022E3",["nsqsupe","NotSquareSupersetEqual"])
  , ("\x022E6",["lnsim"])
  , ("\x022E7",["gnsim"])
  , ("\x022E8",["prnsim","precnsim"])
  , ("\x022E9",["scnsim","succnsim"])
  , ("\x022EA",["nltri","ntriangleleft","NotLeftTriangle"])
  , ("\x022EB",["nrtri","ntriangleright","NotRightTriangle"])
  , ("\x022EC",["nltrie","ntrianglelefteq","NotLeftTriangleEqual"])
  , ("\x022ED",["nrtrie","ntrianglerighteq","NotRightTriangleEqual"])
  , ("\x022EE",["vellip"])
  , ("\x022EF",["ctdot"])
  , ("\x022F0",["utdot"])
  , ("\x022F1",["dtdot"])
  , ("\x022F2",["disin"])
  , ("\x022F3",["isinsv"])
  , ("\x022F4",["isins"])
  , ("\x022F5",["isindot"])
  , ("\x022F6",["notinvc"])
  , ("\x022F7",["notinvb"])
  , ("\x022F9",["isinE"])
  , ("\x022FA",["nisd"])
  , ("\x022FB",["xnis"])
  , ("\x022FC",["nis"])
  , ("\x022FD",["notnivc"])
  , ("\x022FE",["notnivb"])
  , ("\x02305",["barwed","barwedge"])
  , ("\x02306",["Barwed","doublebarwedge"])
  , ("\x02308",["lceil","LeftCeiling"])
  , ("\x02309",["rceil","RightCeiling"])
  , ("\x0230A",["lfloor","LeftFloor"])
  , ("\x0230B",["rfloor","RightFloor"])
  , ("\x0230C",["drcrop"])
  , ("\x0230D",["dlcrop"])
  , ("\x0230E",["urcrop"])
  , ("\x0230F",["ulcrop"])
  , ("\x02310",["bnot"])
  , ("\x02312",["profline"])
  ]


{-# NOINLINE ent2names21 #-}
ent2names21 :: [(Text,[Text])]
ent2names21 =
  [ ("\x02313",["profsurf"])
  , ("\x02315",["telrec"])
  , ("\x02316",["target"])
  , ("\x0231C",["ulcorn","ulcorner"])
  , ("\x0231D",["urcorn","urcorner"])
  , ("\x0231E",["dlcorn","llcorner"])
  , ("\x0231F",["drcorn","lrcorner"])
  , ("\x02322",["frown","sfrown"])
  , ("\x02323",["smile","ssmile"])
  , ("\x0232D",["cylcty"])
  , ("\x0232E",["profalar"])
  , ("\x02336",["topbot"])
  , ("\x0233D",["ovbar"])
  , ("\x0233F",["solbar"])
  , ("\x0237C",["angzarr"])
  , ("\x023B0",["lmoust","lmoustache"])
  , ("\x023B1",["rmoust","rmoustache"])
  , ("\x023B4",["tbrk","OverBracket"])
  , ("\x023B5",["bbrk","UnderBracket"])
  , ("\x023B6",["bbrktbrk"])
  , ("\x023DC",["OverParenthesis"])
  , ("\x023DD",["UnderParenthesis"])
  , ("\x023DE",["OverBrace"])
  , ("\x023DF",["UnderBrace"])
  , ("\x023E2",["trpezium"])
  , ("\x023E7",["elinters"])
  , ("\x02423",["blank"])
  , ("\x024C8",["oS","circledS"])
  , ("\x02500",["boxh","HorizontalLine"])
  , ("\x02502",["boxv"])
  , ("\x0250C",["boxdr"])
  , ("\x02510",["boxdl"])
  , ("\x02514",["boxur"])
  , ("\x02518",["boxul"])
  , ("\x0251C",["boxvr"])
  , ("\x02524",["boxvl"])
  , ("\x0252C",["boxhd"])
  , ("\x02534",["boxhu"])
  , ("\x0253C",["boxvh"])
  , ("\x02550",["boxH"])
  ]


{-# NOINLINE ent2names22 #-}
ent2names22 :: [(Text,[Text])]
ent2names22 =
  [ ("\x02551",["boxV"])
  , ("\x02552",["boxdR"])
  , ("\x02553",["boxDr"])
  , ("\x02554",["boxDR"])
  , ("\x02555",["boxdL"])
  , ("\x02556",["boxDl"])
  , ("\x02557",["boxDL"])
  , ("\x02558",["boxuR"])
  , ("\x02559",["boxUr"])
  , ("\x0255A",["boxUR"])
  , ("\x0255B",["boxuL"])
  , ("\x0255C",["boxUl"])
  , ("\x0255D",["boxUL"])
  , ("\x0255E",["boxvR"])
  , ("\x0255F",["boxVr"])
  , ("\x02560",["boxVR"])
  , ("\x02561",["boxvL"])
  , ("\x02562",["boxVl"])
  , ("\x02563",["boxVL"])
  , ("\x02564",["boxHd"])
  , ("\x02565",["boxhD"])
  , ("\x02566",["boxHD"])
  , ("\x02567",["boxHu"])
  , ("\x02568",["boxhU"])
  , ("\x02569",["boxHU"])
  , ("\x0256A",["boxvH"])
  , ("\x0256B",["boxVh"])
  , ("\x0256C",["boxVH"])
  , ("\x02580",["uhblk"])
  , ("\x02584",["lhblk"])
  , ("\x02588",["block"])
  , ("\x02591",["blk14"])
  , ("\x02592",["blk12"])
  , ("\x02593",["blk34"])
  , ("\x025A1",["squ","square","Square"])
  , ("\x025AA",["squf","squarf","blacksquare","FilledVerySmallSquare"])
  , ("\x025AB",["EmptyVerySmallSquare"])
  , ("\x025AD",["rect"])
  , ("\x025AE",["marker"])
  , ("\x025B1",["fltns"])
  ]


{-# NOINLINE ent2names23 #-}
ent2names23 :: [(Text,[Text])]
ent2names23 =
  [ ("\x025B3",["xutri","bigtriangleup"])
  , ("\x025B4",["utrif","blacktriangle"])
  , ("\x025B5",["utri","triangle"])
  , ("\x025B8",["rtrif","blacktriangleright"])
  , ("\x025B9",["rtri","triangleright"])
  , ("\x025BD",["xdtri","bigtriangledown"])
  , ("\x025BE",["dtrif","blacktriangledown"])
  , ("\x025BF",["dtri","triangledown"])
  , ("\x025C2",["ltrif","blacktriangleleft"])
  , ("\x025C3",["ltri","triangleleft"])
  , ("\x025CA",["loz","lozenge"])
  , ("\x025CB",["cir"])
  , ("\x025EC",["tridot"])
  , ("\x025EF",["xcirc","bigcirc"])
  , ("\x025F8",["ultri"])
  , ("\x025F9",["urtri"])
  , ("\x025FA",["lltri"])
  , ("\x025FB",["EmptySmallSquare"])
  , ("\x025FC",["FilledSmallSquare"])
  , ("\x02605",["starf","bigstar"])
  , ("\x02606",["star"])
  , ("\x0260E",["phone"])
  , ("\x02640",["female"])
  , ("\x02642",["male"])
  , ("\x02660",["spades","spadesuit"])
  , ("\x02663",["clubs","clubsuit"])
  , ("\x02665",["hearts","heartsuit"])
  , ("\x02666",["diams","diamondsuit"])
  , ("\x0266A",["sung"])
  , ("\x0266D",["flat"])
  , ("\x0266E",["natur","natural"])
  , ("\x0266F",["sharp"])
  , ("\x02713",["check","checkmark"])
  , ("\x02717",["cross"])
  , ("\x02720",["malt","maltese"])
  , ("\x02736",["sext"])
  , ("\x02758",["VerticalSeparator"])
  , ("\x02772",["lbbrk"])
  , ("\x02773",["rbbrk"])
  , ("\x027E6",["lobrk","LeftDoubleBracket"])
  ]


{-# NOINLINE ent2names24 #-}
ent2names24 :: [(Text,[Text])]
ent2names24 =
  [ ("\x027E7",["robrk","RightDoubleBracket"])
  , ("\x027E8",["lang","LeftAngleBracket","langle"])
  , ("\x027E9",["rang","RightAngleBracket","rangle"])
  , ("\x027EA",["Lang"])
  , ("\x027EB",["Rang"])
  , ("\x027EC",["loang"])
  , ("\x027ED",["roang"])
  , ("\x027F5",["xlarr","longleftarrow","LongLeftArrow"])
  , ("\x027F6",["xrarr","longrightarrow","LongRightArrow"])
  , ("\x027F7",["xharr","longleftrightarrow","LongLeftRightArrow"])
  , ("\x027F8",["xlArr","Longleftarrow","DoubleLongLeftArrow"])
  , ("\x027F9",["xrArr","Longrightarrow","DoubleLongRightArrow"])
  , ("\x027FA",["xhArr","Longleftrightarrow","DoubleLongLeftRightArrow"])
  , ("\x027FC",["xmap","longmapsto"])
  , ("\x027FF",["dzigrarr"])
  , ("\x02902",["nvlArr"])
  , ("\x02903",["nvrArr"])
  , ("\x02904",["nvHarr"])
  , ("\x02905",["Map"])
  , ("\x0290C",["lbarr"])
  , ("\x0290D",["rbarr","bkarow"])
  , ("\x0290E",["lBarr"])
  , ("\x0290F",["rBarr","dbkarow"])
  , ("\x02910",["RBarr","drbkarow"])
  , ("\x02911",["DDotrahd"])
  , ("\x02912",["UpArrowBar"])
  , ("\x02913",["DownArrowBar"])
  , ("\x02916",["Rarrtl"])
  , ("\x02919",["latail"])
  , ("\x0291A",["ratail"])
  , ("\x0291B",["lAtail"])
  , ("\x0291C",["rAtail"])
  , ("\x0291D",["larrfs"])
  , ("\x0291E",["rarrfs"])
  , ("\x0291F",["larrbfs"])
  , ("\x02920",["rarrbfs"])
  , ("\x02923",["nwarhk"])
  , ("\x02924",["nearhk"])
  , ("\x02925",["searhk","hksearow"])
  , ("\x02926",["swarhk","hkswarow"])
  ]


{-# NOINLINE ent2names25 #-}
ent2names25 :: [(Text,[Text])]
ent2names25 =
  [ ("\x02927",["nwnear"])
  , ("\x02928",["nesear","toea"])
  , ("\x02929",["seswar","tosa"])
  , ("\x0292A",["swnwar"])
  , ("\x02933",["rarrc"])
  , ("\x02935",["cudarrr"])
  , ("\x02936",["ldca"])
  , ("\x02937",["rdca"])
  , ("\x02938",["cudarrl"])
  , ("\x02939",["larrpl"])
  , ("\x0293C",["curarrm"])
  , ("\x0293D",["cularrp"])
  , ("\x02945",["rarrpl"])
  , ("\x02948",["harrcir"])
  , ("\x02949",["Uarrocir"])
  , ("\x0294A",["lurdshar"])
  , ("\x0294B",["ldrushar"])
  , ("\x0294E",["LeftRightVector"])
  , ("\x0294F",["RightUpDownVector"])
  , ("\x02950",["DownLeftRightVector"])
  , ("\x02951",["LeftUpDownVector"])
  , ("\x02952",["LeftVectorBar"])
  , ("\x02953",["RightVectorBar"])
  , ("\x02954",["RightUpVectorBar"])
  , ("\x02955",["RightDownVectorBar"])
  , ("\x02956",["DownLeftVectorBar"])
  , ("\x02957",["DownRightVectorBar"])
  , ("\x02958",["LeftUpVectorBar"])
  , ("\x02959",["LeftDownVectorBar"])
  , ("\x0295A",["LeftTeeVector"])
  , ("\x0295B",["RightTeeVector"])
  , ("\x0295C",["RightUpTeeVector"])
  , ("\x0295D",["RightDownTeeVector"])
  , ("\x0295E",["DownLeftTeeVector"])
  , ("\x0295F",["DownRightTeeVector"])
  , ("\x02960",["LeftUpTeeVector"])
  , ("\x02961",["LeftDownTeeVector"])
  , ("\x02962",["lHar"])
  , ("\x02963",["uHar"])
  , ("\x02964",["rHar"])
  ]


{-# NOINLINE ent2names26 #-}
ent2names26 :: [(Text,[Text])]
ent2names26 =
  [ ("\x02965",["dHar"])
  , ("\x02966",["luruhar"])
  , ("\x02967",["ldrdhar"])
  , ("\x02968",["ruluhar"])
  , ("\x02969",["rdldhar"])
  , ("\x0296A",["lharul"])
  , ("\x0296B",["llhard"])
  , ("\x0296C",["rharul"])
  , ("\x0296D",["lrhard"])
  , ("\x0296E",["udhar","UpEquilibrium"])
  , ("\x0296F",["duhar","ReverseUpEquilibrium"])
  , ("\x02970",["RoundImplies"])
  , ("\x02971",["erarr"])
  , ("\x02972",["simrarr"])
  , ("\x02973",["larrsim"])
  , ("\x02974",["rarrsim"])
  , ("\x02975",["rarrap"])
  , ("\x02976",["ltlarr"])
  , ("\x02978",["gtrarr"])
  , ("\x02979",["subrarr"])
  , ("\x0297B",["suplarr"])
  , ("\x0297C",["lfisht"])
  , ("\x0297D",["rfisht"])
  , ("\x0297E",["ufisht"])
  , ("\x0297F",["dfisht"])
  , ("\x02985",["lopar"])
  , ("\x02986",["ropar"])
  , ("\x0298B",["lbrke"])
  , ("\x0298C",["rbrke"])
  , ("\x0298D",["lbrkslu"])
  , ("\x0298E",["rbrksld"])
  , ("\x0298F",["lbrksld"])
  , ("\x02990",["rbrkslu"])
  , ("\x02991",["langd"])
  , ("\x02992",["rangd"])
  , ("\x02993",["lparlt"])
  , ("\x02994",["rpargt"])
  , ("\x02995",["gtlPar"])
  , ("\x02996",["ltrPar"])
  , ("\x0299A",["vzigzag"])
  ]


{-# NOINLINE ent2names27 #-}
ent2names27 :: [(Text,[Text])]
ent2names27 =
  [ ("\x0299C",["vangrt"])
  , ("\x0299D",["angrtvbd"])
  , ("\x029A4",["ange"])
  , ("\x029A5",["range"])
  , ("\x029A6",["dwangle"])
  , ("\x029A7",["uwangle"])
  , ("\x029A8",["angmsdaa"])
  , ("\x029A9",["angmsdab"])
  , ("\x029AA",["angmsdac"])
  , ("\x029AB",["angmsdad"])
  , ("\x029AC",["angmsdae"])
  , ("\x029AD",["angmsdaf"])
  , ("\x029AE",["angmsdag"])
  , ("\x029AF",["angmsdah"])
  , ("\x029B0",["bemptyv"])
  , ("\x029B1",["demptyv"])
  , ("\x029B2",["cemptyv"])
  , ("\x029B3",["raemptyv"])
  , ("\x029B4",["laemptyv"])
  , ("\x029B5",["ohbar"])
  , ("\x029B6",["omid"])
  , ("\x029B7",["opar"])
  , ("\x029B9",["operp"])
  , ("\x029BB",["olcross"])
  , ("\x029BC",["odsold"])
  , ("\x029BE",["olcir"])
  , ("\x029BF",["ofcir"])
  , ("\x029C0",["olt"])
  , ("\x029C1",["ogt"])
  , ("\x029C2",["cirscir"])
  , ("\x029C3",["cirE"])
  , ("\x029C4",["solb"])
  , ("\x029C5",["bsolb"])
  , ("\x029C9",["boxbox"])
  , ("\x029CD",["trisb"])
  , ("\x029CE",["rtriltri"])
  , ("\x029CF",["LeftTriangleBar"])
  , ("\x029D0",["RightTriangleBar"])
  , ("\x029DA",["race"])
  , ("\x029DC",["iinfin"])
  ]


{-# NOINLINE ent2names28 #-}
ent2names28 :: [(Text,[Text])]
ent2names28 =
  [ ("\x029DD",["infintie"])
  , ("\x029DE",["nvinfin"])
  , ("\x029E3",["eparsl"])
  , ("\x029E4",["smeparsl"])
  , ("\x029E5",["eqvparsl"])
  , ("\x029EB",["lozf","blacklozenge"])
  , ("\x029F4",["RuleDelayed"])
  , ("\x029F6",["dsol"])
  , ("\x02A00",["xodot","bigodot"])
  , ("\x02A01",["xoplus","bigoplus"])
  , ("\x02A02",["xotime","bigotimes"])
  , ("\x02A04",["xuplus","biguplus"])
  , ("\x02A06",["xsqcup","bigsqcup"])
  , ("\x02A0C",["qint","iiiint"])
  , ("\x02A0D",["fpartint"])
  , ("\x02A10",["cirfnint"])
  , ("\x02A11",["awint"])
  , ("\x02A12",["rppolint"])
  , ("\x02A13",["scpolint"])
  , ("\x02A14",["npolint"])
  , ("\x02A15",["pointint"])
  , ("\x02A16",["quatint"])
  , ("\x02A17",["intlarhk"])
  , ("\x02A22",["pluscir"])
  , ("\x02A23",["plusacir"])
  , ("\x02A24",["simplus"])
  , ("\x02A25",["plusdu"])
  , ("\x02A26",["plussim"])
  , ("\x02A27",["plustwo"])
  , ("\x02A29",["mcomma"])
  , ("\x02A2A",["minusdu"])
  , ("\x02A2D",["loplus"])
  , ("\x02A2E",["roplus"])
  , ("\x02A2F",["Cross"])
  , ("\x02A30",["timesd"])
  , ("\x02A31",["timesbar"])
  , ("\x02A33",["smashp"])
  , ("\x02A34",["lotimes"])
  , ("\x02A35",["rotimes"])
  , ("\x02A36",["otimesas"])
  ]


{-# NOINLINE ent2names29 #-}
ent2names29 :: [(Text,[Text])]
ent2names29 =
  [ ("\x02A37",["Otimes"])
  , ("\x02A38",["odiv"])
  , ("\x02A39",["triplus"])
  , ("\x02A3A",["triminus"])
  , ("\x02A3B",["tritime"])
  , ("\x02A3C",["iprod","intprod"])
  , ("\x02A3F",["amalg"])
  , ("\x02A40",["capdot"])
  , ("\x02A42",["ncup"])
  , ("\x02A43",["ncap"])
  , ("\x02A44",["capand"])
  , ("\x02A45",["cupor"])
  , ("\x02A46",["cupcap"])
  , ("\x02A47",["capcup"])
  , ("\x02A48",["cupbrcap"])
  , ("\x02A49",["capbrcup"])
  , ("\x02A4A",["cupcup"])
  , ("\x02A4B",["capcap"])
  , ("\x02A4C",["ccups"])
  , ("\x02A4D",["ccaps"])
  , ("\x02A50",["ccupssm"])
  , ("\x02A53",["And"])
  , ("\x02A54",["Or"])
  , ("\x02A55",["andand"])
  , ("\x02A56",["oror"])
  , ("\x02A57",["orslope"])
  , ("\x02A58",["andslope"])
  , ("\x02A5A",["andv"])
  , ("\x02A5B",["orv"])
  , ("\x02A5C",["andd"])
  , ("\x02A5D",["ord"])
  , ("\x02A5F",["wedbar"])
  , ("\x02A66",["sdote"])
  , ("\x02A6A",["simdot"])
  , ("\x02A6D",["congdot"])
  , ("\x02A6E",["easter"])
  , ("\x02A6F",["apacir"])
  , ("\x02A70",["apE"])
  , ("\x02A71",["eplus"])
  , ("\x02A72",["pluse"])
  ]


{-# NOINLINE ent2names30 #-}
ent2names30 :: [(Text,[Text])]
ent2names30 =
  [ ("\x02A73",["Esim"])
  , ("\x02A74",["Colone"])
  , ("\x02A75",["Equal"])
  , ("\x02A77",["eDDot","ddotseq"])
  , ("\x02A78",["equivDD"])
  , ("\x02A79",["ltcir"])
  , ("\x02A7A",["gtcir"])
  , ("\x02A7B",["ltquest"])
  , ("\x02A7C",["gtquest"])
  , ("\x02A7D",["les","LessSlantEqual","leqslant"])
  , ("\x02A7E",["ges","GreaterSlantEqual","geqslant"])
  , ("\x02A7F",["lesdot"])
  , ("\x02A80",["gesdot"])
  , ("\x02A81",["lesdoto"])
  , ("\x02A82",["gesdoto"])
  , ("\x02A83",["lesdotor"])
  , ("\x02A84",["gesdotol"])
  , ("\x02A85",["lap","lessapprox"])
  , ("\x02A86",["gap","gtrapprox"])
  , ("\x02A87",["lne","lneq"])
  , ("\x02A88",["gne","gneq"])
  , ("\x02A89",["lnap","lnapprox"])
  , ("\x02A8A",["gnap","gnapprox"])
  , ("\x02A8B",["lEg","lesseqqgtr"])
  , ("\x02A8C",["gEl","gtreqqless"])
  , ("\x02A8D",["lsime"])
  , ("\x02A8E",["gsime"])
  , ("\x02A8F",["lsimg"])
  , ("\x02A90",["gsiml"])
  , ("\x02A91",["lgE"])
  , ("\x02A92",["glE"])
  , ("\x02A93",["lesges"])
  , ("\x02A94",["gesles"])
  , ("\x02A95",["els","eqslantless"])
  , ("\x02A96",["egs","eqslantgtr"])
  , ("\x02A97",["elsdot"])
  , ("\x02A98",["egsdot"])
  , ("\x02A99",["el"])
  , ("\x02A9A",["eg"])
  , ("\x02A9D",["siml"])
  ]


{-# NOINLINE ent2names31 #-}
ent2names31 :: [(Text,[Text])]
ent2names31 =
  [ ("\x02A9E",["simg"])
  , ("\x02A9F",["simlE"])
  , ("\x02AA0",["simgE"])
  , ("\x02AA1",["LessLess"])
  , ("\x02AA2",["GreaterGreater"])
  , ("\x02AA4",["glj"])
  , ("\x02AA5",["gla"])
  , ("\x02AA6",["ltcc"])
  , ("\x02AA7",["gtcc"])
  , ("\x02AA8",["lescc"])
  , ("\x02AA9",["gescc"])
  , ("\x02AAA",["smt"])
  , ("\x02AAB",["lat"])
  , ("\x02AAC",["smte"])
  , ("\x02AAD",["late"])
  , ("\x02AAE",["bumpE"])
  , ("\x02AAF",["pre","preceq","PrecedesEqual"])
  , ("\x02AB0",["sce","succeq","SucceedsEqual"])
  , ("\x02AB3",["prE"])
  , ("\x02AB4",["scE"])
  , ("\x02AB5",["prnE","precneqq"])
  , ("\x02AB6",["scnE","succneqq"])
  , ("\x02AB7",["prap","precapprox"])
  , ("\x02AB8",["scap","succapprox"])
  , ("\x02AB9",["prnap","precnapprox"])
  , ("\x02ABA",["scnap","succnapprox"])
  , ("\x02ABB",["Pr"])
  , ("\x02ABC",["Sc"])
  , ("\x02ABD",["subdot"])
  , ("\x02ABE",["supdot"])
  , ("\x02ABF",["subplus"])
  , ("\x02AC0",["supplus"])
  , ("\x02AC1",["submult"])
  , ("\x02AC2",["supmult"])
  , ("\x02AC3",["subedot"])
  , ("\x02AC4",["supedot"])
  , ("\x02AC5",["subE","subseteqq"])
  , ("\x02AC6",["supE","supseteqq"])
  , ("\x02AC7",["subsim"])
  , ("\x02AC8",["supsim"])
  ]


{-# NOINLINE ent2names32 #-}
ent2names32 :: [(Text,[Text])]
ent2names32 =
  [ ("\x02ACB",["subnE","subsetneqq"])
  , ("\x02ACC",["supnE","supsetneqq"])
  , ("\x02ACF",["csub"])
  , ("\x02AD0",["csup"])
  , ("\x02AD1",["csube"])
  , ("\x02AD2",["csupe"])
  , ("\x02AD3",["subsup"])
  , ("\x02AD4",["supsub"])
  , ("\x02AD5",["subsub"])
  , ("\x02AD6",["supsup"])
  , ("\x02AD7",["suphsub"])
  , ("\x02AD8",["supdsub"])
  , ("\x02AD9",["forkv"])
  , ("\x02ADA",["topfork"])
  , ("\x02ADB",["mlcp"])
  , ("\x02AE4",["Dashv","DoubleLeftTee"])
  , ("\x02AE6",["Vdashl"])
  , ("\x02AE7",["Barv"])
  , ("\x02AE8",["vBar"])
  , ("\x02AE9",["vBarv"])
  , ("\x02AEB",["Vbar"])
  , ("\x02AEC",["Not"])
  , ("\x02AED",["bNot"])
  , ("\x02AEE",["rnmid"])
  , ("\x02AEF",["cirmid"])
  , ("\x02AF0",["midcir"])
  , ("\x02AF1",["topcir"])
  , ("\x02AF2",["nhpar"])
  , ("\x02AF3",["parsim"])
  , ("\x02AFD",["parsl"])
  , ("\x0FB00",["fflig"])
  , ("\x0FB01",["filig"])
  , ("\x0FB02",["fllig"])
  , ("\x0FB03",["ffilig"])
  , ("\x0FB04",["ffllig"])
  , ("\x1D49C",["Ascr"])
  , ("\x1D49E",["Cscr"])
  , ("\x1D49F",["Dscr"])
  , ("\x1D4A2",["Gscr"])
  , ("\x1D4A5",["Jscr"])
  ]


{-# NOINLINE ent2names33 #-}
ent2names33 :: [(Text,[Text])]
ent2names33 =
  [ ("\x1D4A6",["Kscr"])
  , ("\x1D4A9",["Nscr"])
  , ("\x1D4AA",["Oscr"])
  , ("\x1D4AB",["Pscr"])
  , ("\x1D4AC",["Qscr"])
  , ("\x1D4AE",["Sscr"])
  , ("\x1D4AF",["Tscr"])
  , ("\x1D4B0",["Uscr"])
  , ("\x1D4B1",["Vscr"])
  , ("\x1D4B2",["Wscr"])
  , ("\x1D4B3",["Xscr"])
  , ("\x1D4B4",["Yscr"])
  , ("\x1D4B5",["Zscr"])
  , ("\x1D4B6",["ascr"])
  , ("\x1D4B7",["bscr"])
  , ("\x1D4B8",["cscr"])
  , ("\x1D4B9",["dscr"])
  , ("\x1D4BB",["fscr"])
  , ("\x1D4BD",["hscr"])
  , ("\x1D4BE",["iscr"])
  , ("\x1D4BF",["jscr"])
  , ("\x1D4C0",["kscr"])
  , ("\x1D4C1",["lscr"])
  , ("\x1D4C2",["mscr"])
  , ("\x1D4C3",["nscr"])
  , ("\x1D4C5",["pscr"])
  , ("\x1D4C6",["qscr"])
  , ("\x1D4C7",["rscr"])
  , ("\x1D4C8",["sscr"])
  , ("\x1D4C9",["tscr"])
  , ("\x1D4CA",["uscr"])
  , ("\x1D4CB",["vscr"])
  , ("\x1D4CC",["wscr"])
  , ("\x1D4CD",["xscr"])
  , ("\x1D4CE",["yscr"])
  , ("\x1D4CF",["zscr"])
  , ("\x1D504",["Afr"])
  , ("\x1D505",["Bfr"])
  , ("\x1D507",["Dfr"])
  , ("\x1D508",["Efr"])
  ]


{-# NOINLINE ent2names34 #-}
ent2names34 :: [(Text,[Text])]
ent2names34 =
  [ ("\x1D509",["Ffr"])
  , ("\x1D50A",["Gfr"])
  , ("\x1D50D",["Jfr"])
  , ("\x1D50E",["Kfr"])
  , ("\x1D50F",["Lfr"])
  , ("\x1D510",["Mfr"])
  , ("\x1D511",["Nfr"])
  , ("\x1D512",["Ofr"])
  , ("\x1D513",["Pfr"])
  , ("\x1D514",["Qfr"])
  , ("\x1D516",["Sfr"])
  , ("\x1D517",["Tfr"])
  , ("\x1D518",["Ufr"])
  , ("\x1D519",["Vfr"])
  , ("\x1D51A",["Wfr"])
  , ("\x1D51B",["Xfr"])
  , ("\x1D51C",["Yfr"])
  , ("\x1D51E",["afr"])
  , ("\x1D51F",["bfr"])
  , ("\x1D520",["cfr"])
  , ("\x1D521",["dfr"])
  , ("\x1D522",["efr"])
  , ("\x1D523",["ffr"])
  , ("\x1D524",["gfr"])
  , ("\x1D525",["hfr"])
  , ("\x1D526",["ifr"])
  , ("\x1D527",["jfr"])
  , ("\x1D528",["kfr"])
  , ("\x1D529",["lfr"])
  , ("\x1D52A",["mfr"])
  , ("\x1D52B",["nfr"])
  , ("\x1D52C",["ofr"])
  , ("\x1D52D",["pfr"])
  , ("\x1D52E",["qfr"])
  , ("\x1D52F",["rfr"])
  , ("\x1D530",["sfr"])
  , ("\x1D531",["tfr"])
  , ("\x1D532",["ufr"])
  , ("\x1D533",["vfr"])
  , ("\x1D534",["wfr"])
  ]


{-# NOINLINE ent2names35 #-}
ent2names35 :: [(Text,[Text])]
ent2names35 =
  [ ("\x1D535",["xfr"])
  , ("\x1D536",["yfr"])
  , ("\x1D537",["zfr"])
  , ("\x1D538",["Aopf"])
  , ("\x1D539",["Bopf"])
  , ("\x1D53B",["Dopf"])
  , ("\x1D53C",["Eopf"])
  , ("\x1D53D",["Fopf"])
  , ("\x1D53E",["Gopf"])
  , ("\x1D540",["Iopf"])
  , ("\x1D541",["Jopf"])
  , ("\x1D542",["Kopf"])
  , ("\x1D543",["Lopf"])
  , ("\x1D544",["Mopf"])
  , ("\x1D546",["Oopf"])
  , ("\x1D54A",["Sopf"])
  , ("\x1D54B",["Topf"])
  , ("\x1D54C",["Uopf"])
  , ("\x1D54D",["Vopf"])
  , ("\x1D54E",["Wopf"])
  , ("\x1D54F",["Xopf"])
  , ("\x1D550",["Yopf"])
  , ("\x1D552",["aopf"])
  , ("\x1D553",["bopf"])
  , ("\x1D554",["copf"])
  , ("\x1D555",["dopf"])
  , ("\x1D556",["eopf"])
  , ("\x1D557",["fopf"])
  , ("\x1D558",["gopf"])
  , ("\x1D559",["hopf"])
  , ("\x1D55A",["iopf"])
  , ("\x1D55B",["jopf"])
  , ("\x1D55C",["kopf"])
  , ("\x1D55D",["lopf"])
  , ("\x1D55E",["mopf"])
  , ("\x1D55F",["nopf"])
  , ("\x1D560",["oopf"])
  , ("\x1D561",["popf"])
  , ("\x1D562",["qopf"])
  , ("\x1D563",["ropf"])
  ]


{-# NOINLINE ent2names36 #-}
ent2names36 :: [(Text,[Text])]
ent2names36 =
  [ ("\x1D564",["sopf"])
  , ("\x1D565",["topf"])
  , ("\x1D566",["uopf"])
  , ("\x1D567",["vopf"])
  , ("\x1D568",["wopf"])
  , ("\x1D569",["xopf"])
  , ("\x1D56A",["yopf"])
  , ("\x1D56B",["zopf"])
  ]


{-# NOINLINE ent2names37 #-}
ent2names37 :: [(Text,[Text])]
ent2names37 =
  [ ("\8770\824",["NotEqualTilde"])
  , ("\8807\824",["NotGreaterFullEqual"])
  , ("\8811\824",["NotGreaterGreater"])
  , ("\10878\824",["NotGreaterSlantEqual"])
  , ("\8782\824",["NotHumpDownHump"])
  , ("\8783\824",["NotHumpEqual"])
  , ("\10703\824",["NotLeftTriangleBar"])
  , ("\8810\824",["NotLessLess"])
  , ("\10877\824",["NotLessSlantEqual"])
  , ("\10914\824",["NotNestedGreaterGreater"])
  , ("\10913\824",["NotNestedLessLess"])
  , ("\10927\824",["NotPrecedesEqual"])
  , ("\10704\824",["NotRightTriangleBar"])
  , ("\8847\824",["NotSquareSubset"])
  , ("\8848\824",["NotSquareSuperset"])
  , ("\8834\8402",["NotSubset"])
  , ("\10928\824",["NotSucceedsEqual"])
  , ("\8831\824",["NotSucceedsTilde"])
  , ("\8835\8402",["NotSuperset"])
  , ("\8287\8202",["ThickSpace"])
  , ("\8766\819",["acE"])
  , ("=\8421",["bne"])
  , ("\8801\8421",["bnequiv"])
  , ("\10184",["bsolhsub"])
  , ("\8745\65024",["caps"])
  , ("\8746\65024",["cups"])
  , ("fj",["fjlig"])
  , ("\8923\65024",["gesl"])
  , ("\8809\65024",["gvertneqq"])
  , ("\8809\65024",["gvnE"])
  , ("\10925\65024",["lates"])
  , ("\8922\65024",["lesg"])
  , ("\8808\65024",["lvertneqq"])
  , ("\8808\65024",["lvnE"])
  , ("\8921\824",["nGg"])
  , ("\8811\8402",["nGt"])
  , ("\8811\824",["nGtv"])
  , ("\8920\824",["nLl"])
  , ("\8810\8402",["nLt"])
  , ("\8810\824",["nLtv"])
  , ("\8736\8402",["nang"])
  , ("\10864\824",["napE"])
  , ("\8779\824",["napid"])
  , ("\8782\824",["nbump"])
  , ("\8783\824",["nbumpe"])
  , ("\10861\824",["ncongdot"])
  , ("\8784\824",["nedot"])
  , ("\8770\824",["nesim"])
  , ("\8807\824",["ngE"])
  , ("\8807\824",["ngeqq"])
  , ("\10878\824",["ngeqslant"])
  , ("\10878\824",["nges"])
  , ("\8806\824",["nlE"])
  , ("\8806\824",["nleqq"])
  , ("\10877\824",["nleqslant"])
  , ("\10877\824",["nles"])
  , ("\8953\824",["notinE"])
  , ("\8949\824",["notindot"])
  , ("\11005\8421",["nparsl"])
  , ("\8706\824",["npart"])
  , ("\10927\824",["npre"])
  , ("\10927\824",["npreceq"])
  , ("\10547\824",["nrarrc"])
  , ("\8605\824",["nrarrw"])
  , ("\10928\824",["nsce"])
  , ("\10949\824",["nsubE"])
  , ("\8834\8402",["nsubset"])
  , ("\10949\824",["nsubseteqq"])
  , ("\10928\824",["nsucceq"])
  , ("\10950\824",["nsupE"])
  , ("\8835\8402",["nsupset"])
  , ("\10950\824",["nsupseteqq"])
  , ("\8781\8402",["nvap"])
  , ("\8805\8402",["nvge"])
  , (">\8402",["nvgt"])
  , ("\8804\8402",["nvle"])
  , ("<\8402",["nvlt"])
  , ("\8884\8402",["nvltrie"])
  , ("\8885\8402",["nvrtrie"])
  , ("\8764\8402",["nvsim"])
  , ("\10924\65024",["smtes"])
  , ("\8851\65024",["sqcaps"])
  , ("\8852\65024",["sqcups"])
  , ("\10185",["suphsol"])
  , ("\8842\65024",["varsubsetneq"])
  , ("\10955\65024",["varsubsetneqq"])
  , ("\8843\65024",["varsupsetneq"])
  , ("\10956\65024",["varsupsetneqq"])
  , ("\8834\8402",["vnsub"])
  , ("\8835\8402",["vnsup"])
  , ("\10955\65024",["vsubnE"])
  , ("\8842\65024",["vsubne"])
  , ("\10956\65024",["vsupnE"])
  , ("\8843\65024",["vsupne"])
  ]

{-
Discrepancies


old ("UnderBar","_")
new ("UnderBar","\818")

old ("angst","\197")
new ("angst","\8491")

old ("epsi","\949")
new ("epsi","\1013")

old ("epsiv","\1013")
new ("epsiv","\949")

old ("ohm","\937")
new ("ohm","\8486")

old ("phiv","\981")
new ("phiv","\966")

old ("race","\8765\817")
new ("race","\10714")

old ("varepsilon","\1013")
new ("varepsilon","\949")

old ("varphi","\981")
new ("varphi","\966")

old ("OverBar","\8254")
new ("OverBar","\175")
-}
