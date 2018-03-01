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
predefinedRefs = Map.fromList $ concatMap f entNames
  where
    f (e,ns) = map (,e) ns


------------------------------------------------------------------------------
-- | Reverse lookup of Html entities. The values in this map should be the
-- "canonical" entity names that are most widely support by browsers, email
-- clients, etc. If you encounter a situation where the value in this map is
-- not the most widely supported, please open a pull request to change the
-- order in the appropriate ent2names table below.
reversePredefinedRefs :: Map Text Text
reversePredefinedRefs = fmap head $ Map.fromList entNames


------------------------------------------------------------------------------
-- | This (Text, [Text]) formulation allows us to have a predictable canonical
-- entity reference. This version was derived from
-- https://dev.w3.org/html5/html-author/charref which has an ordering for the
-- names (along with a few additions that we had previously). We're assuming
-- that the first name in the list is the canonical name. This is important
-- because some names are less widely supported and we want xmlhtml to
-- predictably generate the name that has the widest support rather than
-- having the name decided implicitly by the ordering imposed by Map as had
-- previously been the case.
--
-- This list is split into a bunch of smaller lists because build times for
-- this module get really long (as in minutes) if we don't.
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
