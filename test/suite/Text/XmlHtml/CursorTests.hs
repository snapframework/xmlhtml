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
    testIt   "cursorNEQ              " $ cursorNEQ,
    testCase "cursorNavigation       " $ cursorNavigation,
    testCase "cursorSearch           " $ cursorSearch,
    testCase "cursorMutation         " $ cursorMutation,
    testCase "cursorInsertion        " $ cursorInsertion,
    testCase "cursorDeletion         " $ cursorDeletion
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

cursorNEQ :: Bool
cursorNEQ = let a = fromNode (Element "a" [] [])
                b = fromNode (Element "b" [] [])
            in  a /= b

-- Sample node structure for running cursor tests.
cursorTestTree :: Node
cursorTestTree = Element "department" [("code", "A17")] [
    Element "employee" [("name", "alice")] [
        Element "address" [] [
            TextNode "124 My Road"
            ],
        Element "phone" [] [
            TextNode "555-1234"
            ]
        ],
    Element "employee" [("name", "bob")] [
        Comment "My best friend",
        Element "address" [] [
            TextNode "123 My Road"
            ],
        Element "temp" [] []
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
        getAttribute "name" (current e1) == Just "alice"
    assertBool "childAt 1 "      $
        getAttribute "name" (current e2) == Just "bob"
    assertBool "lastChild "      $
        getAttribute "name" (current e3) == Just "camille"

    do let Just a = lastChild e2
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

    do let Just cmt = firstChild e2
       assertBool  "topNode"  $ tagName (topNode cmt) == Just "department"
       assertBool  "topNodes" $ map tagName (topNodes cmt) == [ Just "department" ]

       assertBool "first child of comment" $ isNothing (firstChild cmt)
       assertBool "last  child of comment" $ isNothing (lastChild cmt)

    do assertBool "nextDF down" $ nextDF r == Just e1

       let Just cmt = firstChild e2
       assertBool "nextDF right" $ nextDF cmt == right cmt

       let Just em = lastChild e2
       assertBool "nextDF up-right" $ nextDF em == Just e3
       
       let Just pelem = lastChild e3
       let Just ptext = lastChild pelem
       assertBool "nextDF end" $ isNothing (nextDF ptext)


cursorSearch :: Assertion
cursorSearch = do
    let r = fromNode cursorTestTree

    let Just e1 = findChild isFirst r
    let Just e2 = findChild ((==(Just "bob")) . getAttribute "name" .  current) r
    let Just e3 = findChild isLast r

    assertBool "findChild isFirst" $
        getAttribute "name" (current e1) == Just "alice"
    assertBool "findChild" $
        getAttribute "name" (current e2) == Just "bob"
    assertBool "findChild isLast" $
        getAttribute "name" (current e3) == Just "camille"

    assertBool "findLeft Just" $ findLeft (const True) e2 == Just e1
    assertBool "findLeft Nothing" $ findLeft (const False) e2 == Nothing
    assertBool "findRight Just" $ findRight (const True) e2 == Just e3
    assertBool "findRight Nothing" $ findRight (const False) e2 == Nothing
    assertBool "findRec" $ findRec (not . hasChildren) r == (firstChild =<< firstChild e1)

    assertBool "isChild true" $ isChild e1
    assertBool "isChild false" $ not $ isChild r
    assertBool "getNodeIndex" $ getNodeIndex e2 == 1


cursorMutation :: Assertion
cursorMutation = do
    let r = fromNode cursorTestTree

    let Just e1 = firstChild r
    let Just e2 = right e1
    let Just e3 = lastChild r

    do let Just cmt = firstChild e2
       let cmt'     = setNode (Comment "Not my friend any more") cmt
       let Just e2' = parent cmt'
       assertBool "setNode" $ current e2' ==
            Element "employee" [("name", "bob")] [
                Comment "Not my friend any more",
                Element "address" [] [
                    TextNode "123 My Road"
                    ],
                Element "temp" [] []
                ]

    do let e1' = modifyNode (setAttribute "name" "bryan") e1
       let n   = current e1'
       assertBool "modifyNode" $ getAttribute "name" n == Just "bryan"

    do let myModifyM = return . setAttribute "name" "chelsea"
       e3' <- modifyNodeM myModifyM e3
       let n   = current e3'
       assertBool "modifyNode" $ getAttribute "name" n == Just "chelsea"


cursorInsertion :: Assertion
cursorInsertion = do
    let r            = fromNode cursorTestTree
    let Just alice   = firstChild r
    let Just bob     = getChild 1 r
    let Just camille = lastChild r

    let fred = Element "employee" [("name", "fred")] []

    -- Stock insertLeft
    do let ins    = insertLeft fred bob
       assertBool "insertLeft leaves cursor" $
            getAttribute "name" (current ins) == Just "bob"

       let Just a = findLeft isFirst ins
       assertBool "insertLeft 1" $
            getAttribute "name" (current a) == Just "alice"

       let Just b = right a
       assertBool "insertLeft 2" $
            getAttribute "name" (current b) == Just "fred"

       let Just c = right b
       assertBool "insertLeft 3" $
            getAttribute "name" (current c) == Just "bob"

       let Just d = right c
       assertBool "insertLeft 4" $
            getAttribute "name" (current d) == Just "camille"

    -- insertLeft on first child
    do let ins    = insertLeft fred alice
       assertBool "insertLeft firstChild" $
            getAttribute "name" (current ins) == Just "alice"

       let Just a = findLeft isFirst ins
       assertBool "insertLeft firstChild 1" $
            getAttribute "name" (current a) == Just "fred"

    -- Stock insertRight
    do let ins    = insertRight fred alice
       assertBool "insertRight leaves cursor" $
            getAttribute "name" (current ins) == Just "alice"

       let Just a = findRight isLast ins
       assertBool "insertRight 1" $
            getAttribute "name" (current a) == Just "camille"

       let Just b = left a
       assertBool "insertRight 2" $
            getAttribute "name" (current b) == Just "bob"

       let Just c = left b
       assertBool "insertRight 3" $
            getAttribute "name" (current c) == Just "fred"

       let Just d = left c
       assertBool "insertRight 4" $
            getAttribute "name" (current d) == Just "alice"

    -- insertRight on last child
    do let ins    = insertRight fred camille
       assertBool "insertRight lastChild" $
            getAttribute "name" (current ins) == Just "camille"

       let Just a = findRight isLast ins
       assertBool "insertRight lastChild 1" $
            getAttribute "name" (current a) == Just "fred"

    let mary = Element "employee" [("name", "mary")] []
    let new  = [fred, mary]

    -- insertManyLeft
    do let ins = insertManyLeft new camille
       assertBool "insertManyLeft leaves cursor" $
            getAttribute "name" (current ins) == Just "camille"

       let Just a = left ins
       assertBool "insertManyLeft 1" $
            getAttribute "name" (current a) == Just "mary"

       let Just b = left a
       assertBool "insertManyLeft 2" $
            getAttribute "name" (current b) == Just "fred"

       let Just c = left b
       assertBool "insertManyLeft 3" $
            getAttribute "name" (current c) == Just "bob"

    -- insertManyRight
    do let ins = insertManyRight new alice
       assertBool "insertManyRight leaves cursor" $
            getAttribute "name" (current ins) == Just "alice"

       let Just a = right ins
       assertBool "insertManyRight 1" $
            getAttribute "name" (current a) == Just "fred"

       let Just b = right a
       assertBool "insertManyRight 2" $
            getAttribute "name" (current b) == Just "mary"

       let Just c = right b
       assertBool "insertManyRight 3" $
            getAttribute "name" (current c) == Just "bob"

    -- insertFirstChild and insertLastChild
    do let Just ins1 = insertFirstChild fred r
       let Just ins2 = insertLastChild mary ins1

       let Just a = firstChild ins2
       assertBool "insert children 1" $
            getAttribute "name" (current a) == Just "fred"

       let Just b = right a
       assertBool "insert children 2" $
            getAttribute "name" (current b) == Just "alice"

       let Just c = right b
       assertBool "insert children 3" $
            getAttribute "name" (current c) == Just "bob"

       let Just d = right c
       assertBool "insert children 4" $
            getAttribute "name" (current d) == Just "camille"

       let Just e = right d
       assertBool "insert children 5" $
            getAttribute "name" (current e) == Just "mary"
       assertBool "insert children 6" $ isLast e

    -- non-element insertFirstChild and insertLastChild
    do let Just cmt = firstChild bob
       assertBool "non-elem insertFirstChild" $
            insertFirstChild fred cmt == Nothing
       assertBool "non-elem insertLastChild" $
            insertLastChild fred cmt == Nothing
       assertBool "non-elem insertManyFirstChild" $
            insertManyFirstChild new cmt == Nothing
       assertBool "non-elem insertManyLastChild" $
            insertManyLastChild new cmt == Nothing

    -- insertManyFirstChild
    do let Just ins = insertManyFirstChild new r

       let Just a = firstChild ins
       assertBool "insertManyFirstChild 1" $
            getAttribute "name" (current a) == Just "fred"

       let Just b = right a
       assertBool "insertManyFirstChild 2" $
            getAttribute "name" (current b) == Just "mary"

       let Just c = right b
       assertBool "insertManyFirstChild 3" $
            getAttribute "name" (current c) == Just "alice"

       let Just d = right c
       assertBool "insertManyFirstChild 4" $
            getAttribute "name" (current d) == Just "bob"

       let Just e = right d
       assertBool "insertManyFirstChild 5" $
            getAttribute "name" (current e) == Just "camille"
       assertBool "insertManyFirstChild 6" $ isLast e


    -- insertManyLastChild
    do let Just ins = insertManyLastChild new r

       let Just a = firstChild ins
       assertBool "insertManyFirstChild 1" $
            getAttribute "name" (current a) == Just "alice"

       let Just b = right a
       assertBool "insertManyFirstChild 2" $
            getAttribute "name" (current b) == Just "bob"

       let Just c = right b
       assertBool "insertManyFirstChild 3" $
            getAttribute "name" (current c) == Just "camille"

       let Just d = right c
       assertBool "insertManyFirstChild 4" $
            getAttribute "name" (current d) == Just "fred"

       let Just e = right d
       assertBool "insertManyFirstChild 5" $
            getAttribute "name" (current e) == Just "mary"
       assertBool "insertManyFirstChild 6" $ isLast e

    -- insertGoLeft from middle
    do let ins    = insertGoLeft fred bob
       let Just a = right ins
       assertBool "insertGoLeft 1" $
            getAttribute "name" (current ins) == Just "fred"
       assertBool "insertGoLeft 2" $
            getAttribute "name" (current a)   == Just "bob"

    -- insertGoLeft from end
    do let ins    = insertGoLeft fred alice
       let Just a = right ins
       assertBool "insertGoLeft 3" $
            getAttribute "name" (current ins) == Just "fred"
       assertBool "insertGoLeft 4" $
            getAttribute "name" (current a)   == Just "alice"

    -- insertGoRight from middle
    do let ins    = insertGoRight fred bob
       let Just a = left ins
       assertBool "insertGoRight 1" $
            getAttribute "name" (current ins) == Just "fred"
       assertBool "insertGoRight 2" $
            getAttribute "name" (current a)   == Just "bob"

    -- insertGoRight from end
    do let ins    = insertGoRight fred camille
       let Just a = left ins
       assertBool "insertGoRight 3" $
            getAttribute "name" (current ins) == Just "fred"
       assertBool "insertGoRight 4" $
            getAttribute "name" (current a)   == Just "camille"


cursorDeletion :: Assertion
cursorDeletion = do
    let r = fromNode cursorTestTree
    let Just alice   = firstChild r
    let Just bob     = getChild 1 r
    let Just camille = lastChild r

    -- removeLeft success
    do let Just (n,del) = removeLeft bob
       let [b,c] = siblings del
       assertBool "removeLeft node1" $ getAttribute "name" n == Just "alice"
       assertBool "removeLeft node2" $ getAttribute "name" b == Just "bob"
       assertBool "removeLeft node3" $ getAttribute "name" c == Just "camille"

    -- removeLeft failure
    do assertBool "removeLeft failure" $ isNothing (removeLeft alice)

    -- removeRight success
    do let Just (n,del) = removeRight bob
       let [a,b] = siblings del
       assertBool "removeLeft node1" $ getAttribute "name" a == Just "alice"
       assertBool "removeLeft node2" $ getAttribute "name" b == Just "bob"
       assertBool "removeLeft node3" $ getAttribute "name" n == Just "camille"

    -- removeRight failure
    do assertBool "removeLeft failure" $ isNothing (removeRight camille)

    -- removeGoLeft success
    do let Just del = removeGoLeft bob
       let Just c   = right del
       assertBool "removeGoLeft 1" $
            getAttribute "name" (current del) == Just "alice"
       assertBool "removeGoLeft 2" $
            getAttribute "name" (current c) == Just "camille"

    -- removeGoLeft failure
    do assertBool "removeGoLeft failure" $ isNothing (removeGoLeft alice)

    -- removeGoRight success
    do let Just del = removeGoRight bob
       let Just a   = left del
       assertBool "removeGoRight 1" $
            getAttribute "name" (current del) == Just "camille"
       assertBool "removeGoRight 2" $
            getAttribute "name" (current a) == Just "alice"

    -- removeGoLeft failure
    do assertBool "removeGoRight failure" $ isNothing (removeGoRight camille)

    -- removeGoUp success
    do let Just del = removeGoUp bob
       let [a,c]    = childNodes (current del)
       assertBool "removeGoUp 1" $ getAttribute "name" a == Just "alice"
       assertBool "removeGoUp 2" $ getAttribute "name" c == Just "camille"

    -- removeGoUp failure
    do assertBool "removeGoUp failure" $ isNothing (removeGoUp r)
