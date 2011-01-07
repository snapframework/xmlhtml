------------------------------------------------------------------------------
-- | A zipper for navigating and modifying XML trees.  This is nearly the
-- same exposed interface as the @xml@ package in @Text.XML.Light.Cursor@,
-- with modifications as needed to adapt to different types.
module Text.XmlHtml.Cursor (
    -- * Cursor type
    Cursor,

    -- * Conversion to and from cursors
    fromNode,
    fromNodes,
    toNode,
    toNodes,
    current,
    siblings,

    -- * Cursor navigation
    parent,
    root,
    getChild,
    firstChild,
    lastChild,
    left,
    right,
    nextDF,

    -- * Search
    findChild,
    findLeft,
    findRight,
    findRec,

    -- * Node classification
    isRoot,
    isFirst,
    isLast,
    isLeaf,
    isChild,
    hasChildren,
    getNodeIndex,

    -- * Updates
    setNode,
    modifyNode,
    modifyNodeM,

    -- * Insertions
    insertLeft,
    insertRight,
    insertGoLeft,
    insertGoRight,
    
    -- * Deletions
    removeLeft,
    removeRight,
    removeGoLeft,
    removeGoRight,
    removeGoUp
    ) where

import           Control.Monad
import           Data.Maybe
import           Data.Text (Text)
import           Text.XmlHtml

------------------------------------------------------------------------------
-- | Data type representing just the tag of an element
data Tag = Tag Text [(Text, Text)]


------------------------------------------------------------------------------
-- | Extracts the tag of a node, if it is an element.  Otherwise, gives
-- 'Nothing'
getTag :: Node -> Maybe Tag
getTag (Element t a _) = Just (Tag t a)
getTag _               = Nothing


------------------------------------------------------------------------------
-- | Reconstructs an element from a tag and a list of its children.
fromTag :: Tag -> [Node] -> Node
fromTag (Tag t a) c = Element t a c


------------------------------------------------------------------------------
-- | A zipper for XML document forests.
data Cursor = Cursor {
    current :: !Node,   -- ^ Retrieves the current node of a 'Cursor'
    lefts   :: ![Node],                 -- right to left
    rights  :: ![Node],                 -- left to right
    parents :: ![([Node], Tag, [Node])] -- parent's tag and siblings
    }


------------------------------------------------------------------------------
-- | Builds a 'Cursor' for navigating a tree. That is, a forest with a single
-- root 'Node'.
fromNode :: Node -> Cursor
fromNode n = Cursor n [] [] []


------------------------------------------------------------------------------
-- | Builds a 'Cursor' for navigating a forest with the given list of roots.
-- The cursor is initially positioned at the left-most node.  Gives 'Nothing'
-- if the list is empty.
fromNodes :: [Node] -> Maybe Cursor
fromNodes (n:ns) = Just (Cursor n [] ns [])
fromNodes []     = Nothing


------------------------------------------------------------------------------
-- | Retrieves the root node containing the current cursor position.
toNode :: Cursor -> Node
toNode cur  = current (root cur)


------------------------------------------------------------------------------
-- | Retrieves the entire forest of 'Node's corresponding to a 'Cursor'.
toNodes :: Cursor -> [Node]
toNodes cur = siblings (root cur)


------------------------------------------------------------------------------
-- | Retrieves a list of the 'Node's at the same level as the current position
-- of a cursor, including the current node.
siblings :: Cursor -> [Node]
siblings (Cursor cur ls rs _) = foldl (flip (:)) (cur:rs) ls


------------------------------------------------------------------------------
-- | Navigates a 'Cursor' to its parent in the document.
parent :: Cursor -> Maybe Cursor
parent c@(Cursor _ _ _ ((ls,t,rs):ps))
            = Just (Cursor (fromTag t (siblings c)) ls rs ps)
parent _    = Nothing


------------------------------------------------------------------------------
-- | Navigates a 'Cursor' up through parents to reach the root level.
root :: Cursor -> Cursor
root = until isRoot (fromJust . parent)


------------------------------------------------------------------------------
-- | Navigates a 'Cursor' down to the indicated child index.
getChild :: Int -> Cursor -> Maybe Cursor
getChild i (Cursor n ls rs ps) =
    let cs          = childNodes n
        Just tag    = getTag n
        (lls, rest) = splitAt i cs
    in  if i < length cs
            then Just $ Cursor (head rest)
                               (reverse lls)
                               (tail rest)
                               ((ls, tag, rs):ps)
            else Nothing


------------------------------------------------------------------------------
-- | Navigates a 'Cursor' down to its first child.
firstChild :: Cursor -> Maybe Cursor
firstChild = getChild 0


------------------------------------------------------------------------------
-- | Navigates a 'Cursor' down to its last child.
lastChild :: Cursor -> Maybe Cursor
lastChild (Cursor (Element t a c) ls rs ps) | not (null c)
    = let rc = reverse c
      in  Just $ Cursor (head rc) (tail rc) [] ((ls, Tag t a, rs):ps)
lastChild _
    = Nothing


------------------------------------------------------------------------------
-- | Moves a 'Cursor' to its left sibling.
left :: Cursor -> Maybe Cursor
left (Cursor c (l:ls) rs ps) = Just (Cursor l ls (c:rs) ps)
left _                       = Nothing


------------------------------------------------------------------------------
-- | Moves a 'Cursor' to its right sibling.
right :: Cursor -> Maybe Cursor
right (Cursor c ls (r:rs) ps) = Just (Cursor r (c:ls) rs ps)
right _                       = Nothing


------------------------------------------------------------------------------
-- | Moves a 'Cursor' to the next node encountered in a depth-first search.
-- If it has children, this is equivalent to 'firstChild'.  Otherwise, if it
-- has a right sibling, then this is equivalent to 'right'.  Otherwise, the
-- cursor moves to the first right sibling of one of its parents.
nextDF :: Cursor -> Maybe Cursor
nextDF c = firstChild c `mplus` up c
  where up x = right x `mplus` (up =<< parent x)


------------------------------------------------------------------------------
-- Repeats the given move until a 'Cursor' is obtained that matches the
-- predicate.
search :: (Cursor -> Bool)         -- ^ predicate
       -> (Cursor -> Maybe Cursor) -- ^ move
       -> Cursor                   -- ^ starting point
       -> Maybe Cursor
search p move c | p c       = return c
                | otherwise = search p move =<< move c


------------------------------------------------------------------------------
-- Navigates a 'Cursor' to the first child that matches the predicate.
findChild :: (Cursor -> Bool) -> Cursor -> Maybe Cursor
findChild p cur = search p right =<< firstChild cur


------------------------------------------------------------------------------
-- Navigates a 'Cursor' to the nearest left sibling that matches a predicate.
findLeft :: (Cursor -> Bool) -> Cursor -> Maybe Cursor
findLeft p cur = search p left =<< left cur


------------------------------------------------------------------------------
-- Navigates a 'Cursor' to the nearest right sibling that matches a predicate.
findRight :: (Cursor -> Bool) -> Cursor -> Maybe Cursor
findRight p cur = search p right =<< right cur


------------------------------------------------------------------------------
-- | Does a depth-first search for a descendant matching the predicate.  This
-- can match the current cursor position.
findRec :: (Cursor -> Bool) -> Cursor -> Maybe Cursor
findRec p = search p nextDF


------------------------------------------------------------------------------
-- | Determines if the 'Cursor' is at a root node.
isRoot :: Cursor -> Bool
isRoot cur = null (parents cur)


------------------------------------------------------------------------------
-- | Determines if the 'Cursor' is at a first child.
isFirst :: Cursor -> Bool
isFirst cur = null (lefts cur)


------------------------------------------------------------------------------
-- | Determines if the 'Cursor' is at a last child.
isLast :: Cursor -> Bool
isLast cur = null (rights cur)


------------------------------------------------------------------------------
-- | Determines if the 'Cursor' is at a leaf node.
isLeaf :: Cursor -> Bool
isLeaf (Cursor (Element _ _ c) _ _ _) = null c
isLeaf _                              = True


------------------------------------------------------------------------------
-- | Determines if the 'Cursor' is at a child node (i.e., if it has a parent).
isChild :: Cursor -> Bool
isChild = not . isRoot


------------------------------------------------------------------------------
-- | Determines if the 'Cursor' is at a non-leaf node (i.e., if it has
-- children).
hasChildren :: Cursor -> Bool
hasChildren = not . isLeaf


------------------------------------------------------------------------------
-- | Gets the index of the 'Cursor' among its siblings.
getNodeIndex :: Cursor -> Int
getNodeIndex cur = length (lefts cur)


------------------------------------------------------------------------------
-- | Replaces the current node.
setNode :: Node -> Cursor -> Cursor
setNode n cur = cur { current = n }


------------------------------------------------------------------------------
-- | Modifies the current node by applying a function.
modifyNode :: (Node -> Node) -> Cursor -> Cursor
modifyNode f cur = cur { current = f (current cur) }


------------------------------------------------------------------------------
-- | Modifies the current node by applying an action in some functor.
modifyNodeM :: Functor m => (Node -> m Node) -> Cursor -> m Cursor
modifyNodeM f cur = flip setNode cur `fmap` f (current cur)


------------------------------------------------------------------------------
-- | Inserts a new 'Node' to the left of the current position.
insertLeft :: Node -> Cursor -> Cursor
insertLeft n (Cursor nn ls rs ps) = Cursor nn (n:ls) rs ps


------------------------------------------------------------------------------
-- | Inserts a new 'Node' to the right of the current position.
insertRight :: Node -> Cursor -> Cursor
insertRight n (Cursor nn ls rs ps) = Cursor nn ls (n:rs) ps


------------------------------------------------------------------------------
-- | Inserts a new 'Node' to the left of the current position, and moves
-- left to the new node.
insertGoLeft :: Node -> Cursor -> Cursor
insertGoLeft n (Cursor nn ls rs ps) = Cursor n ls (nn:rs) ps


------------------------------------------------------------------------------
-- | Inserts a new 'Node' to the right of the current position, and moves
-- right to the new node.
insertGoRight :: Node -> Cursor -> Cursor
insertGoRight n (Cursor nn ls rs ps) = Cursor n (nn:ls) rs ps


------------------------------------------------------------------------------
-- | Removes the 'Node' to the left of the current position, if any.
removeLeft :: Cursor -> Maybe (Node, Cursor)
removeLeft (Cursor n (l:ls) rs ps) = Just (l, Cursor n ls rs ps)
removeLeft _                       = Nothing


------------------------------------------------------------------------------
-- | Removes the 'Node' to the right of the current position, if any.
removeRight :: Cursor -> Maybe (Node, Cursor)
removeRight (Cursor n ls (r:rs) ps) = Just (r, Cursor n ls rs ps)
removeRight _                       = Nothing


------------------------------------------------------------------------------
-- | Removes the current 'Node', and moves the Cursor to its left sibling,
-- if any.
removeGoLeft :: Cursor -> Maybe Cursor
removeGoLeft (Cursor _ (l:ls) rs ps) = Just (Cursor l ls rs ps)
removeGoLeft _                       = Nothing


------------------------------------------------------------------------------
-- | Removes the current 'Node', and moves the Cursor to its right sibling,
-- if any.
removeGoRight :: Cursor -> Maybe Cursor
removeGoRight (Cursor _ ls (r:rs) ps) = Just (Cursor r ls rs ps)
removeGoRight _                       = Nothing


------------------------------------------------------------------------------
-- | Removes the current 'Node', and moves the Cursor to its parent, if any.
removeGoUp :: Cursor -> Maybe Cursor
removeGoUp (Cursor _ ls rs ((lls, Tag t a, rrs):ps))
    = Just (Cursor (Element t a children) lls rrs ps)
  where
    children = foldl (flip (:)) (rs) ls
removeGoUp _                       = Nothing

