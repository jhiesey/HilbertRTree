
data Rect = Rect { xmin :: Int, xmax :: Int, ymin :: Int, ymax:: Int } deriving (Show)

hilbertValue :: Int -> Int -> Int -> Int
hilbertValue x y n = 1

findRectHilbert :: Rect -> Int -> Int
findRectHilbert r n = hilbertValue ((xmin r + xmax r) `quot` 2) ((ymin r + ymax r) `quot` 2) n

intersects :: Rect -> Rect -> Bool
intersects a b = xmin a < xmax b && xmax a > xmin b &&
                 ymin a < ymax b && ymax a > ymin b
                 
-- This class is used for               
class Hilbert a where
  largestH :: a -> Int
  boundingRect :: a -> Rect
                 
data Leaf = Leaf {leafH :: Int, leafRect :: Rect} deriving (Show)

instance Hilbert Leaf where
  largestH = leafH
  boundingRect = leafRect

data RTree = InnerNode Int Rect [RTree]
           | LeafNode Int Rect [Leaf]
           deriving (Show)
           
instance Hilbert RTree where
  largestH (InnerNode h _ _) = h
  largestH (LeafNode h _ _) = h
  boundingRect (InnerNode _ r _) = r
  boundingRect (LeafNode _ r _) = r
  
  
boundIntersect :: Rect -> Rect -> Rect
           
computeLHV :: [RTree] -> Int

computeBound :: [RTree] -> Rect

capacity :: Int
capacity = 10

siblings :: Int
siblings = 2

dimension :: Int
dimension = 65536

-- Splits a list up into groups of a certain size
group :: [a] -> Int -> [[a]]
group as = group' as [] n
  where
    group' [] acc n = acc
    group' as acc n = group' restAs (gr:acc)
    where
      (gr, restAs) = splitAt n as
      
-- Generates n (or n+1, in case of overflow) leaf nodes from a list of elements
generateLeaves :: Hilbert a, Hilbert b => [a] -> Int -> [b]
generateLeaves contents n =
  let
    sortedContents = sortBy (\a b -> compare (largestH a) (largestH b)) contents
    -- Compute the number of new nodes to stick the elements back into
    numElems = length sortedContents
    numNewSiblings = if numElems > n * capacity then n + 1 else n

    elemsPerSibling = numElems `quot` numNewSiblings + if numElems `rem` numNewSiblings == 0 then 0 else 1

    newSiblingElems = group sortedContents elemsPerSibling
    
    generateLeaf :: Hilbert a, Hilbert b => [a] -> b
    generateLeaf ls =
      let
        -- Assumes sorted elements
        newLhv = leafH $ last ls
        newBound = foldr (boundIntersect . leafRect) ls
      in
        LeafNode newLhv newBound ls
  in
    map generateLeaf newSiblingElems
    
splitNode :: ([RTree], RTree, [RTree]) -> Int -> ([RTree], [RTree], [RTree])
splitNode (left, focus, right) =
  let
    numRightSiblings = min (length right) siblings
    -- Separate the siblings from the rest of the parent's nodes list
    (leftSiblings, leftRest) = splitAt (siblings - numRightSiblings) left
    (rightSiblings, rightRest) = splitAt numRightSiblings right
  
    siblingList = leftSiblings ++ (focus:rightSiblings)
    -- Actual number of siblings found (may be less than siblings if the parent is not full)
    numSiblings = (length siblingList) + 1
    -- Get all of the contents of the siblings, plus the new node
    newContents = l:((foldr (\(LeafNode _ _ elems) list -> list ++ elems) [] siblingList))
  
  in
    (leftRest, generateLeaves newContents numSiblings, rightRest)
           
insertTree :: RTree -> Rect -> RTree
insertTree t r =
  let
    h = findRectHilbertRect r dimension
    newLeaf = Leaf h r
    (left, mid, right) = insertT [] t [] newLeaf
  in
    -- Check if the root split
    if null $ tail mid then
      head mid
    else
      -- If the root split, make a new root
      InnerNode (computeLHV mid) (computeBound mid) mid
  where
    -- Accepts a zipper of nodes, and returns 3 lists: unmodified before and after, and modified
    insertT :: ([RTree], RTree, [RTree]) -> Leaf -> ([RTree], [RTree], [RTree])
    insertT (left, focus, right) l =
      case focus of
        InnerNode lhv bound children@[RTree oldh oldr _] ->
          let
            (start, rest) = span (\(ll _ _) -> ll <= (leafH l)) children
            (resStart, resMid, resRest) = insertT (reverse start, head rest, tail rest) l
          in
            if length resStart + length resMid + length resRest <= capacity then
              let
                allChildren = reverse resStart ++ resMid ++ resRest
                newInnerNode = InnerNode (foldr max oldh resMid) (foldr boundIntersect oldr resMid) allChildren
              in
                (left, [newInnerNode], right)
            else
              -- Need to split
              splitNode (left, focus, right)
            
          -- Find correct child, recurse, update h, r
          -- if length children < capacity then
          --   let
          --     (start, rest) = span (\(ll _ _) -> ll <= (leafH l)) children
          --     newInnerNode = InnerNode (max lhv ((\(ll _ _) -> ll) l)) (boundIntersect bound ((\(_ bb _) -> bb) l)) (before ++ (l))
        
        LeafNode lhv bound elements@[Leaf _ _ _] ->
          -- Find location, insert, upddate h, r
          if length elements < capacity then
            -- Don't need to split
            let
              (before, rest) = span (\ll -> (leafH ll) <= (leafH l)) elements
              newLeafNode = LeafNode (max lhv (leafH l)) (boundIntersect bound (leafRect l)) (before ++ (l:rest))
            in
              (left, [newLeafNode], right)
          else
            -- Need to split
            splitNode (left, focus, right)

-- data RTree el = Node [(Rect, RTree el, Int)]
--               | Leaf [(Rect, el, Int)]
--               deriving (Show)
-- 
-- searchTree :: RTree el -> Rect -> [el]
-- searchTree (Node nodeContents) query =
--   concat [searchTree child query | (r, child, _) <- nodeContents, intersects query r]
-- searchTree (Leaf leafContents) query =
--   [e | (r, e, _) <- leafContents, intersects query r]
--   
-- emptyTree :: () -> RTree el
-- emptyTree () = Leaf []
-- 
-- insertT :: RTree Rect -> Rect -> RTree Rect
-- insertT n q = insertTree n q (findRectHilbert q 65536)
-- 
-- insertTree :: RTree Rect -> Rect -> Int -> RTree Rect
-- insertTree (Node nodeContents) query h =
--   let
-- --    correctChild = minimumBy (\(_,_,lhv) m -> min lhv m) 0 $ filter (\(_,_,lhv) -> lhv > h) nodeContents
--     -- Find smallest element larger than h (assuming they are sorted in increasing h order)
--     (start, rest) = span (\(_,_,lhv) -> lhv <= h) nodeContents
--     (_, correctChild, _) = head rest
--     newChild = insertTree correctChild query h
--   in
--     Node (start ++ (newChild:rest))
--     
--   -- up logic
-- insertTree (Leaf leafContents) query h =
--   if length leafContents < 10 then
--     let
--       (start, rest) = span (\(_,_,lhv) -> lhv <= h) leafContents
--       newLeaf = (query, query, h)
--     in
--       Leaf (start ++ (newLeaf:rest))
--   else
--       Leaf []
-- 
-- 
-- -- Data type, which is also a rect
-- newtype Elem = Elem Rect
-- 
-- main = do
--   putStr $ show "hi"