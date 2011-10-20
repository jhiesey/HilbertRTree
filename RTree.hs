
data Rect = Rect { xmin :: Int, xmax :: Int, ymin :: Int, ymax:: Int } deriving (Show)

hilbertValue :: Int -> Int -> Int -> Int
hilbertValue x y n = 1

findRectHilbert :: Rect -> Int -> Int
findRectHilbert r n = hilbertValue ((xmin r + xmax r) `quot` 2) ((ymin r + ymax r) `quot` 2) n

intersects :: Rect -> Rect -> Bool
intersects a b = xmin a < xmax b && xmax a > xmin b &&
                 ymin a < ymax b && ymax a > ymin b
                 
-- data Leaf = Leaf {leafH :: Int, leafRect :: Rect} deriving (Show)

data RTree = Node Int Rect [RTree]
           | Leaf Int Rect
           deriving (Show)

treeLHV :: RTree -> Int
treeLHV (Node h _ _ _) = h
treeLHV (Leaf h _ _) = h

treeBound :: RTree -> Rect
treeBound (Node _ r _) = r
treeBound (Leaf _ r) = r
  
boundIntersect :: Rect -> Rect -> Rect
           
-- computeLHV :: [RTree] -> Int
-- 
-- computeBound :: [RTree] -> Rect

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
generateLeaves :: [RTree] -> Int -> [RTree]
generateLeaves contents n =
  let
    sortedContents = sortBy (\a b -> compare (largestH a) (largestH b)) contents
    -- Compute the number of new nodes to stick the elements back into
    numElems = length sortedContents
    numNewSiblings = if numElems > n * capacity then n + 1 else n

    elemsPerSibling = numElems `quot` numNewSiblings + if numElems `rem` numNewSiblings == 0 then 0 else 1

    newSiblingElems = group sortedContents elemsPerSibling
    
    generateLeaf :: [RTreee] -> RTree
    generateLeaf ls =
      let
        -- Assumes sorted elements
        newLhv = treeLHV $ last ls
        newBound = foldr (boundIntersect . treeBound) ls
      in
        Node newLhv newBound ls
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
    newContents = l:((foldr (\(Node _ _ elems) list -> list ++ elems) [] siblingList))
  
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
      Node (computeLHV mid) (computeBound mid) mid
  where
    -- Accepts a zipper of nodes, and returns 3 lists: unmodified before and after, and modified
    insertT :: ([RTree], RTree, [RTree]) -> Leaf -> ([RTree], [RTree], [RTree])
    insertT (left, focus@(Node lhv bound elems), right) l =
      -- Find the correct insertion point
      let
        (start, rest) = span (\(ll _ _) -> ll <= (treeLHV l)) elems
        -- Compute the new contents of this node: the unchanged previous nodes (in reverse), the new/modified nodes, and the rest
        (resStart, resModified, resRest) = case elems of
          [Node] -> insertT (reverse start, head rest, tail rest) l
          [Leaf] -> (reverse start, l, rest)
          
        totalElems = length resStart + length resModified + resRest
      in  
        if totalElems <= capacity then
          -- Just reconstitute the node, since everything fits
          let
            newLhv = foldr (max . treeLHV) lhv resModified
            newBound = foldr (boundIntersect . treeBound) bound resModified
            
            modifiedNode = Node newLhv newBound ((reverse resStart) ++ resModified ++ resRest)
          in
            (left, [modifiedNode], right)
        else
          -- Otherwise, rearrange the elements in the siblings
          let
            numRightSiblings = min (length right) siblings
            -- Separate the siblings from the rest of the parent's nodes list
            (leftSiblings, leftRest) = splitAt (siblings - numRightSiblings) left
            (rightSiblings, rightRest) = splitAt numRightSiblings right
            
            -- Actual number of siblings found (may be less than siblings if the parent is not full)
            numSiblings = (length siblingList) + 1 -- One extra for the focus     
          in
            
          

        -- splitNode :: ([RTree], RTree, [RTree]) -> Int -> ([RTree], [RTree], [RTree])
        -- splitNode (left, focus, right) =
        --   let
            numRightSiblings = min (length right) siblings
            -- Separate the siblings from the rest of the parent's nodes list
            (leftSiblings, leftRest) = splitAt (siblings - numRightSiblings) left
            (rightSiblings, rightRest) = splitAt numRightSiblings right
        
        --     siblingList = leftSiblings ++ (focus:rightSiblings)
        --     -- Actual number of siblings found (may be less than siblings if the parent is not full)
        --     numSiblings = (length siblingList) + 1
        --     -- Get all of the contents of the siblings, plus the new node
        --     newContents = l:((foldr (\(Node _ _ elems) list -> list ++ elems) [] siblingList))
        -- 
        --   in
        --     (leftRest, generateLeaves newContents numSiblings, rightRest)         
        --   
      -- in
      --   case elems of
      --     [Node] ->
      --       let
      --         -- Recurse
      --         (resStart, resMid, resRest) = insertT (reverse start, head rest, tail rest) l
      --       in
      --         if length resStart + length resMid + length resRest <= capacity then
      --           let
      --             allChildren = reverse resStart ++ resMid ++ resRest
      --             newInnerNode = Node (foldr max oldh resMid) (foldr boundIntersect oldr resMid) allChildren
      --           in
      --             (left, [newInnerNode], right)
      --         else
      --           -- Need to split
      --           splitNode (left, focus, right)
      --       
      --       -- Find correct child, recurse, update h, r
      --       -- if length children < capacity then
      --       --   let
      --       --     (start, rest) = span (\(ll _ _) -> ll <= (leafH l)) children
      --       --     newInnerNode = InnerNode (max lhv ((\(ll _ _) -> ll) l)) (boundIntersect bound ((\(_ bb _) -> bb) l)) (before ++ (l))
      --   
      --     [Leaf] ->
      --       let
      --         (resStart, resMid, resRest) = (reverse start, l, rest)
      --     
      --     
      --       -- Find location, insert, upddate h, r
      --       if length elements < capacity then
      --         -- Don't need to split
      --         let
      --           (before, rest) = span (\ll -> (leafH ll) <= (leafH l)) elements
      --           newLeafNode = Node (max lhv (leafH l)) (boundIntersect bound (leafRect l)) (before ++ (l:rest))
      --         in
      --           (left, [newLeafNode], right)
      --       else
      --         -- Need to split
      --         splitNode (left, focus, right)

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