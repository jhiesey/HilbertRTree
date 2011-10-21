module RTree where

import System.IO.Unsafe

import System.IO
import CPUTime
import System
import Data.List

data Rect = Rect { xmin :: Int, xmax :: Int, ymin :: Int, ymax:: Int } deriving (Show, Eq, Ord)

hilbertValue :: Int -> Int -> Int -> Int
hilbertValue x y n = hilbertValue' x y n 0
  where
    hilbertValue' x y 0 acc = acc
    hilbertValue' x y n acc =
      let
        right = x * 2 >= n
        top = y * 2 >= n
    
        halfN = n `quot` 2
        (mul, newX, newY) = 
          if right then
            if top then
              (2, x - halfN, y - halfN)
            else
              (3, halfN - 1 - y, n - 1 - x)
          else
            if top then
              (1, x, y - halfN)
            else
              (0, y, x)
      in
        hilbertValue' newX newY halfN (acc + halfN * halfN * mul)

findRectHilbert :: Rect -> Int -> Int
findRectHilbert r n = hilbertValue ((xmin r + xmax r) `quot` 2) ((ymin r + ymax r) `quot` 2) n

intersects :: Rect -> Rect -> Bool
intersects a b = xmin a < xmax b && xmax a > xmin b &&
                 ymin a < ymax b && ymax a > ymin b

data RTree = Node Int Rect [RTree]
           | Leaf Int Rect
           | Empty
           deriving (Show)

treeLHV :: RTree -> Int
treeLHV (Node h _ _) = h
treeLHV (Leaf h _) = h

treeBound :: RTree -> Rect
treeBound (Node _ r _) = r
treeBound (Leaf _ r) = r

treeChildren :: RTree -> [RTree]
treeChildren (Node _ _ c) = c
-- This should never be called on leaves!
  
boundIntersect :: Rect -> Rect -> Rect
boundIntersect a b = Rect (min (xmin a) (xmin b)) (max (xmax a) (xmax b)) (min (ymin a) (ymin b)) (max (ymax a) (ymax b))

capacity :: Int
capacity = 10

siblings :: Int
siblings = 2

dimension :: Int
dimension = 65536

-- Splits a list up into groups of a certain size
groupNodes :: [a] -> Int -> [[a]]
groupNodes as n = groupNodes' as [] n
  where
    groupNodes' [] acc n = acc
    groupNodes' as acc n = groupNodes' restAs (gr:acc) n
      where
        (gr, restAs) = splitAt n as
           
insertTree :: RTree -> Rect -> RTree
insertTree t r =
  let
    h = findRectHilbert r dimension
    newLeaf = Leaf h r
    (left, mid, right) = insertT ([], t, []) newLeaf
  in
    -- Check if the root split
    if null $ tail mid then
      head mid
    else
      -- If the root split, make a new root
      Node (foldr1 max (map treeLHV mid)) (foldr1 boundIntersect (map treeBound mid)) mid
  where
    -- Accepts a zipper of nodes, and returns 3 lists: unmodified before and after, and modified
    insertT :: ([RTree], RTree, [RTree]) -> RTree -> ([RTree], [RTree], [RTree])
    -- Special case for the empty tree
    insertT (left, empty@(Empty), right) l =
      (left, [Node (treeLHV l) (treeBound l) [l]], right)
    insertT (left, focus@(Node lhv bound elems), right) l =
      -- Find the correct insertion point
      let
        (start, rest) = span (\ll -> (treeLHV ll) <= (treeLHV l)) elems
        reverseStart = reverse start
        zipper = if null rest then (tail reverseStart, head reverseStart, rest) else (reverse start, head rest, tail rest)
        -- Compute the new contents of this node: the unchanged previous nodes (in reverse), the new/modified nodes, and the rest
        (resStart, resModified, resRest) = case elems of
          (Node _ _ _):_ -> insertT zipper l
          (Leaf _ _):_ -> (reverseStart, [l], rest)
          
        totalElems = length resStart + length resModified + length resRest
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
            numSiblings = (length leftSiblings) + (length rightSiblings) + 1 -- One extra for the focus
            
            -- Get all of the elements that should go in the focus and its cooperating siblings
            allElems = (foldr (++) [] (map treeChildren leftSiblings)) ++ (foldr (++) [] (map treeChildren rightSiblings)) ++ resStart ++ resModified ++ resRest
            numElems = length allElems
            
            -- Compute the number of new nodes to stick the elements back into
            numNewSiblings = if numElems > numSiblings * capacity then numSiblings + 1 else numSiblings
            
            sortedElems = sortBy (\a b -> compare (treeLHV a) (treeLHV b)) allElems
            elemsPerSibling = numElems `quot` numNewSiblings + if numElems `rem` numNewSiblings == 0 then 0 else 1
            
            newSiblingLists = groupNodes sortedElems elemsPerSibling

            generateNode :: [RTree] -> RTree
            generateNode ls =
              let
                -- Assumes sorted elements
                newLhv = treeLHV $ last ls
                newBound = foldr1 boundIntersect (map treeBound ls)
              in
                Node newLhv newBound ls
              
            newNodes = map generateNode newSiblingLists
          in
            (leftRest, newNodes, rightRest)
            
searchTree :: RTree -> Rect -> [Rect]
searchTree (Empty) query = []
searchTree (Node _ _ contents) query =
  concat [searchTree child query | child <- contents, intersects query (treeBound child)]
searchTree (Leaf _ r) query = [r]

-- Splits alternate (even, odd) elements into separate lists, one for x values, one for y values
splitCoords :: [Int] -> ([Int], [Int])
splitCoords cs = splitCoords' cs 0 ([],[])
  where    
    splitCoords' [] _ (xs, ys) = (xs, ys)
    splitCoords' (c:cs) n (xs, ys) =
      let
        thisNode =
          if mod n 2 == 0 then
            ((c:xs), ys)
          else
            (xs, (c:ys))
      in
        splitCoords' cs (n + 1) thisNode

-- Split a string on a character        
splitString :: Char -> String -> [String]
splitString delim xs =
  let
    (start, end) = span (/= delim) xs
  in
    case end of
      (c:cs) -> (start:(splitString delim cs))
      [] -> [start]
  
-- Parses a rect
toRect :: String -> Rect
toRect s =
  let
    coords :: [Int]
    coords = map read (splitString ',' s)
    
    -- Find a bounding box
    (xs, ys) = splitCoords coords
  in
    Rect (foldr1 min xs) (foldr1 max xs) (foldr1 min ys) (foldr1 max ys)
    
data Simple = Simple [Rect] deriving (Show)

insertSimple :: Simple -> Rect -> Simple
insertSimple (Simple t) r = Simple (r:t)

searchSimple :: Simple -> Rect -> [Rect]
searchSimple (Simple t) q = filter (intersects q) t

-- Generates a tree given the contents of a file
generateTree :: String -> RTree
generateTree contents = 
  let
    ls = lines contents
    filteredLs = filter (/= "") ls
  in
    foldr (\s t -> insertTree t (toRect s)) Empty filteredLs

-- Formats a list of rectangles as a string, taking at most the first four rects
formatResult :: [Rect] -> String
formatResult rs = unlines $ take 4 $ map formatRect rs
  where
    formatRect r = "    " ++ foldr1 (\el res -> el ++ "," ++ res)[(show $ xmin r), (show $ ymax r),
                 (show $ xmin r), (show $ ymin r), 
                 (show $ xmax r), (show $ ymin r),
                 (show $ xmax r), (show $ ymax r)]
 
-- Query loop   
readLoop :: RTree -> IO ()
readLoop tree = do
  isEof <- isEOF
  if isEof then return ()
  else do
    query <- getLine
    if query == "" then return ()
    else do
      startTime <- getCPUTime
      let qResult = searchTree tree (toRect query)
      endTime <- getCPUTime
      putStr $ ("Found " ++ show (length qResult) ++ " matches in " ++ show ((endTime - startTime) `quot` 1000) ++ " microseconds:\n")
      -- putStr $ show qResult
      putStr $ formatResult qResult
      putStr "\n"
      readLoop tree
      

main = do
  startTime <- getCPUTime
  files <- getArgs
  contents <- readFile (head files)
  tree <- return $ generateTree contents
  endTime <- getCPUTime
  putStr $ ("Read in " ++ show ((endTime - startTime) `quot` 1000000) ++ " milliseconds\n")

  readLoop tree
  -- putStr $ show $ map (splitString ',') (splitString '\n' contents)
  -- putStr $ show $ generateTree contents
  
  
  
  
  
  
  
  