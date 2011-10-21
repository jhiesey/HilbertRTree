module Main where

import RTree hiding (main)

import Test.QuickCheck
import Data.List
import Data.Function

-- A manual test (visual inspection) for the Hilbert value funciton
tryHilbert :: Int -> String
tryHilbert o =
  let  
    hilbertRow :: Int -> Int -> String
    hilbertRow order row = foldl (\curr x -> curr ++ "\t" ++ (show (x, row)) ++ (show (hilbertValue x row order))) [] [0..order - 1]
  in
    foldr (\y curr -> curr ++ "\n" ++ (hilbertRow o y)) [] [0..o - 1] ++ "\n"

instance Arbitrary Rect where
  arbitrary = do
    xmin <- choose (0, 65535)
    xmax <- choose (xmin, 65535)
    ymin <- choose (0, 65535)
    ymax <- choose (ymin, 65535)
    return (Rect xmin xmax ymin ymax)

-- A simple version, for comparing to our much fancier one
data Simple = Simple [Rect] deriving (Show)
           
insertSimple :: Simple -> Rect -> Simple
insertSimple (Simple t) r = Simple (r:t)

searchSimple :: Simple -> Rect -> [Rect]
searchSimple (Simple t) q = filter (intersects q) t

-- A high-level test (comparison against a very simple version)
t_tree :: [Rect] -> Rect -> Bool
t_tree elems query =
  sort (searchTree (foldl insertTree Empty elems) query) == sort (searchSimple (foldl insertSimple (Simple []) elems) query)

-- Ensures that the Hilbert values are always nondecreasing, and that the maximum values are correct
t_ordered :: [Rect] -> Bool  
t_ordered elems =
  let
    tree = (foldl insertTree Empty elems)
    
    isSortedBy :: (a -> a -> Ordering) -> [a] -> Bool
    isSortedBy cp (fir:rest@(sec:_)) = cp fir sec /= GT && isSortedBy cp rest
    isSortedBy cp (fir:[]) = True
    isSortedBy cp [] = True
    
    orderedT :: RTree -> Bool
    orderedT Empty = True
    orderedT (Leaf _ _) = True
    orderedT (Node h _ children) = (isSortedBy (compare `on` treeLHV) children) && all orderedT children && maximum (map treeLHV children) == h
  in
    orderedT tree

-- Ensures that bounding rect calculations are correct
t_bounds :: [Rect] -> Bool
t_bounds elems =
  let
    tree = (foldl insertTree Empty elems)
    
    boundedT :: RTree -> Bool
    boundedT Empty = True
    boundedT (Leaf _ _) = True
    boundedT (Node _ b children) = foldr1 boundIntersect (map treeBound children) == b && all boundedT children
  in
    boundedT tree
    
-- Checks that every rect exists somewhere
t_allrects :: [Rect] -> Bool
t_allrects elems =
  let
    tree = (foldl insertTree Empty elems)
    
    allRects :: RTree -> [Rect]
    allRects Empty = []
    allRects (Leaf _ b) = [b]
    allRects (Node _ _ children) = concat $ map allRects children
  in
    sort (allRects tree) == sort elems

main = do
  quickCheck t_tree
  quickCheck t_ordered
  quickCheck t_bounds
  quickCheck t_allrects
  putStr $ tryHilbert 8