module Main where

import RTree hiding (main)

import Test.QuickCheck
import Data.List
import Data.Function

-- A functional test for the Hilbert value funciton
-- tryHilbert :: Int -> String
-- tryHilbert o =
--   let  
--     hilbertRow :: Int -> Int -> String
--     hilbertRow order row = foldl (\curr x -> curr ++ "\t" ++ (show (x, row)) ++ (show (hilbertValue x row order))) [] [0..order - 1]
--   in
--     foldr (\y curr -> curr ++ "\n" ++ (hilbertRow o y)) [] [0..o - 1]


instance Arbitrary Rect where
  arbitrary = do
    xmin <- choose (0, 65535)
    xmax <- choose (xmin, 65535)
    ymin <- choose (0, 65535)
    ymax <- choose (ymin, 65535)
    return (Rect xmin xmax ymin ymax)

data Simple = Simple [Rect] deriving (Show)
           
insertSimple :: Simple -> Rect -> Simple
insertSimple (Simple t) r = Simple (r:t)

searchSimple :: Simple -> Rect -> [Rect]
searchSimple (Simple t) q = filter (intersects q) t

-- A high-level test (comparison against a very simple version)
t_tree :: [Rect] -> Rect -> Bool
t_tree elems query =
  sort (searchTree (foldl insertTree Empty elems) query) == sort (searchSimple (foldl insertSimple (Simple []) elems) query)

-- Ensures that the Hilbert values are always nondecreasing
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
    orderedT (Node _ _ children) = (isSortedBy (compare `on` treeLHV) children) && all orderedT children
  in
    orderedT tree

-- Ensures that bounding rect calculations are correct
-- t_bounds :: [Rect] -> Bool

main = do
  quickCheck t_tree
  quickCheck t_ordered    