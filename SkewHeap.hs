-- SkewHeap.hs
-- A simple implementation of the Skew Heap priority queue.
-- Credit for assignment idea: The Monad.Reader issue 16.

module SkewHeap
( SkewHeap()
, null
, empty
, singleton
, insert
, extractMin
, union
, (+*+)
, fromList
, toList
) where

import Prelude hiding (null)

-- |The SkewHeap data type represents a priority queue.
data SkewHeap a = Empty
                | SkewNode a (SkewHeap a) (SkewHeap a)
                deriving (Show)

-- |Adds two SkewHeaps together.
(+*+) :: (Ord a) => SkewHeap a -> SkewHeap a -> SkewHeap a
Empty +*+ heap = heap
heap +*+ Empty = heap
heap1@(SkewNode x1 left1 right1) +*+ heap2@(SkewNode x2 left2 right2)
    | x1 <= x2  = SkewNode x1 (right1 +*+ heap2) left1
    | otherwise = SkewNode x2 (right2 +*+ heap1) left2

-- |Checks whether the given SkewHeap is empty.
null :: SkewHeap a -> Bool
null Empty = True
null _     = False

-- |Constructs an empty SkewHeap.
empty :: SkewHeap a
empty = Empty

-- |Constructs a singleton SkewHeap.
singleton :: a -> SkewHeap a
singleton x = SkewNode x Empty Empty

-- |Inserts an element into a SkewHeap.
insert :: (Ord a) => a -> SkewHeap a -> SkewHeap a
insert x heap = singleton x +*+ heap

-- |Extracts the minimum element from a SkewHeap, returning Maybe a pair
-- containing the minimum element and the new SkewHeap.
extractMin :: (Ord a) => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty = Nothing
extractMin (SkewNode x left right) = Just (x, left +*+ right)

-- |Unions two SkewHeaps.
union :: (Ord a) => SkewHeap a -> SkewHeap a -> SkewHeap a
union = (+*+)

-- |Constructs a SkewHeap from a list of values.
fromList :: (Ord a) => [a] -> SkewHeap a
fromList = foldr insert Empty

-- |Converts a SkewHeap to a list.
toList :: SkewHeap a -> [a]
toList Empty = []
toList (SkewNode x left right) = x : toList left ++ toList right


-- | LENNART'S ADDITIONS HERE
-- I like the idea of a toAscList function since then you get the pleasure of
-- writing heapsort :D  Do you approve?

toAscList :: Ord a => SkewHeap a -> [a]
toAscList sh =
  let m = extractMin sh
  in case m of
    Just (e, sh') -> e : (toAscList sh')
    Nothing -> []

heapsort :: Ord a => [a] -> [a]
heapsort = toAscList . fromList
