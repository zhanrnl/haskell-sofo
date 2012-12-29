-- Challenge problem, PS2.

module CoprimePerm
( coprimePerm
) where

-- Despite Haskell's laziness, it would be inefficient to simply generate a
-- list of all permutations then find the first one where all elements are
-- coprime, because if two elements at the start of a list aren't coprime we
-- know we can skip all permutations based off of that start, but the coprime
-- processing functions don't know that the list has this structure. Thus, it
-- makes sense to dive deeper into this problem and do some form of recursive
-- backtracking or something that will terminate generating permutations early.


-- We'll use laziness to generate all coprime permutations in a list of lists,
-- then we'll just return the first one that was found (thus only finding
-- one).
-- |Returns a single coprime permutation of the given list of Ints.
coprimePerm :: [Int] -> [Int]
coprimePerm xs = case coprimePerms [] xs [] of
    []     -> []
    (p:ps) -> p

{-
-- In an imperative language, this would be a step function to repeatedly
-- apply in a loop until all the perms were generated. However, because
-- Haskell is not imperative, this isn't very useful to us. The structure
-- of the approach, however, is useful: Imagine, instead of performing a step
-- in-place, we were accumulating successful permutations in a list.
partialCoprimePerms :: [Int] -> [Int] -> [([Int], [Int])]
partialCoprimePerms done rest =
    [ (r:done, est) | (r, est) <- eltRestPairs rest,
                      null done || r `coprime` (head done) ]
-}

-- Generates a list of all coprime permutations. Note that the recursive nature
-- of the list generated combined with Haskell's laziness mean that taking the
-- head of this list will only result in enough computation to calculate the
-- first such solution.
coprimePerms :: [Int] -> [Int] -> [[Int]] -> [[Int]]
coprimePerms done []   found = done : found
coprimePerms done rest found = 
    concat [ coprimePerms (r:done) est found
               | (r,est) <- eltRestPairs rest,
                 null done || r `coprime` (head done) ]

{-
-- A manually-recursive way of getting pairs of elements and the rest of the
-- list, but it's uglier than the list-comprehension way.
eltRestPairs :: [Int] -> [(Int,[Int])]
eltRestPairs xs = eltRestPairs' [] [] xs
    where eltRestPairs' pairs _ [] = pairs
          eltRestPairs' pairs front (x:xs) = 
              eltRestPairs' (x:front ++ xs) (front ++ x) xs
-}

-- Generates a list of pairs of each element and the rest of the list,
-- excluding that element.
eltRestPairs :: [a] -> [(a, [a])]
eltRestPairs xs = [ (elt, front ++ rest) | n <- [0..length xs - 1],
                      let (front, (elt:rest)) = splitAt n xs ]

-- Returns True if two numbers are coprime.
coprime :: (Integral a) => a -> a -> Bool
coprime a b = (gcd a b) == 1

