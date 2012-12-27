-- PS1.hs
-- Solutions to PS 1.

-- Imperative solution. Unless you want to get fancy with definitions, this
-- is pretty much good enough.
nthTri :: Int -> Int
nthTri n = n * (n + 1) `div` 2

-- Another approach, basically the definition of the nth triangle number.
nthTri' n = sum [0..n]

{-
-- Alternate approach: defining an infinite list of triangle numbers.
tris :: [Int]
tris = 0:zipWith (+) tris [1..]

nthTri'' :: Int -> Int
nthTri'' n = tris !! n

-- An even clearer definition.
tris' :: [Int]
tris' = scanl1 (+) [0..]

nthTri''' :: Int -> Int
nthTri''' n = tris' !! n
-}

-- Laziness makes this more efficient than it would otherwise be.
-- Note that I have written [Char] in place of String to emphasize that this
-- is completely generalizable to arbitrary lists.
isPalindrome :: [Char] -> Bool
isPalindrome xs = xs == reverse xs

-- A bit odd...
sameParity :: [Int] -> Bool
sameParity xs = (even (head xs)) == (even (last xs))

{-
-- With a lambda function.
specialSum :: Int -> Int
specialSum n = sum . filter (\x -> x /= 3 && x /= 7) $ [1..pred n]

-- An alternate (slightly cleaner) formulation.
specialSum' :: Int -> Int
specialSum' n = sum . filter (/= 7) . filter (/= 3) $ [1..pred n]
-}

-- Using a list comprehension.
specialSum :: Int -> Int
specialSum n = sum [ x | x <- [1..pred n], x /= 3, x /= 7 ]

-- Again, note that, due to laziness, the whole list is not evaluated just
-- to check for membership; only as many elements as are needed are evaluated,
-- which makes the code more efficient than it looks.
--
isSquareBetween :: Int -> Int -> Int -> Bool
isSquareBetween a b c = a `elem` [ x^2 | x <- [b..c] ]

-- Note that by importing Data.List(intercalate), this can be written as
-- trippleLetters = intercalate "-" . map (replicate 3)
tripleLetters :: String -> String
tripleLetters xs = init (concat [ replicate 3 x ++ "-" | x <- xs ])

{-
-- Let's use list comprehensions! Note that combinations without replacelemt
-- are just pairing the first element with every successive element then
-- doing the same thing down the line.
twoCombo :: [a] -> [(a,a)]
twoCombo [] = []
twoCombo (x:xs) = [(x,x') | x' <- xs] ++ twoCombo xs
-}

-- We can think of this, if we assume no duplicates, as only taking the ordered
-- ones from all permutations. That'll eliminate duplicate combinations and
-- self-combinations.
twoCombo :: [Int] -> [(Int,Int)]
twoCombo xs = [ (x,x') | x <- xs, x' <- xs, x < x' ]

-- We'll use recursion to help with this.
twoCombo' :: [a] -> [(a, a)]
twoCombo' xs = if null xs
                 then []
                 else [ (head xs, x) | x <- tail xs] ++ twoCombo' (tail xs)

-- | LENNART'S INTENDED SOLUTION
-- Doesn't require recursion!

twoCombo'' :: [a] -> [(a, a)]
twoCombo'' xs = [(a, b) | (a, n) <- zip xs [1..], b <- drop n xs]

-- | However, this may be off limits because it uses pattern matching on a
-- tuple, which is not strictly taught in Chapter 2. Then, we just have to do
-- it this way:

twoCombo''' :: [a] -> [(a, a)]
twoCombo''' xs = [(fst t, b) | t <- zip xs [1..], b <- drop (snd t) xs]

-- | which uses nothing that's not taught in Chapter 2, including no recursion.

-- Types:
types1 = "hello" :: String -- or [Char]
types2 = 3.0 :: Double -- preferred to Float (for technical reasons)
types3 = [1,3,5] :: [Int] -- or [Integer] or (Integral a) => [a]
func1 :: [a] -> [a] -> [a]
func1 a b = a ++ b
types4  = show 100 :: String -- or [Char]
func2 :: Int -> Int -- even better: (Num a) => a -> a
func2 a = a * a 
types5 = tail :: [a] -> [a]
func3 :: [Int] -> [Int] -- even better: (Enum a) => [a] -> [a]
func3 x = [succ e | e <- x]
func4 :: (Show a, Show b) => [a] -> [b] -> [(String,String)]
func4 x y = [ (show a, show b) | a <- x, b <- y ] 

