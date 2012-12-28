-- Solutions to PS3.

-- For the strict foldl.
import Data.Foldable(foldl')

-- Types.
type1 = (+3) :: (Integral a) => a -> a
func1 :: [Char] -> Bool
func1 = elem 'a'
func2 :: (Integral a) => [b] -> [(a,b)]
func2 = zip [1..]
type4 = (`div` 10) :: (Integral a) => a -> a
type5 = map :: (a -> b) -> [a] -> [b]
type6 = filter even :: (Integral a) => [a] -> [a]
type7 = show . (+1) :: (Show a, Integral a) => a -> String
func3 :: (a -> a) -> [a] -> [a]
func3 f xs = map (f . f) xs
func4 :: (Integral a) => (Char -> a -> a) -> a
func4 f = foldr f 0 "Hello, World!"
type10 = zipWith ($) :: [a -> b] -> [a] -> [b]
-- The function is curried, it also takes a list of [a -> Bool].
func5 :: [a] -> [a -> Bool] -> [[a]]
func5 xs = map (\p -> takeWhile p xs)
-- b is showable, a is a list.
func6 :: (Show a) => [String] -> [a] -> [String]
func6 = foldl (\a b -> show b : a)
-- (.) = \f g x -> f (g x)
-- (.).(.) = \x -> (.) ( (.) x)
-- = \x -> (\f' g' x' -> f' (g' x')) (\g'' x'' -> x (g'' x''))
-- = \x -> (\g' x' -> (\g'' x'' -> x (g'' x'')) (g' x'))
-- = \x g' x' -> (\x'' -> x ((g' x') x'')
-- = \x g' x' x'' -> x (g' x' x'')
-- = \x g' -> (\x' x'' -> x (g' x' x''))
type13 = (.).(.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- The function builds a function that takes a list, and applies each function
-- in the list to f, then composes g with each resulting function.
type14 = (\f g -> map (. g) . map ($ f)) :: a -> (d -> b) -> [a -> b -> c] -> [d -> c]

-- Let's do it with raw recursion in the helper function. We could probably
-- do it with a fold or something as well, but this is easier to write.
mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = mapI 0
    where mapI _ []     = []
          mapI n (x:xs) = f n x : mapI (n+1) xs

-- It's actually a bit harder to do with a fold because it's difficult to
-- choose a folding direction: we may choose foldl so that we can start
-- indexing from the left (start) at zero, but then we build the list in
-- the wrong order; we could use foldr, but then we need to know what index
-- to use as the start (or we just feed in reverse indices, as below). A
-- more advanced technique is to use DiffLists, which would allow us to build
-- the list with a foldl without the inefficiency of (++), but we're not
-- there yet.
mapWithIndex' :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex' f = fst . foldr (\x (acc, n) -> (f n x : acc, n+1)) ([], 0)

-- All, using recursion. Note that we could also specify that the list must
-- contain at least a single element, but this isn't what the builtin one
-- does anyway.
all' :: (a -> Bool) -> [a] -> Bool
all' _ []                 = True
all' f (x:xs) | f x       = all' f xs
              | otherwise = False

-- All, using a fold. I like foldr, so I'll use it, but you could use any fold.
all'' :: (a -> Bool) -> [a] -> Bool
all'' f = foldr (\x all -> if f x then all else False) True

-- I'll get to these
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ []     = error "foldr1': list must not be empty"
foldr1' _ [x]    = x
foldr1' f (x:xs) = f x (foldr1' f xs)

-- I'm going to have some fun here and implement it by considering folding 
-- function application :) that'll bend the mind a bit.
-- [] -> id
-- [2] -> (\x -> f x (id 2))
-- [3,2] -> (\x -> f x (f 3 (id 2)))
-- [5,3,2] -> (\x -> f x (f 5 (f 3 (id 2))))
-- Thus, we fold over the tail of xs, then plug in the head of xs to compute.
foldr1'' :: (a -> a -> a) -> [a] -> a
foldr1'' _ []     = error "foldr1'': list must not be empty"
--foldr1'' f (x:xs) = foldr (\x acc -> \x' -> f x' (acc x)) id xs x
--foldr1'' f (x:xs) = foldr (\x acc x' -> f x' (acc x)) id xs x
foldr1'' f (x:xs) = foldr (\x g x' -> f x' (g x)) id xs x
-- Or, if you want to be really obscure:
--foldr1'' f (x:xs) = foldr (flip ((flip f).)) id xs x

-- I'm using the strict version of foldl so that the stack doesn't overflow
-- with thunks for long lists. It is located in Data.Foldable.
product' :: (Num a) => [a] -> a
product' = foldl' (*) 1

-- Let's see if we can't eliminate the lambda and make it point free.
maxLength :: [[a]] -> Int
--maxLength xss = foldr (\xs maxlen -> max (length xs) maxlen) 0 xss
maxLength = foldr (max . length) 0

-- It doesn't specify which direction to compose, so I'll assume we need to
-- apply the rightmost functions first (the logical choice).
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- Let's see if we can't make this point free again.
map' :: (a -> b) -> [a] -> [b]
--map f = foldr (\x list -> f x : list) []
--map f = foldr (\x list -> (:) (f x) list) []
map' f = foldr ((:) . f) []

-- Hmm...it seems to jsut ignore its second parameter.
const2 :: a -> b -> a
const2 x _ = x
--const2 x = (\_ -> x)

-- This looks like function composition.
fcomp :: (a -> b) -> (b -> c) -> a -> c
fcomp f g x = g (f x)
-- fcomp f g = (\x -> g (f x))
-- fcomp = flip (.)

-- Hmm...it looks like it wants us to give the first function a (b -> a)
-- function, but we only have an a. However, this also looks similar to
-- const2 above.
appc :: ((b -> a) -> c) -> a -> c
appc f x = f (const x)
--appc f = (\x -> f (const x))

-- It looks like it takes a function and modifies each of its parameters
-- beforehand with modification functions, returning a new function that
-- uses the modified parameters. 
on2 :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
on2 f g h = (\x y -> f (g x) (h y))

-- It looks like it takes the first and third functions and composes them so
-- that it can be fed into the second function.
fmapC :: (a -> b) -> ((a -> r) -> r) -> (b -> r) -> r
fmapC f g h = g (h . f)

-- It looks like it feeds the third function as the second argument to the
-- second function, then feeds that result to the first function.
bindC :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r
bindC f g h = f ((flip g) h)

-- Fibonacci numbers! Yay! Let's do this multiple ways.
-- First, an efficient C solution translated to Haskell. Meh.
fib :: Integer -> Integer
fib n = fibBase 0 1 n
    where fibBase a _ 0 = a
          fibBase a b n = fibBase b (a+b) (n-1)

-- The same thing, but more Haskelly.
fib' :: Int -> Integer
fib' n = fibs 0 1 !! n
    where fibs a b = a : fibs b (a + b)

-- But even cooler, an infinite list of fibonacci numbers.
fibs' :: [Integer]
fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')
fib'' :: Int -> Integer
fib'' n = fibs' !! n

-- The same thing, but with scanning. This works because the accumulator is
-- always one step ahead of the folding.
fibs'' :: [Integer]
fibs'' = 0 : scanl (+) 1 fibs''
fib''' :: Int -> Integer
fib''' n = fibs'' !! n

-- Ok, this is a bit tricky, but the technique required is similar to that
-- used in foldr1'' above.
-- foldl (-) k      [] = \k -> id k
-- foldl (-) k     [2] = \k -> id ((-) k                 2)
-- foldl (-) k   [3,2] = \k -> id ((-) ((-) k         3) 2)
-- foldl (-) k [5,3,2] = \k -> id ((-) ((-) ((-) k 5) 3) 2)
-- So it looks like I just pass each successive computation into k, all the
-- way down the line. Thus, I am composing the accumulator on top of our
-- function, which keeps the last computation at the outermost level. This
-- composition makes a new accumulator. Note that each successive computation
-- substitutes in (-) k x for k, so this will be the value to pass to the
-- accumulator. The final function needs to be id so that the result is given.
foldlInR :: (a -> b -> a) -> a -> [b] -> a
--foldlInR f k xs = foldr (\x g -> (\c -> g (f c x))) id xs k
foldlInR f k xs = foldr (\x g c -> g (f c x)) id xs k
