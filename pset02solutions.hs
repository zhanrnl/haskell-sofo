dayName :: Int -> String
dayName 0 = "Sunday"
dayName 1 = "Sunday"
dayName 2 = "Sunday"
dayName 3 = "Sunday"
dayName 4 = "Sunday"
dayName 5 = "Sunday"
dayName 6 = "Sunday"
dayName _ = "Day out of range"

validatePassword :: String -> Bool
validatePassword s =
  let len = length s
  in len > 5 && len < 20

analyzeTriangle :: (Float, Float, Float) -> (Float, Float)
analyzeTriangle tri@(a, b, c)
  | valid = (area, perimeter)
  | otherwise = (-1.0, -1.0)
  where perimeter = a + b + c
        valid = let maxSide = max a (max b c)
                in perimeter > 2 * maxSide
        area = let s = perimeter / 2
               in sqrt(s * (s - a) * (s - b) * (s - c))

gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)

init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x:(init' xs)

cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
  let mid = (length xs) `div` 2
      (xs1, xs2) = splitAt mid xs
  in merge (mergesort xs1) (mergesort xs2)
  where merge [] bs = bs
        merge as [] = as
        merge allas@(a:as) allbs@(b:bs)
          | a < b = a : (merge as allbs)
          | otherwise = b : (merge allas bs)

sum2 :: Num a => [a] -> a
sum2 ns = sum2' ns 0
  where sum2' [] s = s
        sum2' (n:ns) s = sum2' ns (s + n)

reverse2 :: [a] -> [a]
reverse2 xs = reverse2' xs []
  where reverse2' [] bs = bs
        reverse2' (f:fs) bs = reverse2' fs (f:bs)

ppParens :: String -> String
ppParens p = reverse (tail (ppParens' p 0 ""))
  where ppParens' "" _ s = s
        ppParens' ('(':ps) n s =
          ppParens' ps (n+1) ("\n(" ++ (replicate n '\t') ++ s)
        ppParens' (')':ps) n s =
          ppParens' ps (n-1) ("\n)" ++ (replicate (n-1) '\t') ++ s)

-- | CHALLENGE PROBLEM
-- Several bits could be a little bit nicer with higher-order functions, but
-- this is all doable with what we've got. Lazy evaluation!

coprimePerm :: [Int] -> [Int]
coprimePerm ns = 
  case (coprimePerm' [] ns []) of
    [] -> [-1]
    rs -> reverse (head rs)

coprimePerm' :: [Int] -> [Int] -> [[Int]] -> [[Int]]
coprimePerm' built [] results = built : results
coprimePerm' built remain results =
  let selects = [ns | ns@(n, _) <- selectOff remain,
                 null built || coprime n (head built)]
  in concat [coprimePerm' (n:built) l results | (n, l) <- selects]

selectOff :: [Int] -> [(Int, [Int])]
selectOff xs = [(b, fs ++ bs) | n <- [0..(length xs - 1)],
                let (fs, (b:bs)) = splitAt n xs]

coprime :: Int -> Int -> Bool
coprime a b = (gcd a b) == 1
