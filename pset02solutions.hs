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

ppParens :: String -> String
ppParens p = reverse (tail (ppParens' p 0 ""))
  where ppParens' "" _ s = s
        ppParens' ('(':ps) n s =
          ppParens' ps (n+1) ("\n(" ++ (replicate n '\t') ++ s)
        ppParens' (')':ps) n s =
          ppParens' ps (n-1) ("\n)" ++ (replicate (n-1) '\t') ++ s)