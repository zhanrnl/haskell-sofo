-- Solutions to PS4.

-- For Map.
import qualified Data.Map as M

-- For the histogram function, it's useful to see what Data.Map has to offer.
makeHistogram :: (Ord a) => [a] -> M.Map a Int
makeHistogram = foldr countElement M.empty
    where countElement elt map = M.insert elt (count elt map + 1) map
          count = M.findWithDefault 0

-- Even better:
makeHistogram' :: (Ord a) => [a] -> M.Map a Int
makeHistogram' = foldr incCount M.empty
    where incCount x = M.insertWith (+) x 1

-- See Passwords.hs for the solution to Problem 2.

-- bindMaybe is pretty self-explanatory.
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  `bindMaybe` _ = Nothing
(Just x) `bindMaybe` f = f x

-- We can do this by traversing the list exactly once, so let's use a fold.
-- Specifically, because we're building up a list, use a right fold.
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr consJusts []
    where consJusts Nothing  = id
          consJusts (Just x) = (x:)

-- Again, this can be done by traversing the list once from the right, so
-- we'll use a right fold.
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr consMappedJusts []
    where consMappedJusts x = case f x of
              Nothing  -> id
              (Just y) -> (y:)

-- Note that (->) is right-associative, so this definition is type-equivalent
-- and performs the same lifting action.
(+++) :: (b -> c) -> (b' -> c') -> Either b b' -> Either c c'
(+++) f f' (Left b)   = Left (f b)
(+++) f f' (Right b') = Right (f' b')
{-
-- This is equivalent to the following:
f +++ f' = \e -> case e of
    (Left b)   -> Left (f b)
    (Right b') -> Right (f' b')
-}

-- Ok.
silliness :: Either a (Either b c) -> Either (Either a b) c
silliness (Left a)          = Left (Left a)
silliness (Right (Left b))  = Left (Right b)
silliness (Right (Right c)) = Right c

-- See the module SkewHeap for the solution to the SkewHeap problem.
