-- PS5 solutions.

-- Imports.
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Foldable as F

-- Problem 1: Although Either is usually used in Haskell to represent the
-- result of a computation that might fail, with the successful result
-- stored in Right and some sort of error state or message stored in Left,
-- it might in some circumstances be useful to have an instance of fmap that
-- mapped over the left value instead of the right. Find a way to implement
-- an fmap instance that maps over the Left value, and use it to write a
-- function:
-- mapLeft :: (a -> b) -> Either a c -> Either b c
-- (The function is just so that the user doesn't have to use you custom
-- Functor explicitly.)
-- (Hint: this might be a good place to use newtype.)

newtype FlipEither a b = FlipEither { getEither :: Either b a }
                       deriving (Show)

instance Functor (FlipEither a) where
    fmap f (FlipEither (Left x))  = FlipEither (Left (f x))
    fmap _ (FlipEither (Right x)) = FlipEither (Right x)

-- Also, a helper function.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = getEither . fmap f . FlipEither

-- Problem 2a: You should already be familiar with Maps from Data.Map. We don't
-- have access to the internals of Data.Map so as to implement fmap
-- efficiently, but we can get access to elements through the toList and
-- fromList functions. Write a function fmapMap that uses these functions to
-- implement fmap for Data.Map. (You can double-check your behavior against
-- Data.Map.map.)

fmapMap :: (Ord k) => (a -> b) -> M.Map k a -> M.Map k b
fmapMap f = M.fromList . fmap (\(k,v) -> (k,f v)) . M.toList

-- Note that, while the lambda expression is verbose (which is often good),
-- it is needlessly verbose here: all it does is apply f to the second value
-- of the pair. This seems like it would be a good implementation for fmap on
-- a pair, and indeed it turns out that fmap is defined for a pair as:
-- fmap :: (a -> b) -> (c,a) -> (c,b)
-- with a potential implementation:
-- fmap f (x,y) = (x,f y)
-- or:
-- fmap f = \(x,y) -> (x,f y)
-- Thus, we may replace the lambda with another fmap.

fmapMap' :: (Ord k) => (a -> b) -> M.Map k a -> M.Map k b
fmapMap' f = M.fromList . fmap (fmap f) . M.toList

-- Problem 2b: Now, see if you can reimplement fmapMap in terms of a fold on
-- the Map. It might be helpful to see what folds are available.

-- Now, I know I want to make a function that folds into a Map, so I need the
-- folding function to include the key as well as the value. I thus decided
-- on foldrWithKey. In this case, foldrWithKey has type:
-- (k -> a -> Map k b -> Map k b) -> Map k b -> Map k a -> Map k b
fmapMap'' :: (Ord k) => (a -> b) -> M.Map k a -> M.Map k b
--fmapMap'' f = M.foldrWithKey (\k v m -> M.insert k (f v) m) M.empty
--fmapMap'' f = M.foldrWithKey (\k v -> M.insert k (f v)) M.empty
fmapMap'' f = M.foldrWithKey (\k -> M.insert k . f) M.empty

-- Problem 3a: Create a type called Logger that stores a value plus some sort
-- of log value alongside it. Such a type might be useful if we wanted to
-- run through a computation while keeping some sort of metadata on the
-- values as well.

-- Here's the meh way. It's really a bit much considering we're just storing
-- something alongside a value. (There are additional reasons, but they're
-- from what's coming.)
data Logger' l a = Logger' a l deriving (Show)

-- I don't want to have to define my own accessors.
--getValue' (Logger' x _) = x
--getLog' (Logger' _ l) = l

-- We could also do:
data Logger'' l a = Logger'' { value :: a, log :: l } deriving (Show)

-- This is a very good definition and usually would be what we'd use, but
-- (for reasons that will become apparent later) we're going to use, at
-- least for the solutions, the following definition:

-- What we'll use:
newtype Logger l a = Logger { getLog :: (a,l) } deriving (Show)

-- Problem 3b: Recognizing that the Logger type represents a value in the
-- context that there's a log or some sort of metadata attached, write the
-- Functor instance for Logger.

-- Simple enough: we just change the value without modifying the log.
instance Functor (Logger l) where
    fmap f (Logger (x,l)) = Logger (f x,l)

-- Problem 3c: Show that your Functor instance obeys the Functor laws.

-- The first law is that fmap id = id. Let's put that to the test:
-- fmap id (Logger (x,l)) = Logger (id x,l)
--                        = Logger (x,l)
--     -> Logger (x,l) = Logger (x,l)
-- Yay! Next, we have fmap (f . g) = fmap f . fmap g
-- fmap (f . g) (Logger (x,l)) = Logger (f . g $ x, l)
-- fmap f (fmap g (Logger (x,l)) = fmap f (Logger (g x, l))
--                               = Logger (f (g x), l)
--     -> Logger (f (g x), l) = Logger (f (g x), l)
-- Cool! We're a valid functor!

-- Problem 3d: Right now, our logging facilities are pretty poor. Write two
-- functions, one that wraps a value with a log and another that performs
-- computations on values with logs attached:
-- addLog :: l -> a -> Logger l a
-- computeWithLog :: (?) => Logger l a -> (a -> Logger l b) -> Logger l b
-- The first function should take a log and a value and return that value with
-- the log attached. The second function is a bit more complicated: it
-- represents applying a function to a value with a log attached, and having
-- the resulting value have both logs attached. For example, let's say that
-- we have a value 4 with an attached log of "value of first die", and we
-- want to add the value 3 with an attached log " plus value of second die";
-- computeWithLog should then return the value 7 with an attached log
-- "value of first die plus value of second die". Part of the challenge is
-- finding a generic way of combining different types of logs, without
-- writing one function to combine strings, another to combine numbers, etc.

addLog :: l -> a -> Logger l a
addLog l x = Logger (x,l)

-- We want a generic means of combining things, so (given what we learned
-- in LYAH 11) we probably want a monoid. Monoids give us exactly the
-- generic framework we need to represent log values that can be combined
-- with other log values.
computeWithLog :: (Monoid l) => Logger l a -> (a -> Logger l b) -> Logger l b
computeWithLog (Logger (x, l1)) f =
    let (Logger (y, l2)) = f x
    in  Logger (y, l1 `mappend` l2)

-- Problem 3e: We now have functions to compute Logger values given Logger
-- values and functions that return Logger values. These are great and all,
-- but it would be nice if one didn't have to write a new function every time
-- one wanted to attach a log to the output of an existing function. (Even
-- more important, it's cleaner to write reusable code and then attach
-- logs to that than to integrate logging into the main logic.) Write a
-- function withLog that takes a function that produces a normal value and
-- makes it return a Logger value instead.

withLog :: l -> (a -> b) -> a -> Logger l b
--withLog l f x = Logger (f x,l)
-- Note that the right hand side is equivalent to fmap f (Logger (x,l)). We
-- can use that and our addLog function to clean this up:
withLog l f = fmap f . addLog l

-- Problem 4a: Write an instance of Monoid that tracks the minimum value of
-- some type. Any new type should simply wrap the raw value.

-- We need to have a bounded type in order to have an additive identity,
-- and we need an orderable type for there to be a minimum.
-- Also, we need to use a newtype declaration to give whatever type we use
-- a new identity as a minimum monoid.

newtype Minimum a = Minimum { getMinimum :: a } deriving (Show)

instance (Bounded a, Ord a) => Monoid (Minimum a) where
    mempty = Minimum maxBound
    Minimum x `mappend` Minimum y = Minimum (min x y)

-- Problem 4b: Rewrite the minimum monoid to contain a Maybe value rather than
-- a raw value. This allows us to be a bit more general in what can be treated
-- as a Minimum. Call this new monoid MMinimum.

newtype MMinimum a = MMinimum { getMMinimum :: Maybe a } deriving (Show)

instance (Ord a) => Monoid (MMinimum a) where
    mempty = MMinimum Nothing
    MMinimum Nothing `mappend` m = m
    m `mappend` MMinimum Nothing = m
    MMinimum (Just x) `mappend` MMinimum (Just y) = MMinimum . Just $ min x y

-- Note that you will be able to rewrite the last mappend method above to be
-- more concise and expressive later, once we've done applicative functors.
--MMinimum x `mappend` MMinimum y = MMinimum (min <$> x <*> y)

-- Problem 4c: Are functions monoids? Create a sample function monoid instance
-- and see if it has the properties of a monoid.

-- First, it would be impossible to combine functions with different input
-- and output types generically: how to I do (a -> b) `mappend` (a -> b)?
-- So let's make it so that we have to return the same type as our parameter.

newtype MFunction a = MFunction { getMFunction :: (a -> a) }

instance Monoid (MFunction a) where
    mempty = MFunction id
    MFunction f `mappend` MFunction g = MFunction (f . g)

-- Again, the last could be written more concisely, but we can't do that yet.

-- Now, are functions monoids? Well, for something to be a monoid it must have
-- an associative binary operation and an identity element, and they must
-- obey the following laws:
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
--
-- mempty `mappend` (MFunction f) = (MFunction id) `mappend` (MFunction f)
--                                = MFunction (id . f)
--                                = Mfunction f
--     -> MFunction f = MFunction f
-- (MFunction f) `mappend` mempty = (MFunction f) `mappend` (MFunction id)
--                                = MFunction (f . id)
--                                = MFunction f
--     -> MFunction f = MFunction f
-- ((MFunction f) `mappend` (MFunction g)) `mappend` (MFunction h)
--     = (MFunction (f . g)) `mappend` (MFunction h)
--     = MFunction ((f . g) . h)
--     = MFunction (f . g . h)
-- (MFunction f) `mappend` ((MFunction g) `mappend` (MFunction h))
--     = (MFunction f) `mappend` (MFunction (g . h))
--     = MFunction (f . (g . h))
--     = MFunction (f . g . h)
--     -> MFunction (f . g . h) = MFunction (f . g . h)
--
-- So it indeed does appear that functions can be Monoids. In fact, the id
-- function seems almost expressly designed to be the monoidal identity, and
-- function composition seems almost expressly designed to be the monoidal
-- addition operation. This makes sense intuitively, since we want functions
-- to share various properties with other abstract frameworks such as numbers.
--

-- Problem 5a: Give the types of the following expressions:
-- 1. fmap (+3)
-- 2. fmap (+)
-- 3. fmap id
-- 4. fmap ($)
-- 5. fmap (.)
-- 6. fmap const
-- 7. fmap map

-- fmap (+3) :: (Num a, Functor f) => f a -> f a
-- fmap (+) :: (Num a, Functor f) => f a -> f (a -> a)
-- fmap id :: (Functor f) => f a -> f a
-- fmap ($) :: (Functor f) => f (a -> b) -> f (a -> b)
-- fmap (.) :: (Functor f) => f (b -> c) -> f ((a -> b) -> a -> c)
-- fmap const :: (Functor f) => f a -> f (b -> a)
-- fmap map :: (Functor f) => f (a -> b) -> f ([a] -> [b])

-- Problem 5b: What's the type of fmap fmap fmap? Determine the type manually
-- (not by typing ":t fmap fmap fmap" into ghci).
-- (Hint: Remember that all functions in Haskell are curried. It might be
-- helpful to first determine the type of fmap fmap.)

-- fmap :: (Functor f) => (a -> b) -> f a -> f b
-- (a -> b)                       -> f a -> f b
-- (a1 -> b1) -> (f1 a1 -> f1 b1)
-- a = (a1 -> b1)
-- b = (f1 a1 -> f1 b1)
--
-- fmap fmap :: f (a1 -> b1) -> f (f1 a1 -> f1 b1)
-- f (a1 -> b1)                   -> f (f1 a1 -> f1 b1)
-- (a2 -> b2) -> (f2 a2 -> f2 b2)
-- Because we need a functor instance, and the only functor we can see is
-- a function, let's rewrite the last as:
-- ((->) (a2 -> b2)) (f2 a2 -> f2 b2)
-- f = ((->) (a2 -> b2)) 
-- a1 = f2 a2
-- b1 = f2 b2
-- 
-- fmap fmap fmap :: ((->) (a2 -> b2)) (f1 (f2 a2) -> f1 (f2 b2))
--
-- fmap fmap fmap :: (Functor f, Functor f1) =>
--                   (a -> b) -> f (f1 a) -> f (f1 b)
--

-- Problem 5c: Using the results from the previous part, find a combination
-- of fmaps that yield a function with type:
-- (Functor f, Functor f1, Functor f2) =>
--   (a -> b) -> f (f1 (f2 a)) -> f (f1 (f2 b))

-- Remember that in the intermediate step in part b, fmap fmap had type:
-- f (a -> b) -> f (f1 a -> f1 b)
-- When we applied it to fmap, the Haskell typesystem inferred that the functor
-- instance implicit in fmap is (-> r). Thus, fmap fmap lifted fmap to work
-- one level deeper inside a Functor. If we substitute ((->) (a1 -> b1)) for f
-- in the type of fmap fmap, we get a return type of:
-- ((->) (a1 -> b1)) (f1 a -> f1 b)
-- (a1 -> b1) -> f1 a -> f1 b
-- That is, applying fmap fmap to some variant of fmap lifts the function it is
-- applied to to work on functors of some types related to the original types.
-- In fmap fmap (fmap), the related type is a Functor of the original type.
-- However, fmap fmap fmap works just like fmap but one level deeper, so we
-- should theoretically be able to call fmap fmap on fmap fmap fmap to go
-- another level:
--
-- fmap fmap (fmap fmap fmap)
--
-- fmap fmap (fmap fmap fmap) :: (Functor f, Functor f1, Functor f2) =>
--                               (a -> b) -> f (f1 (f2 a)) -> f (f1 (f2 b))
--

-- Problem 6: Implement F.foldr in terms of F.foldMap.

-- foldMap has the type:
-- (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- We're going to have to use some sort of monoid to accomplish the fold, so
-- what do we use? 
foldrF :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
foldrF f k t = 

