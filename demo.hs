import Data.Char
import System.Random
import Control.Monad
import Data.List

main = putStrLn "hello, world!"

pythag a b = sqrt (a^2 + b^2)

nonMult5 = [n | n <- [1..100], n `mod` 5 /= 0]

nonMult5' = filter (\n -> n `mod` 5 /= 0) [1..100]

forwardsBackwards str = str ++ (reverse str)

repeat5 str = concat (replicate 5 str) ++ "!"

onlyLetters str = filter isLetter str

helloName = getLine >>= \name -> putStrLn ("Hello, " ++ name)

randomNumbers = replicateM 100 (randomRIO (1, 100) :: IO Int)

maxRandom = fmap maximum randomNumbers

collatzSeq n = n : unfoldr f n
  where f n
          | n == 1 = Nothing
          | even n = d $ n `div` 2
          | otherwise = d $ n * 3 + 1
        d n = Just (n, n)

collatz = length . collatzSeq