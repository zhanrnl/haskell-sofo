-- DeVigenere.hs
-- The DeVigenere problem using Monads.

module DeVigenere
( deVigenere
, plainToKey
, deVigenereEncrypt
, deVigenereDecrypt
) where

import qualified Data.Char as C
import Control.Applicative

-- Letter position.
position :: Char -> Maybe Int
position c
    | C.isAlpha c = Just ((C.ord c `mod` 0x20) - 1)
    | otherwise   = Nothing

-- Letter rotation.
rotateLetter :: Int -> Char -> Maybe Char
rotateLetter s c =
    let newPos = (\p -> ((p + s) `mod` 26) + 1) <$> position c
        ordShift = ((C.ord c) `div` 0x20) * 0x20
    in  (.) <$> pure C.chr <*> pure (ordShift+) <*> newPos

-- Sequence applicatives. Credit: Learn You a Haskell
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

-- |This function takes a key in the form of a list of offsets and performs
-- the corresponding de Vigenere rotations to the given string. It can be
-- used for encryption and decryption (decryption is just using the negative
-- key).
deVigenere :: [Int] -> String -> Maybe String
deVigenere key = sequenceA . zipWith rotateLetter (cycle key)

-- |Converts a plaintext key into a list of offsets suitable for use with
-- the deVigenere function.
plainToKey :: String -> Maybe [Int]
plainToKey = sequenceA . map position

-- |Encrypts the given plaintext using the given plain key using the De
-- Vigenere cipher.
deVigenereEncrypt :: String -> String -> Maybe String
deVigenereEncrypt key plaintext =
    plainToKey key >>= (flip deVigenere) plaintext

-- |Decrypts the given ciphertext using the given plain key using the De
-- Vigenere cipher.
deVigenereDecrypt :: String -> String -> Maybe String
deVigenereDecrypt key ciphertext =
    let decryptionKey = (map negate) <$> plainToKey key
    in  decryptionKey >>= (flip deVigenere) ciphertext

