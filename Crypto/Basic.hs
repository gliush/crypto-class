module Crypto.Basic where

import Data.Word (Word8)
import Data.Bits (xor)
import Control.Monad (liftM2)
import Data.List.Split (splitEvery)
import Text.Printf (printf)
import Data.List (sortBy)

possibleLetters :: [Word8]
possibleLetters = [32..126]

possibleXors = sortBy cmp [ (wordToChar a,wordToChar b,xor a b)
    | a <- possibleLetters, b <- possibleLetters]
  where
    cmp (_,_,a) (_,_,b) = compare a b

charToWord :: Char -> Word8
charToWord = fromIntegral . fromEnum

wordToChar :: Word8 -> Char
wordToChar = toEnum . fromIntegral

charsToWords :: [Char] -> [Word8]
charsToWords = map charToWord

hexsToWords :: [Char] -> [Word8]
hexsToWords s = map (read . ("0x" ++)) $ splitEvery 2 s

wordsToHexs :: [Word8] -> String
wordsToHexs = concatMap (printf "%02x") 

xorTexts :: [Word8] -> [Word8] -> [Word8]
xorTexts a b = [ xor x y | (x,y) <- zip a b ]

xorChar :: Char -> Char -> Word8
xorChar a b = xor (charToWord a) (charToWord b)
