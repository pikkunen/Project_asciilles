module Asciilate (asciilate, average) where

import Utils
import Test.QuickCheck
import Data.Maybe
import Data.List

{- asciilate scale list
   PURPOSE:  converts greyscale values of pixels to suitable ascii characters
   PRE:      all values in list should be between 0 and 255, all lists in list should be of equal length.
   POST:     a list of strings where every character corresponds to a group consisting of a square with side scale of values from
             list. if list contains an amount of lists not divisile square, or if any of the rows does, then a few pixels will.
             be discarded untill it fits
   SIDE EFFECTS: None
   EXAMPLES:
-}
asciilate :: Int -> [[Int]] -> [[Char]]
asciilate _ [] = []
asciilate scale lss =
  let rows = take scale lss
  in  if length rows == scale
        then asciilate' scale rows : asciilate scale (drop scale lss)
        else []


-- Auxilary function for asciilate which maps a group of rows to a row of characters

asciilate' :: Int -> [[Int]] -> [Char]
asciilate' _ [] = ""
asciilate' scale lss
  | length (head lss) >= scale =
    let mean = fromIntegral . average $ lss >>= take scale
    in  calculateSymbol mean : asciilate' scale (map (drop scale) lss)
  | otherwise = ""

symbols = "#W@O%$ioc*;:+!^'`-. "

-- given a grey value (0 - 255) finds the apropriate symbol where darker greys have symbols with more "ink"
calculateSymbol :: Int -> Char
calculateSymbol grey = symbols !! (floor $ (fromIntegral grey / 255) * fromIntegral (length symbols - 1))


-- the following is for tests

-- The inverse of calculateSymbol. Note that it is not a true inverse as multiple grey values lead to the same symbol.
-- This function will return the smallest grey value that matches that symbol instead.
reverseSymbol :: Char -> Int
reverseSymbol symbol
  | symbol `elem` symbols = (*255) . fromJust . (`elemIndex` symbols) $ symbol
  | otherwise             = error $ "symbol must be in symbols. Was: " ++ show symbol

newtype Symbol = Symbol Char deriving Show


fromSymbol :: Symbol -> Char
fromSymbol (Symbol s) = s

instance Arbitrary Symbol where
  arbitrary = elements $ map Symbol symbols

runtests = quickCheck (\lss -> map (map fromSymbol) lss == (asciilate 1 . map (map (reverseSymbol . fromSymbol)) $ lss) )
