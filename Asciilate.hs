module Asciilate (asciilate, average, runtests) where

import Utils
import Test.QuickCheck
import Data.Maybe
import Data.List

import qualified GHC.Word as W

{- asciilate scale list
   PURPOSE:  converts greyscale values of pixels to suitable ascii characters
   PRE:      all values in list should be between 0 and 255, all lists in list should be of equal length.
   POST:     a list of strings where every character corresponds to a group consisting of a square with side scale of values from
             list. if list contains an amount of lists not divisile square, or if any of the rows does, then pixels will be removed
             untill it is divisible again
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

{- calculateSymbol grey
   PURPOSE:  Find the apropriate symbol for a given grey value
   PRE:      0 <= grey <= 255
   POST:     Each symbol in symbols covers an equal amount of the black-white spectrum. Depending on the value of grey,
             the function will return a different symbol where darker grey get symbols earlier in symbols (and thus have
             more "ink") and brighter grey values result in symbols later in symbols.
   EXAMPLES: getSymbol 0 = '#'
             getSymbol 255 = ' '
             getSymbol 3 * 255 `div` length symbols = symbols !! 3
-}
calculateSymbol :: Int -> Char
calculateSymbol grey = symbols !! min (length symbols - 1) (grey * length symbols `div` 255) -- the min is necessary to make it not crash on 255


-- the following is for tests

-- The inverse of calculateSymbol. Note that it is not a true inverse as multiple grey values lead to the same symbol.
-- This function will return the smallest grey value that matches that symbol instead.
reverseSymbol :: Char -> Int
reverseSymbol symbol
  | symbol `elem` symbols = (`div` (length symbols - 1)) . (*255) . fromJust . (`elemIndex` symbols) $ symbol
  | otherwise             = error $ "symbol must be in symbols. Was: " ++ show symbol

newtype Symbol = Symbol Char deriving Show

getSymbol :: Symbol -> Char
getSymbol (Symbol s) = s

instance Arbitrary Symbol where
  arbitrary = elements $ map Symbol symbols

runtests =
  sequence_
    [quickCheck (\lss -> map (map getSymbol) lss == (asciilate 1 . map (map (reverseSymbol . getSymbol)) $ lss) )
    -- a 2DList of symbols with reverseSymbol called on them that gets asciilated should result in the original symbol
    ,quickCheck ((\lss scale ->
                  let lss' = getList2D $ fmap fromIntegral lss
                      scale' = getPositive scale
                      sss = asciilate scale' lss'
                  in  null sss || length (head sss) == length (head lss') `div` scale' &&
                      length sss == length lss' `div` scale') :: List2D W.Word8 -> Positive Int -> Bool)
    ]
