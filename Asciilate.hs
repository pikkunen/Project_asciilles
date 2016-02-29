module Asciilate (asciilate, asciilate', average) where


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

asciilate' :: Int -> [[Int]] -> String
asciilate' _ [] = ""
asciilate' scale lss
  | length (head lss) >= scale =
    let mean = fromIntegral . average $ lss >>= take scale
        symbolIndex = floor $ (mean / 255) * fromIntegral (length symbols - 1); -- a lower mean corresponds to a lower symbol index
        symbols = "#W@O%$ioc*;:+!^'`-. "                                        -- which corresponds to a symbol earlier in symbols
    in  (symbols !! symbolIndex) : asciilate' scale (map (drop scale) lss)
  | otherwise = ""


{- average list
   PURPOSE:  calculates the average value in a list
   PRE:      True
   POST:     the average value in list, rounded to nearest lower value
   SIDE EFFECTS: None
   EXAMPLES: average [2, 4, 2] = 2
-}
average :: [Int] -> Int
average xs = foldl (+) 0 xs `div` length xs
