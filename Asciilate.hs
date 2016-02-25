module Asciilate (asciilate, asciilate', average) where


{- asciilate list
   PURPOSE:  converts greyscale values of pixels to suitable ascii characters
   PRE:      all values in list should be between 0 and 255, all lists in list should be of equal length.
   POST:     a list of strings where every character corresponds to a group of nine values in list. if list contains an amount of lists not divisile by three the last one or two rows will be discarded.
   SIDE EFFECTS: None
   EXAMPLES:
-}
asciilate :: [[Int]] -> [[Char]]
asciilate [] = []
asciilate (xs: []) = []
asciilate (xs:ys:[]) = []
asciilate ((xs):(ys):(zs):ls) = asciilate' (xs, ys, zs) : asciilate ls


{- asciilate' row
   PURPOSE:  auxillary function for asciilate
   PRE:      all values should be between 0 and 255, all listns in row should be of equal length
   POST:     a string which corresponds to groups of values from the row. if the length of a list is not divisible by three, the last one or two elements will be discarded.
   SIDE EFFECTS: None
   EXAMPLES:
-}
asciilate' :: ([Int],[Int],[Int]) -> String
asciilate' ([],[],[]) = []
asciilate' ((a:[]),(d:[]),(g:[])) = []
asciilate' ((a:b:[]),(d:e:[]),(g:h:[])) = []
asciilate' ((a:b:c:xs),(d:e:f:ys),(g:h:i:zs))
    | mean < 10  = ' ': asciilate' (xs,ys,zs)
    | mean < 30  = '.': asciilate' (xs,ys,zs)
    | mean < 45  ='\'': asciilate' (xs,ys,zs)
    | mean < 60  = '*': asciilate' (xs,ys,zs)
    | mean < 80  = ':': asciilate' (xs,ys,zs)
    | mean < 100 = '+': asciilate' (xs,ys,zs)
    | mean < 130 = '?': asciilate' (xs,ys,zs)
    | mean < 170 = '&': asciilate' (xs,ys,zs)
    | mean < 200 = '#': asciilate' (xs,ys,zs)
    | otherwise  = '@': asciilate' (xs,ys,zs)
    where
	mean = average [a, b, c, d, e, f, g, h, i]


{- average list
   PURPOSE:  calculates the average value in a list
   PRE:      True
   POST:     the average value in list, rounded to nearest lower value
   SIDE EFFECTS: None
   EXAMPLES: average [2, 4, 2] = 2
-}
average :: [Int] -> Int
average xs = foldl (+) 0 xs `div` length xs

-- ([234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129])

-- [[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129], [234, 23, 34, 234, 68, 129]]

-- [[234, 23, 34, 234, 68], [234, 23, 34, 234, 68],[234, 23, 34, 234, 68],[234, 23, 34, 234, 68],[234, 23, 34, 234, 68],[234, 23, 34, 234, 68]]
