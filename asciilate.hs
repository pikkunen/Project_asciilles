

asciilate :: [[Int]] -> [[Char]]
asciilate [] = []
asciilate ((xs):(ys):(zs):ls) = asciilate' (xs, ys, zs) : asciilate ls

    
asciilate' :: ([Int],[Int],[Int]) -> String	
asciilate' ([],[],[]) = []
asciilate' ((a:b:c:xs),(d:e:f:ys),(g:h:i:zs))
    | mean < 5  =  (' ': asciilate' (xs,ys,zs)) 
    | mean < 50 =  ('.': asciilate' (xs,ys,zs))
    | mean < 100 = ('+': asciilate' (xs,ys,zs)) 
    | mean < 150 = ('?': asciilate' (xs,ys,zs)) 
    | mean < 200 = ('&': asciilate' (xs,ys,zs))
    | otherwise =  ('#': asciilate' (xs,ys,zs))
    where 
	mean = average [a, b, c, d, e, f, g, h, i]    
        
        
average :: [Int] -> Int
average xs = foldl (+) 0 xs `div` length xs

-- ([234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129])

-- [[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129],[234, 23, 34, 234, 68, 129]]