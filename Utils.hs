module Utils (average, List2D, getList2D) where

import Test.QuickCheck

average :: Integral a => [a] -> a
average ls = foldl (+) 0 ls `div` fromIntegral (length ls)

-- for testing
-- the newtypes exist so that we can create different Arbitrary instances for them
newtype List2D a = List2D [[a]] deriving (Show)

-- getX is a naming convention for extracting things wraped in newtypes
getList2D :: List2D a -> [[a]]
getList2D (List2D a) = a

instance Arbitrary a => Arbitrary (List2D a) where
  arbitrary = do
    a <- arbitrary
    let a' = map (getNonEmpty) (getNonEmpty a)
    let shortestLength = minimum $ map length a'
    return . List2D $ map (take shortestLength) a'

instance Functor List2D where
  fmap f = List2D . map (map f) . getList2D
