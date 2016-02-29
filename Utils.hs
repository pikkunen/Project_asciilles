module Utils (average, NonEmptyList, fromNonEmptyList, List2D, fromList2D) where

import Test.QuickCheck hiding (NonEmptyList)

average :: Integral a => [a] -> a
average ls = foldl (+) 0 ls `div` fromIntegral (length ls)

-- for testing
-- the newtypes exist so that we can create different Arbitrary instances for them
newtype List2D a = List2D [[a]] deriving (Show)
newtype NonEmptyList a = NonEmptyList [a] deriving (Show)

fromNonEmptyList :: NonEmptyList a -> [a]
fromNonEmptyList (NonEmptyList a) = a

fromList2D :: List2D a -> [[a]]
fromList2D (List2D a) = a

instance Arbitrary a => Arbitrary (List2D a) where
  arbitrary = do
    a <- arbitrary
    let a' = map (fromNonEmptyList) (fromNonEmptyList a)
    let shortestLength = minimum $ map length a'
    return . List2D $ map (take shortestLength) a'

instance Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary = do
    as <- arbitrary
    a <- arbitrary
    return . NonEmptyList $ a:as
