module MathUtils
(
  hammingDistance,
  euclideanDistance,
  invertedMultiply,
  multVectorByValue,
  vectorsSum,
  matrixMaximum,
  diff
) where

import Data.List

invertedMultiply :: (Fractional a) => a -> a
invertedMultiply = (/) 1

vectorsSum :: Num a => [[a]] -> [a]
vectorsSum = foldl1' (zipWith (+))

multVectorByValue :: (Floating a) => [a] -> a -> [a]
multVectorByValue vec val = map (*val) vec

hammingDistance :: (Floating a) => [a] -> [a] -> a
hammingDistance x y = sqrt . sum . map (abs) $ zipWith (-) x y

euclideanDistance :: (Floating a) => [a] -> [a] -> a
euclideanDistance x y = sqrt . sum . map (^2) $ zipWith (-) x y

diff :: (Num a) => [[a]] -> [[a]] -> [[a]]
diff = zipWith (zipWith (-))

matrixMaximum :: (Num a, Ord a) => [[a]] -> a
matrixMaximum = maximum . map (maximum)
