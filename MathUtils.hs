module MathUtils
(
  hammingDistance,
  euclideanDistance,
  rollFraction,
  multVectorByValue,
  vectorsSum
) where

import Data.List

rollFraction :: (Fractional a) => a -> a
rollFraction = (/) 1

vectorsSum :: Num a => [[a]] -> [a]
vectorsSum = foldl1' (zipWith (+))

multVectorByValue :: (Floating a) => [a] -> a -> [a]
multVectorByValue vec val = map (*val) vec

hammingDistance :: (Floating a) => [a] -> [a] -> a
hammingDistance x y = sqrt . sum . map (abs) $ zipWith (-) x y

euclideanDistance :: (Floating a) => [a] -> [a] -> a
euclideanDistance x y = sqrt . sum . map (^2) $ zipWith (-) x y

