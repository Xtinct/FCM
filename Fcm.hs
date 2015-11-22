module Fcm
(
  generateRandomMatrix,
  generateVector,
  generateMembershipMatrix,
  normalize,
  calcCenters
) where

import System.Random
import Data.List.Split.Internals
import MathUtils
import Data.List


normalize :: Int -> [[Float]] -> [[Float]]
normalize cs xs = map normalize' xs
  where
    normalize' xs = sub (equate xs) xs
    sub s xs = map (subtract s) xs
    equate x = (sum x - 1) / fromIntegral cs

generateMembershipMatrix :: StdGen -> Int -> Int -> [[Float]]
generateMembershipMatrix g cs vs = normalize cs $ generateRandomMatrix g cs vs

generateRandomMatrix :: StdGen -> Int -> Int -> [[Float]]
generateRandomMatrix gen cs vs = chunksOf cs $ generateVector gen size
  where
    size = cs*vs

-- taking range of (0.1..1) instead of (0..1) to prevent negative numbers
generateVector :: StdGen -> Int -> [Float]
generateVector gen vs = take vs $ randomRs (0.1,1) gen :: [Float]

calcCenters :: [[Float]] -> [[Float]] -> [[Float]]
calcCenters xs ws = map (fraction xs) $ transpose ws
  where
    fraction xs ws = multVectorByValue  (summate xs ws) $ (rollFraction . sum $ map (^fuzziness) ws)
    summate xs ws = vectorsSum $ zipWith ( \weight vector -> multVectorByValue vector (weight^fuzziness) ) ws xs
    fuzziness = 1