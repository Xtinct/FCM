{-# LANGUAGE DeriveDataTypeable #-}

module Fcm
(
  normalize,
  generateMembershipMatrix,
  generateRandomMatrix,
  generateVector,
  calcCenters,
  separate,
  separate',
  calcWeights,
  Distance(..)
) where

import System.Random
import Data.List.Split.Internals
import MathUtils
import Data.List
import Data.Data
import Data.Typeable

data Distance = Hamming | Euclidean
  deriving(Eq, Show, Read, Data)

--instance Read Distance where
--  readsPrec "hamming" = Hamming
--  readsPrec "euclidean" = Euclidean


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

generateVector :: StdGen -> Int -> [Float]
generateVector gen vs = take vs $ randomRs (0,1) gen :: [Float]

calcCenters :: Int ->[[Float]] -> [[Float]] -> [[Float]]
calcCenters fuzziness xs ws = map (fraction xs) $ transpose ws
  where
    fraction xs ws = multVectorByValue  (summate xs ws) $ (invertedMultiply . sum $ map (^fuzziness) ws)
    summate xs ws = vectorsSum $ zipWith ( \weight vector -> multVectorByValue vector (weight^fuzziness) ) ws xs

calcWeights :: (Floating a) => Int -> ([a] -> [a] -> a) -> [[a]] -> [[a]] -> [[a]]
calcWeights fuzziness d xs cs = map (weight) xs
  where
    weight x = map (weight' x) cs
    weight' x v = invertedMultiply . sum $ map (\c -> (d x v / d x c)**( 2/fromIntegral (fuzziness-1) )) cs

separate' :: ([Float] -> [Float] -> Float)-> Int -> [[Float]] -> Float -> [[Float]] -> [[Float]]
separate' d fuzziness ws threshold xs =
  case compare (matrixMaximum $ diff ws' ws) threshold of
    GT -> separate' d fuzziness ws' threshold xs
    otherwise -> ws'
  where ws' = calcWeights fuzziness d xs $ calcCenters fuzziness xs ws

separate :: Distance -> Int -> Int -> Float -> [[Float]] -> [[Float]]
separate _ _ _ _ [[]] = []
separate df nc fuzziness threshold xs =
  case df of
    Hamming -> separate' hammingDistance fuzziness startMatrix threshold xs
    Euclidean -> separate' euclideanDistance fuzziness startMatrix threshold xs
  where
    nv = length xs
    startMatrix = generateMembershipMatrix (mkStdGen 0) nc nv