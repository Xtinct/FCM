module MathUtils
(
  hammingDistance,
  euclideanDistance,
) where

hammingDistance :: (Floating a) => [a] -> [a] -> a
hammingDistance x y = sqrt . sum . map (abs) $ zipWith (-) x y

euclideanDistance :: (Floating a) => [a] -> [a] -> a
euclideanDistance x y = sqrt . sum . map (^2) $ zipWith (-) x y