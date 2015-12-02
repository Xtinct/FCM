{-# LANGUAGE DeriveDataTypeable #-}

module InputUtils
(
parseCSV,
dropBom,
dos2unix,
split,
applyInputOptions,
InputOptions(..)
) where

import Data.Data
import Data.Typeable
import Control.Exception
import System.IO.Error
import Fcm
import Data.List

split :: Char -> String -> [String]
split d s  =  case dropWhile(==d) s of
                      "" -> []
                      s' -> w : split d s''
                            where (w, s'') = break(==d) s'


dos2unix :: String -> String
dos2unix = filter(\n -> n /= '\r')

dropBom :: String -> String
dropBom ('\xfeff':s) = s
dropBom s = s

convert :: String -> Float
convert x = case reads x of
            [(xc, "")] -> xc
            _ -> error "Bad Input, can't convert to float"

parseCSV :: FilePath -> IO [[String]]
parseCSV fp = do
  contents <- readFile fp `catch` ioHandler
  return $ map(\x-> split ',' x) . lines $  dropBom $ dos2unix contents


ioHandler :: IOException -> IO String
ioHandler e
  | isDoesNotExistError e = error "File could not be found"
  | otherwise = error "can't access flle"

applyInputOptions :: [[String]] -> Bool -> Bool -> Bool -> [[Float]]
applyInputOptions list sH sC sN =
  case (sC, sN) of
    (True, False) -> m $ map init list'
    (False, True) -> m $ map tail list'
    (True, True) -> m $ map (tail . init) $ list'
    (False, False) -> m $ list'
  where
  m = map (map convert)
  list' = if sH then tail list else list


data InputOptions = InputOptions {
  delimiter :: String,
  stripHeader :: Bool,
  stripClass :: Bool,
  stripNumber :: Bool,
  clusterCount :: Int,
  fuzziness :: Int,
  threshold :: Float,
  distanceFunction :: Distance,
  input :: FilePath,
  output :: FilePath
} deriving (Show, Data, Typeable)
