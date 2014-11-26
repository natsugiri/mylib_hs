{-# LANGUAGE BangPatterns #-}

import Control.Applicative ((<$>))
import Control.Monad
import Data.List -- (sort, foldl', foldl1', nub)
import Data.Function (on)
import Data.Char

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Sequence as Seq
import Data.Sequence (viewl, viewr, ViewL((:<)), ViewR((:>)))

import Data.Graph

getInt :: IO Int
getInt = read <$> getLine

getInts :: IO [Int]
getInts = map read .words <$> getLine

main = do
  l <- getInts
  print l
