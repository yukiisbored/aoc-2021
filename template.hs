module Main where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import qualified Text.ParserCombinators.ReadP as P

part1 :: a -> Int
part1 xs = undefined

part2 :: a -> Int
part2 xs = undefined

pp = undefined

main :: IO ()
main = do
  cnt <- readFile "in.txt"

  let (xs, _) = last $ P.readP_to_S (P.many pp) cnt

  print (part1 xs, part2 xs)
