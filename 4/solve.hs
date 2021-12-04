module Main where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import qualified Text.ParserCombinators.ReadP as P

type Board = [[Int]]
type Puzzle = ([Int], [Board])

part1 :: Puzzle -> Int
part1 (xs, bs) = n * b'
  where f xs y = let f = any (all (`elem` xs)) in  f y || f (transpose y)
        f' (xs, bs) x = let g = f xs in if any g bs then (xs, filter g bs) else (x:xs, bs)
        (ns@(n:_), [b]) = foldl f' ([], bs) xs
        b' = sum $ filter (not . (`elem` ns)) $ concat b

part2 :: a -> Int
part2 xs = undefined

pp :: P.ReadP Puzzle
pp = do
  nums <- xs <* P.string "\n"
  boards <- P.many b
  return (nums, boards)
  where xs = P.sepBy (P.readS_to_P reads) (P.string ",") <* P.string "\n" :: P.ReadP [Int]
        bl = P.sepBy1 (P.readS_to_P reads) (P.string " ")  <* P.string "\n" :: P.ReadP [Int]
        b = P.manyTill bl (P.string "\n")

main :: IO ()
main = do
  cnt <- readFile "in.txt"

  let cnt' = cnt ++ "\n" -- Ugly hack, whatever.
      xs = head $ fst $ last $ P.readP_to_S (P.many pp) cnt'

  print $ part1 xs
