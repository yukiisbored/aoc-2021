module Main where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import qualified Text.ParserCombinators.ReadP as P

import Data.Bits
import Data.Bool (bool)
import Data.Bifunctor

fromListLE :: Bits a => [Bool] -> a
fromListLE = foldr (\ b a -> bool zeroBits (bit 0) b .|. shiftL a 1) zeroBits

fromListBE :: Bits a => [Bool] -> a
fromListBE = foldl' (\ a b -> shiftL a 1 .|. bool zeroBits (bit 0) b) zeroBits

part1 :: [[Bool]] -> Int
part1 xs = uncurry (*) $ bimap fromListLE fromListLE $ foldl' g ([], []) $ map f $ transpose xs
  where f xs = (length $ filter id xs, length $ filter not xs)
        g (gs, es) (t, f) = if t > f then (True : gs, False : es) else (False : gs, True : es)

part2 :: [[Bool]] -> Int
part2 xs = h' (>=) * h' (<)
  where f xs = (length $ filter id xs, length $ filter not xs)
        g p (xs, z) = let y = map (uncurry p . f) (transpose xs) !! z in (filter (\x -> x !! z == y) xs, z + 1)
        h xs p = fromListBE . head . fst $ until ((==) 1 . length . fst) (g p) (xs, 0)
        h' = h xs

pp = P.manyTill (t <|> f) (P.char '\n')
  where t = P.char '1' $> True
        f = P.char '0' $> False

main :: IO ()
main = do
  cnt <- readFile "in.txt"

  let (xs, _) = last $ P.readP_to_S (P.many pp) cnt

  print (part1 xs, part2 xs)
