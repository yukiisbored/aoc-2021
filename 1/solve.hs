module Main where

part1 :: [Int] -> Int
part1 xs = length $ filter id $ zipWith (<) xs $ drop 1 xs

part2 :: [Int] -> Int
part2 xs = part1 $ zipWith3 (\x y z -> x + y + z) xs (drop 1 xs) (drop 2 xs)

main :: IO ()
main = do
  content <- readFile "in.txt"

  let xs :: [Int]
      xs = map read $ lines content
      p1Answer = part1 xs
      p2Answer = part2 xs

  print (p1Answer, p2Answer)
