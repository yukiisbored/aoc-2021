{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

p1 :: String -> (Int, Int)
p1 xs =
  case x of
    "forward" -> (y', 0)
    "up" -> (0, -y')
    "down" -> (0, y')

  where [x, y] = words xs
        y' = read y :: Int

solve1 :: [String] -> Int
solve1 xs = uncurry (*) $ foldl (\(x, y) (a, b) -> (x+a, y+b)) (0,0) $ map p1 xs

do2 :: (Int, Int, Int) -> String -> (Int, Int, Int)
do2 (a, x, y) cmd =
  case cmd of
    "forward" -> (a, x + v, y + (a * v))
    "down" -> (a + v, x, y)
    "up" -> (a - v, x, y)

  where [cmd, v'] = words cmd
        v = read v' :: Int

solve2 :: [String] -> Int
solve2 xs = (\(_, x, y) -> x * y) $ foldl do2 (0,0,0) xs

main :: IO ()
main = do
  cnt <- readFile "in.txt"

  let xs = lines cnt
      p1Solution = solve1 xs
      p2Solution = solve2 xs

  print (p1Solution, p2Solution)
