import Data.List (sort)

main :: IO ()
main = do
  input <- readFile "../inputs/2024/day01.in"
  let pairs = readPair <$> lines input
  print $ part1 pairs
  print $ part2 pairs

part1, part2 :: [(Int, Int)] -> Int
part1 pairs = sum $ abs <$> zipWith (-) (sort lefts) (sort rights)
  where
    (lefts, rights) = unzip pairs
part2 pairs = sum $ score <$> lefts
  where
    (lefts, rights) = unzip pairs
    score n = n * length (filter (== n) rights)

readPair :: String -> (Int, Int)
readPair s = (read a, read b)
  where
    (a : b : _) = words s
