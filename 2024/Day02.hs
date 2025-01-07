main :: IO ()
main = do
  input <- readFile "../inputs/2024/day02.in"
  let nums = [read <$> words line | line <- lines input]
  print $ part1 nums
  print $ part2 nums

part1, part2 :: [[Int]] -> Int
part1 input = length $ filter isSafe input
part2 input = length $ filter id (isSafeDampen <$> input)

isSafe, isSafeDampen :: [Int] -> Bool
isSafe = check
isSafeDampen nums = check nums || or ([check (removeAt i nums) | i <- [0 .. length nums - 1]])
  where
    removeAt i xs = take i xs ++ drop (i + 1) xs

check nums = abs (sum (score <$> pairs)) == safeScore
  where
    pairs = overlappingPairs nums
    safeScore = length pairs
    overlappingPairs xs = zip xs (tail xs)
    score (prev, this) = if abs (prev - this) > 3 then 0 else signum (prev - this)
