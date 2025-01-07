import Text.Parsec
import Text.Parsec.String (Parser)

data Operation = Mul | Do | Dont deriving (Eq)

main :: IO ()
main = do
  input <- readFile "../inputs/2024/day03.in"
  let ops = getOperations input
  print $ part1 ops
  print $ part2 ops

part1, part2 :: [(Operation, Int, Int)] -> Int
part1 = evalMuls False
part2 = evalMuls True

evalMuls :: Bool -> [(Operation, Int, Int)] -> Int
evalMuls False ops = sum (uncurry (*) <$> [(a, b) | (_, a, b) <- ops])
evalMuls True ops = loop True 0 ops
  where
    loop _ res [] = res
    loop active acc ((op, a, b) : ops)
      | op == Do = loop True acc ops
      | op == Dont = loop False acc ops
      | active = loop active (acc + (a * b)) ops
      | otherwise = loop active acc ops

getOperations input = case parse findOperations "" input of
  Left _ -> []
  Right r -> r

mulParser :: Parser (Operation, Int, Int)
mulParser = do
  _ <- string "mul("
  x <- many1 digit
  _ <- char ','
  y <- many1 digit
  _ <- char ')'
  return (Mul, read x, read y)

doParser :: Parser (Operation, Int, Int)
doParser = do
  _ <- string "do()"
  return (Do, 0, 0)

dontParser :: Parser (Operation, Int, Int)
dontParser = do
  _ <- string "don't()"
  return (Dont, 0, 0)

findOperations = many loop
  where
    loop = try mulParser <|> try doParser <|> try dontParser <|> try (anyChar >> loop)