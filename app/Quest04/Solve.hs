module Quest04.Solve (solve) where

import           Control.Exception
import           Text.Printf

questPrefix = "app/Quest04/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Input = [Int]

parseInput :: FilePath -> IO Input
parseInput filePath = map read . lines <$> readFile filePath

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 input = sum input - minimum input * length input

part2 :: Input -> Int
part2 = part1

part3 :: Input -> Int
part3 input = minimum $ map calc $ pairToList $ until (\(l, r) -> r - l < 3) f (mi, ma)
    where
        calc target = sum $ map (\x -> abs $ x - target) input
        mi = minimum input
        ma = maximum input
        pairToList (a, b) = [a..b]

        f (l, r)
            | m1v > m2v = (m1, r)
            | otherwise = (l, m2)
            where
                m1 = (l + l + r) `div` 3
                m2 = (l + r + r) `div` 3
                m1v = calc m1
                m2v = calc m2

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 10
   testCase part1 "part1" 83

   -- Part 2
   testCase part2 "part2" 906502

   -- Part 3
   testCase part3 "part3_sample" 8
   testCase part3 "part3" 127872359
