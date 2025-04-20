module Quest08.Solve (solve) where

import           Control.Exception (assert)
import           Text.Printf       (printf)

questPrefix = "app/Quest08/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2"        = solvePart part2 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Input = Int

parseInput :: FilePath -> IO Input
parseInput filePath = read <$> readFile filePath

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 input = (acc - input) * width
   where
      (acc, width) = head $ dropWhile ((< input) . fst) $ scanl solve' (1, 0) [1..]
         where
            solve' (acc, _) level = (acc + nextWidth, nextWidth)
               where
                  nextWidth = 1 + 2 * level

solve' :: Int -> Int -> Int -> Int -> (Int, Int, Int, [Int])
solve' inputBlocks inputAcolytes highPriests withOptimization =
   head $
   dropWhile ((< inputBlocks) . getBlocks) $
   scanl solve' (1, 1, 0, [1]) [1..]

   where
      getBlocks (_, blocks, _, _)       = blocks
      solve' (thickness, _, _, l) level = (nextThickness, nextBlocks, nextWidth, nextLevelList)
         where
            nextWidth     = 1 + 2 * level
            nextThickness = thickness * highPriests `mod` inputAcolytes + withOptimization * inputAcolytes
            nextLevelList = map (+ nextThickness) ((0 : l) ++ [0])
            nextBlocks    = sum nextLevelList -
                            withOptimization * sum (map
                              (\height -> (highPriests * nextWidth * height) `mod` inputAcolytes) $
                                init $
                                tail nextLevelList)

part2 :: Input -> Int
part2 input = (blocks - inputBlocks) * width
   where
      inputBlocks           = 20240000
      inputAcolytes         = 1111
      (_, blocks, width, _) = solve' inputBlocks inputAcolytes input 0

part3 :: Input -> Int
part3 input = blocks - inputBlocks
   where
      inputBlocks       = 202400000
      inputAcolytes     = 10
      (_, blocks, _, _) = solve' inputBlocks inputAcolytes input 1

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 21
   testCase part1 "part1" 5368974

   -- Part 2
   testCase part2 "part2" 105113855

   -- Part 3
   testCase part3 "part3" 41082
