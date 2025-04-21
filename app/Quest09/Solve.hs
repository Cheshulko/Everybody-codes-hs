{-# LANGUAGE TupleSections #-}
module Quest09.Solve (solve) where

import           Control.Applicative (liftA2)
import           Control.Exception   (assert)
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Tuple          as T
import           Text.Printf         (printf)

questPrefix = "app/Quest09/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Input = [Int]

parseInput :: FilePath -> IO Input
parseInput filePath = map read . lines <$> readFile filePath

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

solve' :: [Int] -> Input -> M.Map Int Int
solve' availableStamps input = foldl brightness' dp [1 .. maximum input]
   where
      dp                        = M.fromList ((0, 0) : map (, 1) availableStamps)
      brightness' dp brightness = M.insert brightness stamps dp
         where
            current  = fromMaybe maxBound $ M.lookup brightness dp
            possible = map (+ 1) $ catMaybes $ [M.lookup (brightness - stamp) dp | stamp <- availableStamps]
            stamps   = minimum (current : possible)

part1 :: Input -> Int
part1 input = sum $ map (dp M.!) input
   where
      dp = solve' [1, 3, 5, 10] input

part2 :: Input -> Int
part2 input = sum $ map (dp M.!) input
   where
      dp = solve' [1, 3, 5, 10, 15, 16, 20, 24, 25, 30] input

part3 :: Input -> Int
part3 input = sum $ map (minimum . splitted dp) input
   where
      dp = solve' [1, 3, 5, 10, 15, 16, 20, 24, 25, 30, 37, 38, 49, 50, 74, 75, 100, 101] input

      splitted dp brightness = mapMaybe (fmap (T.uncurry (+)) . uncurry (liftA2 (,))) possibleSplitting
         where
            possibleSplitting = [(M.lookup brightness1 dp, M.lookup brightness2 dp) |
               brightness1 <- [0 .. brightness],
               let brightness2 = brightness - brightness1,
               brightness1 <= brightness2,
               abs (brightness1 - brightness2) <= 100]

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 10
   testCase part1 "part1" 13771

   -- Part 2
   testCase part2 "part2_sample" 10
   testCase part2 "part2" 5203

   -- Part 3
   testCase part3 "part3_sample" 10449
   testCase part3 "part3" 153488
