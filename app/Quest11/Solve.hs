{-# LANGUAGE TupleSections #-}
module Quest11.Solve (solve) where

import           Control.Exception (assert)
import           Data.List.Split   (splitOn)
import qualified Data.Map          as M
import           Data.Maybe        (fromJust, fromMaybe)
import           Text.Printf       (printf)

questPrefix = "app/Quest11/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Input = M.Map String [String]

parseLine :: String -> Input -> Input
parseLine line = M.insert from to
   where
      [from, remaining] = splitOn ":" line
      to = splitOn "," remaining

parseInput :: FilePath -> IO Input
parseInput filePath = foldr parseLine M.empty . lines <$> readFile filePath

insertState :: Num a => a -> Maybe a -> Maybe a
insertState count Nothing       = Just count
insertState count (Just count') = Just $ count' + count

solve' :: Int -> String -> Input -> Int
solve' targetDepth start input = sum $ M.elems $ bfs $ M.fromList [((1, start), 1)]
   where
      bfs heap
         | depth > targetDepth = heap
         | otherwise           = bfs restHeap'
         where
            (((depth, node), count), restHeap) = fromJust $ M.minViewWithKey heap

            neighbours = M.findWithDefault [] node input
            tos        = map (1 + depth,) neighbours
            restHeap'  = foldr (M.alter (insertState count)) restHeap tos

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 = solve' 4 "A"

part2 :: Input -> Int
part2 = solve' 10 "Z"

part3 :: Input -> Int
part3 input = maximum result - minimum result
   where
      result = map (flip (solve' 20) input) $ M.keys input

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 8
   testCase part1 "part1" 48

   -- Part 2
   testCase part2 "part2" 303643

   -- Part 3
   testCase part3 "part3_sample" 268815
   testCase part3 "part3" 739715586270
