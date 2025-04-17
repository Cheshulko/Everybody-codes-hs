{-# LANGUAGE FlexibleContexts #-}
module Quest05.Solve (solve) where

import           Control.Exception
import           Data.Array
import           Data.List
import qualified Data.Map          as M
import           Data.Maybe
import           Text.Printf

questPrefix = "app/Quest05/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Input = Array Int [Int]

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- lines <$> readFile filePath

   let grid = transpose $ map (map read . words) content
   let rows = length grid

   return $ listArray (0, rows - 1) grid

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

playGame :: Array Int [Int] -> Int -> Array Int [Int]
playGame state row = state // [
      (row, tail $ state ! row),
      (nextRow, take num members ++ [claps] ++ drop num members)]
   where
      num
         | claps' <= membersLength = claps' - 1
         | otherwise = membersLength - (claps' - membersLength) + 1

      rows = length state
      nextRow = (row + 1) `mod` rows
      claps = head (state ! row)
      cycles = (claps - 1) `div` (2 * membersLength)
      claps' = claps - (cycles * 2 * membersLength)
      members = state ! nextRow
      membersLength = length members

getNumber :: (Show b) => Array i [b] -> Int
getNumber state = read $ concatMap (show . head) (elems state)

insertState :: Num a => Maybe a -> Maybe a
insertState Nothing    = Just 1
insertState (Just cnt) = Just $ cnt + 1

playGame' (state, round, m) = (nextState, round + 1, M.alter insertState number m)
   where
      rows = length state
      curRow = round `mod` rows
      nextRow = (curRow + 1) `mod` rows
      nextState = playGame state curRow
      number = getNumber nextState

part1 :: Input -> Int
part1 state = result $ until (\(_, round, _) -> round == 10) playGame' (state, 0, M.empty)
   where
      result (state, round, _) = getNumber state

part2 :: Input -> Int
part2 state = result $ until (\(state, _, m) -> fromMaybe 0 (M.lookup (getNumber state) m) >= 2024) playGame' (state, 0, M.empty)
   where
      result (state, round, _) = round * getNumber state

part3 :: Input -> Int
part3 state = result $ until (\(state, round, m) -> fromMaybe 0 (M.lookup state m) >= 1) playGame' (state, 0, M.empty)
   where
      result (state, round, m) = maximum $ map getNumber $ M.keys m

      playGame' (state, round, m) = (nextState, round + 1, M.alter insertState state m)
         where
            rows = length state
            curRow = round `mod` rows
            nextRow = (curRow + 1) `mod` rows
            nextState = playGame state curRow

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 2323
   testCase part1 "part1" 3225

   -- Part 2
   testCase part2 "part2_sample" 50877075
   testCase part2 "part2" 15801453187320

   -- Part 3
   testCase part3 "part3_sample" 6584
   testCase part3 "part3" 9594100210021000
