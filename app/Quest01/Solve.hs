{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Quest01.Solve (solve) where

import           Control.Exception
import           Data.Maybe
import           Text.Printf

scoreEnemy :: Char -> Maybe Int
scoreEnemy 'A' = Just 0
scoreEnemy 'B' = Just 1
scoreEnemy 'C' = Just 3
scoreEnemy 'D' = Just 5
scoreEnemy _   = Nothing

solveSingleGroup :: FilePath -> IO Int
solveSingleGroup filePath = score <$> readFile filePath
    where
        score (first:remaining) = scoreGroup [scoreEnemy first] + score remaining
        score []                = 0

solveDoubleGroup :: FilePath -> IO Int
solveDoubleGroup filePath = score <$> readFile filePath
    where
        score (first:second:remaining) = scoreGroup [scoreEnemy first, scoreEnemy second] + score remaining
        score []                       = 0


solveTripleGroup :: FilePath -> IO Int
solveTripleGroup filePath = score <$> readFile filePath
    where
        score (first:second:third:remaining) = scoreGroup [scoreEnemy first, scoreEnemy second, scoreEnemy third] + score remaining
        score []                             = 0

scoreGroup :: [Maybe Int] -> Int
scoreGroup group = enemiesCount * max 0 (enemiesCount - 1) + sum (map (fromMaybe 0) group)
    where enemiesCount = length (filter isJust group)

questPrefix = "app/Quest01/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

solvePart :: (FilePath -> IO Int) -> String -> IO ()
solvePart solver part = printf "Part `%s`: %d\n" part =<< solver (printf questPrefix part)

part1 :: FilePath -> IO Int
part1 = solveSingleGroup

part2 :: FilePath -> IO Int
part2 = solveDoubleGroup

part3 :: FilePath -> IO Int
part3 = solveTripleGroup

testCase :: (FilePath -> IO Int) -> String -> Int -> IO ()
testCase solver part result = do
   x <- solver (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 5
   testCase part1 "part1" 1324

   -- Part 2
   testCase part2 "part2_sample" 28
   testCase part2 "part2" 5696

   -- Part 3
   testCase part3 "part3_sample" 30
   testCase part3 "part3" 27535
