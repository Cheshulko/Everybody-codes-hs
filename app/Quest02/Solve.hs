{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
module Quest02.Solve (solve) where

import           Control.Exception
import           Control.Monad     (when)
import qualified Data.Foldable     as S
import           Data.List.Split   (splitOn)
import qualified Data.Set          as S
import           Data.Tuple        (swap)
import           Text.Printf
import           Text.Regex.TDFA

questPrefix = "app/Quest02/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Input = ([String], [String])

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- readFile filePath

   let [runes, texts] = splitOn "\n\n" content
   let runes' = tail $ map head (runes =~ "[A-Z]+" :: [[String]])
   let texts' = lines texts

   return (runes', texts')

transpose :: [String] -> [String]
transpose []        = []
transpose ([] : xs) = transpose xs
transpose xs        = map head xs : transpose (map tail xs)

runesInText :: String -> String -> [(Int, String)]
runesInText text pattern  = findMatches text 0
   where
      findMatches "" _ = []
      findMatches text dropped =
         case (text =~ pattern :: (Int, Int)) of
            (-1, _)         -> []
            (position, len) -> let
               position' = dropped + position
               dropped'  = dropped + position + 1
               in (position', pattern) : findMatches (drop (position + 1) text) dropped'

runesInTexts :: [String] -> String -> [(Int, String)]
runesInTexts texts rune = concatMap (`runesInText` rune) texts

runesPositionInText :: Int -> String -> [String]  -> [Int]
runesPositionInText k text = concatMap (concatMap (indexed text) . runesInText (concat $ replicate k text))
   where
      indexed text (position, rune) = [(position + i) `mod` length text | i <- [0..length rune - 1]]

solveTexts :: (String -> [String] -> [Int]) -> [String] -> [String] -> [(Int, Int)]
solveTexts searcher runes texts = concatMap paired indexed
   where
      indexed = zip (map (`searcher` runes) texts) [0..]
      paired (positions, i) = zip (repeat i) positions

solvePart solver part = printf "Part `%s`: %d\n" part . uncurry solver =<< parseInput (printf questPrefix part)

part1 :: [String] -> [String] -> Int
part1 runes texts = sum $ map (length . runesInTexts texts) runes

part2 :: [String] -> [String] -> Int
part2 runes = S.length . S.fromList . solveTexts (runesPositionInText 1) runes'
   where
      runes' = runes ++ map reverse runes

part3 :: [String] -> [String] -> Int
part3 runes texts = S.length $ S.fromList horizontal `S.union` S.fromList vertical'
   where
      texts'     = transpose texts
      runes'     = runes ++ map reverse runes
      horizontal = solveTexts (runesPositionInText 2) runes' texts
      vertical   = solveTexts (runesPositionInText 1) runes' texts'
      vertical'  = map swap $ solveTexts (runesPositionInText 1) runes' texts'

testCase solver part result = do
   x <- uncurry solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 4
   testCase part1 "part1" 33

   -- Part 2
   testCase part2 "part2_sample" 42
   testCase part2 "part2" 5133

   -- Part 3
   testCase part3 "part3_sample" 10
   testCase part3 "part3" 11639
