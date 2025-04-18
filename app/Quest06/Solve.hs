module Quest06.Solve (solve) where

import           Control.Exception (assert)
import           Data.List.Split   (splitOn)
import qualified Data.Map          as M
import           Data.Maybe        (fromMaybe)
import           Text.Printf       (printf)

questPrefix = "app/Quest06/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2"        = solvePart part2 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Input = M.Map String [String]

parseLine :: String -> Input -> Input
parseLine line = M.insert from to
   where
      [from, remaining] = splitOn ":" line
      to = filter (`notElem` ["ANT", "BUG"]) $ splitOn "," remaining

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- lines <$> readFile filePath

   return $ foldr parseLine M.empty content

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

solve' :: Input -> [String]
solve' input = unique $ dfs 0 input [] "RR" M.empty
   where
      unique = head . snd . head . filter ((== 1) . length . snd) . M.assocs
      dfs curDep graph path cur deps
         | isLeaf    = M.alter (insert' (cur : path)) curDep deps
         | otherwise = foldr (dfs (curDep + 1) graph (cur : path)) deps children
            where
               children      = fromMaybe [] $ M.lookup cur graph
               isLeaf        = cur == "@"

      insert' path Nothing      = Just [path]
      insert' path (Just paths) = Just $ path : paths

part1 :: Input -> String
part1 = concat . reverse . solve'

part2 :: Input -> String
part2 = reverse . map head . solve'

part3 :: Input -> String
part3 = part2

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" "RRB@"
   testCase part1 "part1" "RRRCBHNHKXHL@"

   -- Part 2
   testCase part2 "part2" "RKHWSQLKQC@"

   -- Part 3
   testCase part3 "part3" "RRSQJGQPRVGZ@"
