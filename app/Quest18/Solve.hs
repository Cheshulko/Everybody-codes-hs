{-# LANGUAGE TupleSections #-}
module Quest18.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import qualified Data.Map          as M
import           Text.Printf       (printf)

questPrefix = "app/Quest18/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Grid = A.Array (Int, Int) Char

data Input = Input {
   getGrid   :: Grid,
   getStarts :: [(Int, Int)],
   getPalms  :: [(Int, Int)]
} deriving Show

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- lines <$> readFile filePath

   let rows  = length content
   let cols  = length . head $ content
   let grid  = A.listArray ((1, 1), (rows, cols)) (concat content)

   let edges =
         [(1, j)    | j <- [1 .. cols]] ++
         [(rows, j) | j <- [1 .. cols]] ++
         [(i, 1)    | i <- [1 .. rows]] ++
         [(i, cols) | i <- [1 .. rows]]

   let starts = filter ((== '.') . (grid A.!)) edges
   let palms  = map fst $ filter ((== 'P') . snd) $ A.assocs grid

   return $ Input grid starts palms

getNeighbours :: Grid -> (Int, Int) -> [(Int, Int)]
getNeighbours grid (i, j) = filter isInBound neighbours
   where
      ((minI, minJ), (maxI, maxJ)) = A.bounds grid
      isInBound (i, j) = minI <= i && i <= maxI && minJ <= j && j <= maxJ
      neighbours       = [(i + di, j + dj) |
                          di <- [-1 .. 1],
                          dj <- [-1 .. 1],
                          abs di + abs dj == 1]

bfs :: Grid -> [(Int, Int)] -> M.Map (Int, Int) Int
bfs input starts = bfs' starts visitedInitial
   where
      visitedInitial  = M.fromList $ map (, 0) starts
      bfs' [] visited = visited
      bfs' (x : xs) visited = bfs' xs' visited'
         where
            neighbours = filter (`M.notMember` visited) $ getNeighbours input x
            dist       = visited M.! x
            accessible = filter ((/= '#') . (input A.!)) neighbours
            xs'        = xs ++ accessible
            visited'   = foldr (`M.insert` (dist + 1)) visited accessible

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 input = maximum $ map (dist M.!) $ getPalms input
   where
      dist = bfs (getGrid input) $ getStarts input

part2 :: Input -> Int
part2 = part1

part3 :: Input -> Int
part3 input = minimum $ map (\ground -> sum $ map (M.! ground) dists) grounds
   where
      grounds = map fst $ filter ((== '.') . snd) $ A.assocs $ getGrid input
      dists   = map (\palm -> bfs (getGrid input) [palm]) $ getPalms input

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 11
   testCase part1 "part1" 101

   -- Part 2
   testCase part2 "part2_sample" 21
   testCase part2 "part2" 1232

   -- Part 3
   testCase part3 "part3_sample" 12
   testCase part3 "part3" 268142
