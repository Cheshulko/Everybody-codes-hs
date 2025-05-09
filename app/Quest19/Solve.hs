module Quest19.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import           Data.List.Split   (splitOn)
import           Text.Printf       (printf)

questPrefix = "app/Quest19/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Grid a          = A.Array (Int, Int) a
type PermutationGrid = Grid (Int, Int)

data Input a = Input {
   getDirections :: String,
   getGrid       :: Grid a
} deriving Show

parseInput :: FilePath -> IO (Input Char)
parseInput filePath = do
   [directions, remaining] <- splitOn "\n\n" <$> readFile filePath

   let grid   = lines remaining
   let rows   = length grid
   let cols   = length . head $ grid
   let grid'  = A.listArray ((1, 1), (rows, cols)) (concat grid)

   return Input {
      getDirections = directions,
      getGrid       = grid'
   }

buildAnswer :: Grid Char -> String
buildAnswer grid = init $ tail [grid A.! (fst start, j) | j <- [snd start .. snd end]]
   where
      start = fst $ head $ filter ((== '>') . snd) $ A.assocs grid
      end   = fst $ head $ filter ((== '<') . snd) $ A.assocs grid

rotateInput :: Input e -> Grid e
rotateInput input = foldl rotateNeighbours grid $ zip cells directions'
   where
      grid       = getGrid input
      directions = getDirections input

      directionsCnt = length directions

      order = [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]

      ((minRow, minCol), (maxRow, maxCol))     = A.bounds grid
      ((minRow', minCol'), (maxRow', maxCol')) = ((minRow + 1, minCol + 1), (maxRow - 1, maxCol - 1))

      cells    = [(i, j) | i <- [minRow' .. maxRow'], j <- [minCol' .. maxCol']]
      cellsCnt = length cells
      directionsReplicateCnt = 1 + cellsCnt `div` directionsCnt
      directions' = take cellsCnt $ concat $ replicate directionsReplicateCnt directions

      rotateNeighbours grid ((i, j), cell) = grid A.// zip neighbours values'
         where
            neighbours = map (\(di, dj) -> (i + di, j + dj)) order
            values     = map (grid A.!) neighbours

            values' = let
               tail' = tail values
               head' = head values
               last' = last values
               init' = init values
               in case cell of
                  'L' -> tail' ++ [head']
                  'R' -> last' : init'

solvePermutation :: PermutationGrid  -> PermutationGrid -> Int -> PermutationGrid
solvePermutation sequence permutation iterations =
   fst $ fst $ until ((== 0) . snd) permute ((sequence, permutation), iterations)

   where
      applyPermutation :: PermutationGrid  -> PermutationGrid  -> PermutationGrid
      applyPermutation sequence permutation =
         sequence A.// map (\(ind, _) -> (ind, sequence A.! (permutation A.! ind))) (A.assocs sequence)

      permute ((sequence, permutation), iterations)
         | iterations `mod` 2 == 1 =
            (
               (
                  applyPermutation sequence    permutation,
                  applyPermutation permutation permutation
               ),
               iterations `div` 2
            )
         | otherwise               =
            (
               (
                  sequence,
                  applyPermutation permutation permutation
               ),
               iterations `div` 2
            )

solveInput :: Input Char -> Int -> String
solveInput input iterations = answer
   where
      grid   = getGrid input
      bounds = A.bounds grid

      permutationGrid = A.listArray bounds $ map fst $ A.assocs grid
      input'          = input { getGrid = permutationGrid }
      permutation     = getGrid $ input { getGrid = rotateInput input' }

      answerGrid = solvePermutation permutationGrid permutation iterations
      answer     = buildAnswer $ A.listArray bounds $ map (grid A.!) $ A.elems answerGrid

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input Char -> String
part1 input = solveInput input 1

part2 :: Input Char -> String
part2 input = solveInput input 100

part3 :: Input Char -> String
part3 input = solveInput input 1048576000

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" "WIN"
   testCase part1 "part1" "4675213295755686"

   -- Part 2
   testCase part2 "part2_sample" "VICTORY"
   testCase part2 "part2" "3723637389439581"

   -- Part 3
   testCase part3 "part3" "3146724581947653"
