{-# LANGUAGE TupleSections #-}
module Quest15.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import           Data.List         (permutations)
import qualified Data.Map          as M
import qualified Data.Set          as S
import           Text.Printf       (printf)

questPrefix = "app/Quest15/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Point = (Int, Int)
type Grid  = A.Array Point Char

data Input = Input {
   getGrid  :: Grid,
   getStart :: Point,
   getHerbs :: M.Map Char [Point]
} deriving Show

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- lines <$> readFile filePath

   let rows' = length content
   let cols' = length . head $ content
   let grid  = A.listArray ((1, 1), (rows', cols')) (concat content)

   let herbs = findHerbs grid
   let start = head $ filter ((== '.') . (grid A.!)) [(1, j) | j <- [1 .. cols']]

   return Input {
      getGrid  = grid,
      getStart = start,
      getHerbs = herbs
   }

findHerbs :: Grid -> M.Map Char [Point]
findHerbs grid = foldr (
   \herb -> M.insert herb (map fst $ filter ((== herb) . snd) $ A.assocs grid)) M.empty herbTypes
   where
      herbTypes = S.fromList $ map snd $ filter ((`notElem` ['#', '~', '.']) . snd) $ A.assocs grid

getNeighbours :: Grid -> (Int, Int) -> [(Int, Int)]
getNeighbours grid (i, j) = filter isInBound neighbours
   where
      ((minI, minJ), (maxI, maxJ)) = A.bounds grid
      isInBound (i, j) = minI <= i && i <= maxI && minJ <= j && j <= maxJ
      neighbours       = [(i + di, j + dj) |
                          di <- [-1 .. 1],
                          dj <- [-1 .. 1],
                          abs di + abs dj == 1]

bfs :: Grid -> (Int, Int) -> M.Map (Int, Int) Int
bfs input start = bfs' [start] visitedInitial
   where
      visitedInitial  = M.fromList [(start, 0)]
      bfs' [] visited = visited
      bfs' (x : xs) visited = bfs' xs' visited'
         where
            neighbours = filter (`M.notMember` visited) $ getNeighbours input x
            dist       = visited M.! x
            accessible = filter ((`notElem` ['#', '~']) . (input A.!)) neighbours
            xs'        = xs ++ accessible
            visited'   = foldr (`M.insert` (dist + 1)) visited accessible

subGrid :: Grid -> (Int, Int) -> (Int, Int) -> Grid
subGrid grid (startRow, startCol) (endRow, endCol) = A.ixmap
   ((1, 1), (endRow - startRow + 1, endCol - startCol + 1))
   (\(i, j) -> (i + startRow - 1, j + startCol - 1))
   grid

solveGrid :: Grid -> Point -> M.Map Char [Point] -> Int
solveGrid grid start herbs = fst $ foldr solveOrderAndReturn (maxBound, orderDp) orders
   where
      herbTypes         = M.keys herbs
      interestingPoints = start : concatMap snd (M.assocs herbs)
      fromPointMem      = M.fromList $ map (
         \point -> (point, bfs grid point)) interestingPoints

      orders  = permutations herbTypes
      orderDp = M.empty

      solveOrderAndReturn order (best, orderDp) = (min best bestForOrder, orderDp')
         where
            bestDist     = M.fromList [(start, 0)]
            finalPoints  = herbs M.! last order
            bestForOrder = minimum $ map (
               \point -> (bestDist' M.! point) + (fromPointMem M.! start M.! point)) finalPoints

            (_, bestDist', orderDp') = solveOrder bestDist $ reverse order

      solveOrder bestDist = foldr solveOrder' ("", bestDist, orderDp)
         where
            solveOrder' nextHerb (curOrderPrefix, curDist, orderDp) = (nextOrderPrefix, nextDist, orderDpUpd)
               where
                  nextHerbPoints  = herbs M.! nextHerb
                  nextOrderPrefix = nextHerb : curOrderPrefix
                  initNextDist    = M.fromList $ map (, maxBound) nextHerbPoints

                  nextDist = if nextOrderPrefix `M.member` orderDp
                     then orderDp M.! nextOrderPrefix
                     else foldr updateBestDistFor initNextDist $ M.assocs curDist

                  orderDpUpd = M.insert nextOrderPrefix nextDist orderDp
                  updateBestDistFor (point, distToPoint) distBest = foldr (
                     \toPoint -> M.alter (instertState' toPoint) toPoint) distBest toPoints
                     where
                        toPoints              = M.keys distBest
                        distFromPointTo       = fromPointMem M.! point
                        instertState' toPoint = insertState ((+ distToPoint) $ distFromPointTo M.! toPoint)

      insertState value (Just value') = Just $ min value value'

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 input = (* 2) $ minimum $ map (dist M.!) $ snd $ head $ M.assocs herbs
   where
      grid  = getGrid input
      start = getStart input
      herbs = getHerbs input
      dist  = bfs grid start

part2 :: Input -> Int
part2 input = solveGrid grid start herbs
   where
      grid = getGrid input
      start = getStart input
      herbs = getHerbs input

-- the dummiest part I`ve faced so far
part3 :: Input -> Int
part3 input = ansLeft + 1 + ansMidle + 6 + ansRight + 1
   where
      gridInitial = getGrid input
      grid = gridInitial A.// [((76, 87), 'X')]

      ((minRow, minCol), (maxRow, maxCol)) = A.bounds grid
      width  = (maxCol - minCol + 1) `div` 3
      height = maxRow - minRow + 1

      gridLeft   = subGrid grid (minRow, minCol) (maxRow, width)
      herbsLeft  = findHerbs gridLeft
      ansLeft    = solveGrid gridLeft (height - 1, width) herbsLeft

      gridMidle  = subGrid grid (minRow, width + 1) (maxRow, 2 * width)
      herbsMidle = findHerbs gridMidle
      ansMidle   = solveGrid gridMidle (1, width `div` 2) herbsMidle

      gridRight  = subGrid grid (minRow, 2 * width + 1) (maxRow, 3 * width)
      herbsRight = findHerbs gridRight
      ansRight   = solveGrid gridRight (height - 1, 1) herbsRight

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 26
   testCase part1 "part1" 188

   -- Part 2
   testCase part2 "part2_sample" 38
   testCase part2 "part2" 530

   -- Part 3
   testCase part3 "part3" 1572
