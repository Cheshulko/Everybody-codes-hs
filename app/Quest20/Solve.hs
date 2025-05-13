module Quest20.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import           Data.Function     (on)
import           Data.List         (find, group, groupBy, sortBy)
import qualified Data.Map          as M
import           Data.Maybe        (fromJust, isJust, mapMaybe)
import           Text.Printf       (printf)

questPrefix = "app/Quest20/input/%s"

solve :: String -> IO ()
solve part@"part1_sample"   = solvePart part1 part
solve part@"part1"          = solvePart part1 part
solve part@"part2_sample_1" = solvePart part2 part
solve part@"part2_sample_2" = solvePart part2 part
solve part@"part2_sample_3" = solvePart part2 part
solve part@"part2"          = solvePart part2 part
solve part@"part3_sample"   = solvePart part3 part
solve part@"part3"          = solvePart part3 part
solve "test"                = test

type Cell      = (Int, Int)
type Direction = (Int, Int)
type Grid      = A.Array Cell Char

data Input = Input {
   getGrid  :: Grid,
   getStart :: Cell
} deriving Show

data State = State {
   getCell      :: Cell,
   getDirection :: Direction,
   getValue     :: Int,
   getLevel     :: Int
} deriving (Show, Eq, Ord)

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- lines <$> readFile filePath

   let rows   = length content
   let cols   = length . head $ content
   let grid'  = A.listArray ((1, 1), (rows, cols)) (concat content)

   let start = fst $ head $ filter ((== 'S') . snd) $ A.assocs grid'

   return Input {
      getStart = start,
      getGrid  = grid'
   }

down :: (Int, Int)
down = (1, 0)

up :: (Int, Int)
up = (-1, 0)

inf :: Int
inf = 10^10

getNeighbours :: Grid -> Int -> Cell -> [(Cell, Direction)]
getNeighbours grid level (i, j) = accessible
   where
      ((minI, minJ), (maxI, maxJ)) = A.bounds grid
      checkBounds ((i, j), direction)
         | minI <= i && i <= maxI && minJ <= j && j <= maxJ = Just ((i, j), direction)
         | otherwise = Nothing
      neighbours  = [((i + di, j + dj),(di, dj)) |
                     di <- [-1 .. 1],
                     dj <- [-1 .. 1],
                     abs di + abs dj == 1]
      accessible = filter (filterLevel . (grid A.!) . fst) $ filter ((/= '#') . (grid A.!) . fst) $ mapMaybe checkBounds neighbours
         where
            filterLevel 'A' = level == 0
            filterLevel 'B' = level == 1
            filterLevel 'C' = level == 2
            filterLevel _   = True

getRotations :: Direction -> [Direction]
getRotations (di, dj) = [(di, dj), (dj, di), (-dj, -di)]

getDeltaAltitude :: Char -> Int
getDeltaAltitude '+' = 1
getDeltaAltitude '-' = -2
getDeltaAltitude _   = -1

updateLevel :: A.Array (Int, Int) Char -> Int -> (Int, Int) -> Int
updateLevel grid level to = case grid A.! to of
   'A' -> 1
   'B' -> 2
   'C' -> 3
   _   -> level

calcAltitude :: Int -> A.Array Cell Char -> State -> Int
calcAltitude altitude grid to = altitude + getDeltaAltitude (grid A.! getCell to)

insertState :: Ord a => a -> Maybe a -> Maybe a
insertState altitude Nothing          = Just altitude
insertState altitude (Just altitude') = Just $ max altitude' altitude

updateDist :: Num a => a -> (Int, Int) -> a
updateDist dist direction
   | direction == down = dist + 1
   | otherwise         = dist

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 input = maximum $ M.elems $ fst $ solve 100 $ M.fromList starts
   where
      (si, sj) = getStart input
      grid     = getGrid input
      dirs     = [(di, dj) | di <- [-1 .. 1], dj <- [-1 .. 1], abs di + abs dj == 1]
      starts   = map (\(di, dj) -> (State (si, sj) (di, dj) 0 0, 1000)) dirs

      ((minI, minJ), (maxI, maxJ)) = A.bounds grid
      cells = [(i, j) | i <- [minI .. maxI], j <- [minJ .. maxJ]]

      solve iterations dp  = until ((== 0) . snd) f (dp, iterations)
         where
            f :: (M.Map State Int, Int) -> (M.Map State Int, Int)
            f (dp, iterations) = (foldl makeStep M.empty $ M.assocs dp, iterations - 1)
               where
                  makeStep acc (State cell direction value level, altitude) =
                     foldr (\to b -> M.alter (insertState (calcAltitude altitude grid to)) to b) acc validTos
                     where
                        validRotations = getRotations direction
                        tos = filter (flip elem validRotations . getDirection) $ map (\(to, direction) -> State to direction value level) $ getNeighbours grid level cell
                        validTos = filter ((>= 0) . calcAltitude altitude grid ) tos

part2 :: Input -> Int
part2 input = getValue $ fromJust $ snd $ solve dp
   where
      grid  = getGrid input
      start@(si, sj) = getStart input
      ((minI, minJ), (maxI, maxJ)) = A.bounds grid

      cells = [(i, j) | i <- [minI .. maxI], j <- [minJ .. maxJ]]

      dirs = [(di, dj) | di <- [-1 .. 1], dj <- [-1 .. 1], abs di + abs dj == 1]
      dp = M.fromList $ map (\(di, dj) -> (State (si, sj) (di, dj) 0 0, 10000)) dirs

      solve dp = until (isJust . snd) f (dp, Nothing)
         where
            f :: (M.Map State Int, Maybe State) -> (M.Map State Int, Maybe State)
            f (dp, iterations) = (dp', result)
               where
                  result = fmap fst $ find (\(k, v) -> getCell k == start && v >= 10000 && getLevel k == 3) $ M.assocs dp'

                  dp' = foldl makeStep M.empty (M.assocs dp)
                     where
                        makeStep acc (State cell direction value level, altitude) =
                           foldr (\to b -> M.alter (insertState (calcAltitude altitude grid to)) to b) acc tos
                           where
                              validRotations = getRotations direction
                              tos = filter (flip elem validRotations . getDirection) $
                                 map (\(to, direction) -> State to direction (value + 1) (updateLevel grid level to)) $
                                 getNeighbours grid level cell

part3 :: Input -> Int
part3 input = maximum $
   map ((blocksHeight +) . (\(j, att) -> solveRemaining (State (minI, j) down 0 0, att))) $
   filter ((>= 0) . snd) $
   A.assocs $ fst $ fst blocks

   where
      start@(si, sj) = getStart input

      grid = getGrid input
      ((minI, minJ), (maxI, maxJ)) = A.bounds grid
      width = maxJ - minJ + 1;
      height = maxI - minI + 1;

      next    = A.listArray (minJ, maxJ) (replicate width (-1)) A.// [(sj, 384400)]
      current = A.listArray (minJ, maxJ) (replicate width 1337)

      blocksHeight = snd (fst blocks) * height
      blocks = until (all (<= 0) . fst . snd) f ((current, -1), (next, 0))
         where
            f ((cur, cnt1), (next, cnt2)) =
               (
                  (next, cnt1 + 1),
                  (
                     A.listArray (minJ, maxJ) $
                        map (\x -> maximum $
                           map (\(j, delta) -> (next A.! j) + delta) $
                           map (\(j, col) -> (j, col A.! x)) $
                           A.assocs deltas)
                        [minJ .. maxJ],
                     cnt2 + 1
                  )
               )

      deltas = A.listArray (minJ, maxJ)                                                                 $
         map (foldl (A.//) $ A.listArray (minJ, maxJ) $ replicate width (-inf))                         $
         map (map (\alt -> [(snd $ getCell $ fst $ head alt, maximum (0 : map snd alt) - 1 - inf)]))    $
         map (groupBy ((==) `on` (snd . getCell . fst)))                                                $
         map (sortBy (compare `on` (snd . getCell . fst)))                                              $
         map (filter ((== maxI) . fst . getCell . fst))                                                 $
         map (M.assocs . solve)                                                                         $
         map (\(si, sj) -> M.fromList [(State (si, sj) down 0 0, inf)]) [(minI, j) | j <- [minJ .. maxJ]]

      solveRemaining start = maximum (-inf: map getValue (M.keys $ solve $ M.fromList [start]))

      solve dp = snd $ until (null . M.keys . fst) f (dp, M.empty)
         where
            f :: (M.Map State Int, M.Map State Int) -> (M.Map State Int, M.Map State Int)
            f (dp, bestDp) = (dp', bestDp')
               where
                  bestDp' = foldr (\(k, att) -> M.alter (insertState att) k) bestDp $ M.assocs dp'

                  dp' = foldl makeStep M.empty (M.assocs dp)
                     where
                        makeStep acc (State cell direction value level, altitude)
                           | altitude > 0 = foldr makeStep' acc tos
                           | otherwise    = M.empty
                           where
                              validRotations = filter (/= up) $ getRotations direction
                              tos =
                                 filter (flip elem validRotations . getDirection) $
                                 map (\(to, direction) -> State to direction (updateDist value direction) level) $
                                 getNeighbours grid level cell

                              makeStep' to b = case M.lookup to bestDp of
                                 Nothing  -> M.alter (insertState (calcAltitude altitude grid to)) to b
                                 Just alt -> checkBest alt to b
                                    where
                                       checkBest att to dp
                                          | att >= alt' = dp
                                          | otherwise = M.alter (insertState alt') to dp

                                          where
                                             alt' = calcAltitude altitude grid to

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 1045
   testCase part1 "part1" 1031

   -- Part 2
   testCase part2 "part2_sample_1" 24
   testCase part2 "part2_sample_2" 78
   testCase part2 "part2_sample_3" 206
   testCase part2 "part2" 540

   -- Part 3
   testCase part3 "part3_sample" 768790
   testCase part3 "part3" 768791
