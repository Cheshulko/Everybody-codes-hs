module Quest13.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import           Data.Char         (ord)
import qualified Data.Map          as M
import           Data.Maybe        (fromJust, fromMaybe)
import qualified Data.Set          as S
import           Text.Printf       (printf)

questPrefix = "app/Quest13/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

data Input = Input {
   getGrid    :: A.Array (Int, Int) Char,
   getBlocked :: [(Int, Int)],
   getStart   :: (Int, Int),
   getEnds    :: [(Int, Int)]
} deriving Show

platformHeight :: Input -> (Int, Int) -> Maybe Int
platformHeight input pos = case cell of
   '#' -> Nothing
   ' ' -> Nothing
   'S' -> Just 0
   'E' -> Just 0
   ch  -> Just (ord ch - ord '0')
   where
      grid' = getGrid input
      cell  = grid' A.! pos

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- lines <$> readFile filePath

   let rows' = length content
   let cols' = length . head $ content
   let grid  = A.listArray ((1, 1), (rows', cols')) (concat content)

   let blocked = map fst $ filter ((`elem` ['#', ' ']) . snd) $ A.assocs grid
   let start   = head $ map fst $ filter ((== 'E') . snd) $ A.assocs grid
   let ends    = map fst $ filter ((== 'S') . snd) $ A.assocs grid

   return Input {
      getGrid    = grid,
      getBlocked = blocked,
      getStart   = start,
      getEnds    = ends
   }

dijkstra :: Input -> Int
dijkstra input = minimum $ map (resultDist M.!) $ getEnds input
   where
      resultDist = dijkstra' input dist visited (M.fromList [((0, start), start)])

      start   = getStart input
      grid    = getGrid input
      blocked = getBlocked input
      visited = S.fromList blocked

      ((minRow, minCol), (maxRow, maxCol)) = A.bounds grid
      dist = M.insert start 0 $ M.fromList [((row, col), maxBound :: Int) |
                                            row <- [minRow .. maxRow],
                                            col <- [minCol .. maxCol]]

      dijkstra' input dist visited queue
        | M.null queue = dist
        | pos `S.member` visited = dijkstra' input dist visited queueRemaing
        | otherwise = dijkstra' input distUpdated visitedUpdated queueUpdated
        where
            (((curDist, pos@(i, j)), _), queueRemaing) = fromJust $ M.minViewWithKey queue

            curHeight         = fromJust $ platformHeight input pos
            distUpdated       = M.union (M.fromList accessibleRelaxed) dist
            queueUpdated      = M.union queueRemaing (M.fromList [((v, k), k) | (k, v) <- accessibleRelaxed])
            visitedUpdated    = S.insert pos visited
            accessibleRelaxed = map snd $ filter fst $ map f accessible
               where
                  f to = (toDist > toDistUpdated, (to, toDistUpdated))
                    where
                        toDist        = dist M.! to
                        toDistUpdated = 1 + curDist + min absHeight (10 - absHeight)
                        absHeight     = abs $ toHeight - curHeight
                        toHeight      = fromJust $ platformHeight input to

            isInBound (i, j) = minRow <= i && i <= maxRow && minCol <= j && j <= maxCol
            accessible       = filter (`notElem` blocked) $ filter isInBound neighbours
            neighbours       = [(i + di, j + dj) |
                                di <- [-1 .. 1],
                                dj <- [-1 .. 1],
                                abs di + abs dj == 1]

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 = dijkstra

part2 :: Input -> Int
part2 = part1

part3 :: Input -> Int
part3 = part1

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 28
   testCase part1 "part1" 145

   -- Part 2
   testCase part2 "part2" 602

   -- Part 3
   testCase part3 "part3_sample" 14
   testCase part3 "part3" 560
