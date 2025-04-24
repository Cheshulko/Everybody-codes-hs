module Quest10.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import           Data.Char         (ord)
import           Data.List         (sortOn, transpose)
import           Data.List.Split   (splitOn)
import           Data.Maybe        (fromMaybe, isJust, mapMaybe)
import           Text.Printf       (printf)

questPrefix = "app/Quest10/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2_sample" = solvePart part2 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

type Input = A.Array (Int, Int) Cell

data Cell =
   Set     Char |
   Filled  Char |
   Paired  Char |
   Blocked      |
   Vacant       |
   Empty
   deriving     (Show, Eq)

getCell :: Char -> Cell
getCell '.' = Empty
getCell '*' = Blocked
getCell '?' = Vacant
getCell c   = Filled c

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _     = False

isSet :: Cell -> Bool
isSet (Set _) = True
isSet _       = False

isVacant :: Cell -> Bool
isVacant Vacant = True
isVacant _      = False

isPaired :: Cell -> Bool
isPaired (Paired _) = True
isPaired _          = False

parseGrid :: [String] -> Input
parseGrid input = A.listArray ((1, 1), (rows, cols)) (concat grid)
   where
      input'  = concat input
      content = lines input'
      rows    = length input
      cols    = length . head $ input
      grid    = map (map getCell) content

parseGrids :: String -> [Input]
parseGrids input = map parseGrid $ transpose $ map (splitOn " ") $ lines input

parseInput :: FilePath -> IO [Input]
parseInput filePath = concatMap parseGrids . splitOn "\n\n" <$> readFile filePath

take2Head :: [a] -> [a]
take2Head []          = []
take2Head [x]         = [x]
take2Head [x, y]      = [x, y]
take2Head (x : y : _) = [x, y]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunksOf'
   where
      chunksOf' [] = []
      chunksOf' ys = take n ys : chunksOf' (drop n ys)

getSetChar :: ((Int, Int), Cell) -> Maybe Char
getSetChar (_, Set ch) = Just ch
getSetChar _           = Nothing

calcPower :: [Char] -> Int
calcPower = sum . zipWith (\p c -> (1 + ord c - ord 'A') * p) [1 ..]

getAssoc :: Input -> (Int, Int) -> ((Int, Int), Cell)
getAssoc grid pos = (pos, grid A.! pos)

solveGrid :: Input -> Input
solveGrid input = solveGrid' input $ filter (isEmpty . snd) $ A.assocs input

solveGridComplex :: Input -> Int
solveGridComplex inputGrid = ans
   where
      allEmptyCells       = filter (isEmpty . snd) $ A.assocs inputGrid
      chunksOf4EmptyCells = chunksOf 4 allEmptyCells
      groupsCntInWidth    = (snd (snd $ A.bounds inputGrid) - 2) `div` 6
      groupsChunksInLine  = chunksOf groupsCntInWidth chunksOf4EmptyCells
      emptyGroups         = map concat $ concatMap transpose (chunksOf 4 groupsChunksInLine)

      solvedGrid = foldl solveGrid' inputGrid (concat $ replicate 2 emptyGroups)
      ans        = foldl (\acc group -> acc + calcAnsForGroup solvedGrid group) 0 emptyGroups

      calcAnsForGroup grid group = let
         setLengthInGroup = length $ filter (isSet . (grid A.!) . fst) group

         in if setLengthInGroup == length group
            then calcPower $ mapMaybe (getSetChar . getAssoc grid . fst) group
            else 0

solveGrid' :: Input -> [((Int, Int), Cell)] -> Input
solveGrid' gridInitial emptyGroup = A.listArray (A.bounds gridInitial) (A.elems resultGrid)
   where
      emptyGroup' = concat $ replicate 2 emptyGroup

      ansGrid1 = solveFold gridInitial False emptyGroup'
      ansGrid2 = solveFold ansGrid1 True $ filter (not . isSet . (ansGrid1 A.!) . fst) emptyGroup'

      setEmpties    = map (getAssoc ansGrid2 . fst) emptyGroup'
      filledVacants = map (\(pos, Paired ch) -> (pos, Filled ch)) $
         filter (isVacant . (gridInitial A.!) . fst) $
         filter (isPaired . snd) $
         A.assocs ansGrid2

      isSetAllEmpties = all (isSet . (ansGrid2 A.!) . fst) emptyGroup'
      resultGrid = if isSetAllEmpties
         then gridInitial A.// (filledVacants ++ setEmpties)
         else gridInitial

      solveFold grid useVacant = foldl solveCell grid
         where
            solveCell grid groupCell = grid A.// solve' grid useVacant groupCell

      solve' grid useVacant (pos@(i, j), _) = fromMaybe [] results
         where
            getFilled (pos, Filled c) = Just (pos, c)
            getFilled _               = Nothing

            getVacant (pos, Vacant) = Just (pos, '?')
            getVacant _             = Nothing

            getPaired (pos, Paired c) = Just (pos, c)
            getPaired _               = Nothing

            elems = A.assocs grid

            horizontalFilled  = mapMaybe getFilled $ filter (\((ie, _), _) -> ie == i) elems
            verticalFilled    = mapMaybe getFilled $ filter (\((_, je), _) -> je == j) elems

            horizontalPaired  = mapMaybe getPaired $ filter (\((ie, _), _) -> ie == i) elems
            verticalPaired    = mapMaybe getPaired $ filter (\((_, je), _) -> je == j) elems

            horizontalVacant  = mapMaybe getVacant $ filter (\((ie, _), _) -> ie == i) elems
            verticalVacant    = mapMaybe getVacant $ filter (\((_, je), _) -> je == j) elems

            horizontal        = horizontalFilled ++ horizontalPaired ++ horizontalVacant
            vertical          = verticalFilled   ++ verticalPaired   ++ verticalVacant

            cur = (pos, '#')
            horizontalNearest = filter (`notElem` horizontalPaired) $ sortOn (snd . fst) (take2Head (reverse left) ++ take2Head right)
               where
                  horizontalOrdered  = sortOn (snd . fst) (cur : horizontal)
                  horizontalOrdered' = break (== cur) horizontalOrdered
                  (left, _ : right)  = horizontalOrdered'

            verticalNearest = filter (`notElem` verticalPaired) $ sortOn (fst . fst) (take2Head (reverse left) ++ take2Head right)
               where
                  verticalOrdered   = sortOn (fst . fst) (cur : vertical)
                  verticalOrdered'  = break (== cur) verticalOrdered
                  (left, _ : right) = verticalOrdered'

            horizontalNearestFilled = filter ((/= '?') . snd) horizontalNearest
            verticalNearestFilled   = filter ((/= '?') . snd) verticalNearest

            results
               | isJust case1              = case1
               | useVacant && isJust case2 = case2
               | useVacant && isJust case3 = case3
               | otherwise                 = Nothing
               where
                  case1 = solveDirectional horizontalNearestFilled verticalNearestFilled
                  case2 = solveDirectional horizontalNearestFilled verticalNearest
                  case3 = solveDirectional verticalNearestFilled   horizontalNearest

                  solveDirectional takeFrom matchTo = case ans of
                     [ans] -> ans
                     _     -> Nothing

                     where
                        vacantCells   = filter ((== '?') . snd) matchTo
                        anyVacantCell = not $ null vacantCells
                        candidates    = [ch | ch <- map snd takeFrom, anyVacantCell || any ((== ch) . snd) matchTo]

                        ans = filter isJust $ map solveCandidate candidates

                        solveCandidate candidate = let
                              matchNoVacantCells = filter ((== candidate) . snd) matchTo
                              takeFromCells      = filter ((== candidate) . snd) takeFrom
                              matchToCells
                                 | not $ null matchNoVacantCells = matchNoVacantCells
                                 | otherwise                     = vacantCells

                              in case (takeFromCells, matchToCells) of
                                 ([cellFrom], [cellTo]) -> Just [
                                    (pos,          Set candidate),
                                    (fst cellFrom, Paired candidate),
                                    (fst cellTo,   Paired candidate)]
                                 _ -> Nothing

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: [Input] -> String
part1 = mapMaybe getSetChar . A.assocs . solveGrid . head

part2 :: [Input] -> Int
part2 = sum . map (calcPower . mapMaybe getSetChar . A.assocs . solveGrid)

part3 :: [Input] -> Int
part3 = solveGridComplex . head

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" "PTBVRCZHFLJWGMNS"
   testCase part1 "part1" "QPDHWJBLGKCMFTRV"

   -- Part 2
   testCase part2 "part2_sample" 1851
   testCase part2 "part2" 198903

   -- Part 3
   testCase part3 "part3_sample" 3889
   testCase part3 "part3" 213592
