module Quest17.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Array        as A
import qualified Data.Foldable     as S
import           Data.Function     (on)
import qualified Data.IntMap       as IM
import           Data.List         (groupBy, sort, sortBy, sortOn)
import           Data.Maybe        (fromMaybe)
import qualified Data.Set          as S
import           Text.Printf       (printf)

questPrefix = "app/Quest17/input/%s"

solve :: String -> IO ()
solve part@"part1_sample" = solvePart part1 part
solve part@"part1"        = solvePart part1 part
solve part@"part2"        = solvePart part2 part
solve part@"part3_sample" = solvePart part3 part
solve part@"part3"        = solvePart part3 part
solve "test"              = test

data Input = Input {
   getGrid  :: A.Array (Int, Int) Char,
   getStars :: [(Int, Int)]
} deriving Show

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- lines <$> readFile filePath

   let rows  = length content
   let cols  = length . head $ content
   let grid  = A.listArray ((1, 1), (rows, cols)) (concat content)
   let stars = map fst $ filter ((== '*').  snd) $ A.assocs grid

   return $ Input grid stars

-- DSU
data DSU = DSU {
   sizes   :: IM.IntMap Int,
   parents :: IM.IntMap Int
} deriving Show

dsuEmpty :: DSU
dsuEmpty = DSU {
   sizes   = IM.empty,
   parents = IM.empty
}

dsuJoin :: DSU -> Int -> Int -> DSU
dsuJoin dsu x y
   | dsuSame dsu x y = dsu
   | sx < sy   = dsuJoin dsu py px
   | otherwise = DSU {
      sizes    = IM.insert px (sx + sy) $ sizes dsu,
      parents  = IM.insert py px $ parents dsu
   }
   where
      (px, py) = (dsuParent dsu x, dsuParent dsu y)
      (sx, sy) = (dsuSize dsu x, dsuSize dsu y)

dsuSize :: DSU -> Int -> Int
dsuSize dsu x = fromMaybe 1 $ IM.lookup x (sizes dsu)

dsuParent :: DSU -> Int -> Int
dsuParent dsu x = case IM.lookup x $ parents dsu of
   Just p  -> dsuParent dsu p
   Nothing -> x

dsuSame :: DSU -> Int -> Int -> Bool
dsuSame dsu x y = dsuParent dsu x == dsuParent dsu y

-- Graph
type Edge a  = (a, a, Int)
type Graph a = A.Array Int [Edge a]

listEdges :: Graph a -> [Edge a]
listEdges = concat . A.elems

edgeWeight :: Edge a -> Int
edgeWeight (_, _, w) = w

graphOfEdges :: Int -> [Edge Int] -> Graph Int
graphOfEdges n = A.accumArray (flip (:)) [] (0, n - 1) . concatMap indexEdge
   where
      indexEdge edge@(from, to, _) = [(from, edge), (to, edge)]

-- Kruskal
kruskal :: Graph Int -> [Edge Int]
kruskal = foldEdges (dsuEmpty, 0) . sortBy (compare `on` edgeWeight) . listEdges
   where
      foldEdges _ [] = []
      foldEdges state@(dsu, numEdges) (edge@(from, to, _) : edges)
        | dsuSame dsu from to = foldEdges state edges
        | otherwise = edge : foldEdges (dsuJoin dsu from to, numEdges + 1) edges

buildStarsGraph :: Input -> Graph Int
buildStarsGraph input = graph
   where
      uniquePairs :: [a] -> [(a, a)]
      uniquePairs []       = []
      uniquePairs (x : xs) = [(x, y) | y <- xs] ++ uniquePairs xs

      stars = getStars input
      graph = graphOfEdges (length stars) $
         map
         (
            \(((i1, j1), ind1), ((i2, j2), ind2)) -> (ind1, ind2, abs (i1 - i2) + abs (j1 - j2))
         ) $
         uniquePairs (zip stars [0..])

mstEdgesSum :: Graph Int -> Int
mstEdgesSum  = sum . map (\(_, _, weight) -> weight) . kruskal

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Int
part1 input = startCnt + edgesSum
   where
      startCnt = length $ getStars input
      edgesSum = mstEdgesSum $ buildStarsGraph input

part2 :: Input -> Int
part2 = part1

part3 :: Input -> Int
part3 input = product $ take 3 $ reverse $ sort constellations
   where
      graph = buildStarsGraph input
      stars = getStars input
      edges =
         filter ((< 6) . edgeWeight) $
         sortBy (compare `on` edgeWeight) $
         listEdges graph

      edgesGrouped =
         groupBy ((==) `on` fst) $
         sortOn fst $
         map (\edge@(from, _, _) -> (dsuParent starsDsu from, edge)) edges

      constellationGraphs = map (graphOfEdges (length stars) . map snd) edgesGrouped
      constellations      = zipWith (+)
         (map mstEdgesSum constellationGraphs)
         (map uniqueStars constellationGraphs)

      uniqueStars =
         S.length .
         foldr (\(from, to, _) b -> S.insert from $ S.insert to b) S.empty .
         concat . A.elems

      starsDsu = foldEdges dsuEmpty edges
         where
            foldEdges dsu [] = dsu
            foldEdges dsu ((from, to, _) : edges) = foldEdges (dsuJoin dsu from to) edges

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 16
   testCase part1 "part1" 141

   -- Part 2
   testCase part2 "part2" 1214

   -- Part 3
   testCase part3 "part3_sample" 15624
   testCase part3 "part3" 5160368028
