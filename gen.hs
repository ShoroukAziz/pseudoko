{--
Shorouk Said Abdalla Abdelaziz
15100852
SPL - Course Project
--}

import Solver
import Helper
import Boards
import Data.List (delete)
import System.Random
import Data.List
import System.Random  -- first run : "cabal install random" in power shell
import Data.List (nub, sortBy)
import System.Random (mkStdGen ,randomRs)

seeds :: [[Int]]
seeds = permutations [1,2,3,4,5,6,7,8,9]

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

slice from to xs = take (to - from + 1) (drop from xs)

dummyBoard :: [Int] ->[[Int]]
dummyBoard seed  = do
  let r1 = rotate 6 seed
  let r2 = rotate 6 r1
  let r3 = rotate 5 r2
  let r4 = rotate 6 r3
  let r5 = rotate 6 r4
  let r6 = rotate 5 r5
  let r7 = rotate 6 r6
  let r8 = rotate 6 r7
  [seed]++[r1]++[r2]++[r3]++[r4]++[r5]++[r6]++[r7]++[r8]


swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs = let elemI = xs !! i
                            elemJ = xs !! j
                            left = take i xs
                            middle = take (j - i - 1) (drop (i + 1) xs)
                            right = drop (j + 1) xs
                    in  left ++ [elemJ] ++ middle ++ [elemI] ++ right


rotateL :: [[Int]] -> [[Int]]
rotateL = reverse . transpose

rotateR :: [[Int]] -> [[Int]]
rotateR =  transpose

shuffle:: Int -> Int -> Int-> [[Int]] -> [[Int]]
shuffle m n o board = do
  let a = rotateR board
  let b = swapElementsAt m (m+1) a
  let c = swapElementsAt n (n+1) b
  let d = swapElementsAt o (o+1) c
  let e = rotateL d
  e

generateFullBoard ::Int -> Int -> Int -> Int-> Int -> [[Int]]
generateFullBoard s1 m n o rand = shuffle m n o ( dummyBoard (seeds !! rand) )

removeCell :: (Int,Int) -> [[Int]] -> [[Int]]
removeCell (x,y) board = makeNewBoard [((x,y),0)] board

-- from 30 for to 45 max
cellsSeed ::Int -> Int -> [Int]
cellsSeed s1 n_removed = rndSelect s1 [0..81] n_removed

isSafe  :: Int -> [[Int]] -> Bool
isSafe x board = if notSolved (solveLogically(removeCell (indices !!x) board)) == True then False else True

prepareBoard :: [Int] -> [[Int]] -> [[Int]]
prepareBoard [] board = board
prepareBoard (x:xs) board = sol
  where sol = if isSafe x board == True then ( prepareBoard xs (removeCell (indices !!x) board))
              else if isSafe x board == True then ( prepareBoard xs (removeCell (indices !!x) board))
              else board

generate ::Int -> Int -> Int -> Int-> Int -> Int -> [[Int]]
generate s1 m n o n_removed rnd_board = prepareBoard (cellsSeed s1 n_removed) (generateFullBoard s1 m n o rnd_board)

gen s1 m n o x y = putStrLn (nicestBoard(generate s1 m n o x y))

play :: IO ()
play = do
    y <- randomRIO (0,362879)
    x <- randomRIO (35,45)
    m <- randomRIO (0,1)
    n <- randomRIO (3,4)
    o <- randomRIO (6,7)
    s1 <-randomRIO (98,100)

    gen s1 m n o x y
