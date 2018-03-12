{--
Shorouk Said Abdalla Abdelaziz
15100852
SPL - Course Project
--}

module Solver
(
solve , solveLogically , notSolved
) where
import Boards
import Helper
import Data.List
import System.Random  -- first run : "cabal install random" in power shell
import Data.List (nub, sortBy)


-- takes a cells and returns all it's neighbours in a square
getSquare :: (Int , Int) -> [(Int,Int)]
getSquare (x ,y) = [(a,b) | a <- range x , b <- range y ]
  where
    range x
      | div x 3 == 0 = [n | n <- [0..2]]
      | div x 3 == 1 = [n | n <- [3..5]]
      | div x 3 == 2 = [n | n <- [6..8]]

-- takes a cell and returns its neighbours in the same square that needs to be checked
getSquareToBeChecked :: (Int,Int) -> [(Int,Int)]
getSquareToBeChecked (x,y) = [cell | cell <- (getSquare (x,y) ),fst(cell) /= x , snd(cell) /=y ]

-- takes a cell and returns all its neighbours
cellsToCheck :: Int -> Int -> [(Int,Int)]
cellsToCheck x y = [( x , b )| b <-[0..8] , y /= b ]++[( a , y )| a <-[0..8] , x /= a ] ++ (getSquareToBeChecked (x, y))

--takes a grid and a cell and returns  the values of that cell
takenNumbers :: [[Int]]->(Int , Int) -> Int
takenNumbers board  (x,y) = (board !! x )!! y

{--============================================================================================================--}

--takes a grid and a cell and generates a list of all possible solutions for that cell
cellPossibilities :: [[Int]] -> (Int , Int) -> [Int]
cellPossibilities board (x,y)
  | ((board !! x) !! y) /= 0 = [0,0]
  | otherwise = [n | n <- [1..9] , elem n (map (takenNumbers board) (cellsToCheck x y)) == False]

-- takes a grid and returns a list of each cell index and a list its possible solutions
gridPossibilities :: [[Int]] -> [((Int,Int),[Int])]
gridPossibilities board = [(index , possibilitiesOfIndex) | index<- indices ,let possibilitiesOfIndex = (cellPossibilities board index) ]

--takes a board returns a list of cells that have only one possible solution
cellsWithUniqueSolution:: [[Int]] -> [((Int,Int),Int)]
cellsWithUniqueSolution board = [ (x,y)| cell <-xs ,let x = fst cell , let j = snd cell  , (length j)==1 ,let y =  j !! 0  ]
  where xs = gridPossibilities board

{--=======================================================================================================--}

--a list of all possible solutions in a given row or column
-- put i = 0 for a row or 1 for a col
listPossibility:: Int -> Int -> [[Int]] -> [Int]
listPossibility i n board = concat (map (cellPossibilities board) list)
  where
    list = if i == 0
      then  [(n,y)|y<-[0..8]]
      else  [(x,n)|x<-[0..8]]

--returns a list of all the digits that could be placed in only one cell with respect to a row or a col
--put i = 0 for a row or 1 for a col
uniqueInAList :: Int -> [[Int]] -> [[Int]]
uniqueInAList i board = [unique (listPossibility i n board) | n <-[0..8]]

--returns (row or col nmber,[digits that could be placed in only one cell])
--put i = 0 for a row or 1 for a col
listsPossibility :: Int-> [[Int]] -> [(Int,[Int])]
listsPossibility  i board  = [(l,val) | l <- [0..8] , let val = (uniqueInAList i  board) !! l , val /= []]

--returns (row or col number , unique solution)
listsPossilities' :: Int -> [[Int]] -> [(Int,Int)]
listsPossilities' i board = [(l,val) | x <- xs , let l = fst(x) , val <- snd(x) ]
  where
    xs = listsPossibility i board

{--returns cells that has unique solution with respect to rows or columns
put i = 0 for a row or 1 for a col--}
cellsWithUniqueSolutionInAList :: Int -> [[Int]] -> [((Int,Int),Int)]
cellsWithUniqueSolutionInAList 0 board = [((r,c),val) | x<- xs  , p<- poss , let r = fst(x) , let c = snd(fst(p)) , let val = snd(x) ,r == fst(fst(p)) , elem val (snd(p)) == True  ]
  where
    xs = listsPossilities' 0 board
    poss = gridPossibilities board
cellsWithUniqueSolutionInAList 1 board = [((r,c),val) | x<- xs  , p<- poss , let c = fst(x) , let r = fst(fst(p)) , let val = snd(x) ,c == snd(fst(p)) , elem val (snd(p)) == True  ]
  where
    xs = listsPossilities' 1 board
    poss = gridPossibilities board

{--=======================================================================================================--}

listOfSquares :: [[(Int,Int)]]
listOfSquares = map getSquare squares
  where squares = [(x,y) | x<-[0,3,6] , y<-[0,3,6]]

squarePossibility ::  Int -> [[Int]] -> [Int]
squarePossibility  n board = concat (map (cellPossibilities board) (listOfSquares !! n))

uniqueInASquare :: [[Int]] -> [[Int]]
uniqueInASquare board = [unique (squarePossibility n board) | n <-[0..8]]

squaresPossibility :: [[Int]] -> [(Int,[Int])]
squaresPossibility  board  = [(s,val) | s <- [0..8] , let val = (uniqueInASquare   board) !! s , val /= []]

squaresPossiblities' ::[[Int]] -> [(Int,Int)]
squaresPossiblities' board = [(s,val) | x <- xs , let s = fst(x) , val <- snd(x) ]
  where
    xs = squaresPossibility  board

possibilitiesForASquare sqNum board = [(index , value) | index <- (listOfSquares !! sqNum) , values <- (gridPossibilities board) , let value = snd(values) , fst(values) == index]

--which cell in square x has the value val
which board (x , val) = [(cell,val) | cells <- (possibilitiesForASquare x board) , let cell = fst (cells) , elem val (snd(cells)) ==True  ]

cellsWithUniqueSolutionInSquare :: [[Int]] -> [((Int,Int),Int)]
cellsWithUniqueSolutionInSquare board = concat (map (which board) (squaresPossiblities' board) )

{--=======================================================================================================--}

readyToFillSolutions board = cellsWithUniqueSolution board ++ cellsWithUniqueSolutionInAList 0 board++cellsWithUniqueSolutionInAList 1 board ++ cellsWithUniqueSolutionInSquare board

{--=======================================================================================================--}

findSolutionsandUpdate :: [[Int]] -> [[Int]]
findSolutionsandUpdate board = makeNewBoard (readyToFillSolutions board) board
{--=======================================================================================================--}

notSolved :: [[Int]] -> Bool
notSolved xs = elem 0 (concat xs)

{--=======================================================================================================--}
solveS :: [[Int]] -> [[Int]]
solveS board = ans
  where
    ans = if notSolved board == False --if no more zeros ie done
      then board
      else  if (findSolutionsandUpdate board) == board -- if no new move and guessing is nedded
      then guess (guesses board) board
      else  solveS (findSolutionsandUpdate board) --if still able to solve logically
{--=======================================================================================================--}
cellsWith2Possibilities:: [[Int]] -> [((Int,Int),[Int])]
cellsWith2Possibilities board =[(fst(p) , snd(p) )| p <-  gridPossibilities board , (snd(p)!!0 )/=0 , length(snd(p)) == 2 ]

guesses :: [[Int]] -> [((Int,Int),Int)]
guesses board = [(index,guess) | item <- (cellsWith2Possibilities board) , let index = fst(item) , let g = snd(item) , guess <- g  ]

guessSolutionsandUpdate ::Int -> [[Int]] -> [[Int]]
guessSolutionsandUpdate n board = makeNewBoard [((guesses board)!!n)] board

guess :: [((Int,Int),Int)] -> [[Int]] -> [[Int]]
guess xs board = temp
  where
    temp = if notSolved (solveS (makeNewBoard [xs!!1] board ))
      then solveS (makeNewBoard [xs!!0] board )
      else solveS (makeNewBoard [xs!!1] board )

{--=======================================================================================================--}
--solves Boards that doesn't need guessing
solveLogically :: [[Int]] -> [[Int]]
solveLogically board = ans
  where
    ans = if notSolved board == False --if no more zeros ie done
      then board
      else  if (findSolutionsandUpdate board) == board -- if no new move and guessing is nedded
      then stop
      else  solveS (findSolutionsandUpdate board) --if still able to solve logically
{--=======================================================================================================--}
solve :: [[Int]]  -> IO ()
solve board = putStrLn (nicestBoard(solveS board))
