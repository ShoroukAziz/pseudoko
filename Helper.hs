{--
Shorouk Said Abdalla Abdelaziz
15100852
SPL - Course Project
--}

module Helper
(
indices,
occFound,
unique,
makeNewBoard,
printBoard,
rndSelect,
nicestBoard
)where
import Boards
import Data.List (nub, sortBy)
import System.Random (mkStdGen ,randomRs)
--generates a list if indices for the whole grid
indices :: [( Int,Int)]
indices = [(x,y)| x <- [0..8] , y <- [0..8]]


--counts occurances of a number in a list
occFound :: Int -> [Int] -> Int
occFound x xs = length (filter (==x) xs)

--returns the elements that is expected in only one cell in a given row
unique :: [Int] -> [Int]
unique xs =  [x | x<-[1..9] , let  y = occFound x xs , y==1]

-- takes [((x,y) solution)] and board and returns new board
makeNewBoard::  [((Int,Int),Int)] -> [[Int]] -> [[Int]]
makeNewBoard [] board = board
makeNewBoard (cellAndVal:cells) board = do
  let temp = take r  board ++ [newRow] ++ drop (r+1) board
  makeNewBoard cells temp
  where
    value = snd(cellAndVal)
    r = fst(fst(cellAndVal))
    c = snd(fst(cellAndVal))
    newRow = take c (board !! r) ++ [value] ++ drop (c+1) (board !! r)

  -- 4. Extract a given number of randomly selected elements from a list.
rndSelect :: Int-> [Int] -> Int -> [Int]
rndSelect s1 xs n = map (xs !!) rs
    where rs = take n $ nub $ randomRs (0, length xs - 1) (mkStdGen s1)

printBoard board =  sequence_ $ board >>= (\x -> [print x])


stringBoard board = [show(x) | x <- board  ]
replace board = [map repl x | x<- (stringBoard board)]
niceBoard board =  (map (insert 6 '|') (replace board))
--nicerBoard (x:xs) = "----------------------"++"\n"++"|"++ x ++ "\n"++ nicerBoard xs
--nicerBoard [] = "----------------------"
nicerBoard (x:(y:(z:xs))) ="----------------------"++"\n"++"|"++ x ++"\n"++ "|------+------+------|"++"\n"++"|"++ y ++ "\n"++ "|------+------+------|"++"\n"++"|"++z++"\n"++nicerBoard xs
nicerBoard [] = "----------------------\n"
nicestBoard board = nicerBoard (niceBoard board)

repl '0' = '.'
repl '[' = ' '
repl ']' = ' '
repl ',' = ' '
repl '"' = ' '
repl c = c

insert :: Int -> a -> [a] -> [a]
insert n y xs = countdown n xs where
   countdown 0 xs = y:countdown n xs -- reset to original n
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs
