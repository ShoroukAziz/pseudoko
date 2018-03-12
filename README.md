# _P_seudoko
a 9x9 sudoku generator and solver
### The solver:
solves easy to hard Sudoku (puzzles that requires no guessing)
### The generator:
Generates boards that can be solved by the solver.
-	The generator is limited to a certain number of board as it doesn’t use backtracking to generate the boards but shuffling a systematically generated board.

## Files
-	Solver.hs : the solver script
-	gen.hs    : the generator script
-	Helper.hs : contains some helper functions used in the solver and generator.
-	Boards.hs : some sample boards to test

## Use
### Solver
- load the script 
```
:l Solver.hs
```
- to solve a sample board 
solve "board name"
```
solve easy
```
- to solve anyboard
solve [[board row]]
```
solve [[9,1,0,4,0,2,0,5,7],[5,0,6,0,9,0,0,0,0],[0,0,2,6,0,1,3,9,0],[0,0,0,0,0,8,2,0,0],[8,2,0,0,0,0,0,6,9],[0,0,3,7,0,0,0,0,0],[0,6,5,1,0,9,8,0,0],[0,0,0,0,4,0,9,0,5],[3,7,0,2,0,5,0,4,6]]

```

### Generator
- load the script
```
:l gen.hs
```
to generate a board
```
play
```

## Sample boards 
list of boards ready to test:
- easy
- med
- hard
- evil
