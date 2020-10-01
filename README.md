accepts content from the standard input, the format of it must be

<name>
003020600
900305001
001806400
008102900
700000008
006708200
002609500
800203009
005010300

whith each number representing a sudoku square, with 0 being an empty square,
multiple sudokus can be inputed as once. It is easier to pipe the source
 sudoku file into the executable

for example:

to compile use : 
ghc -o sudoku sudoku.hs
to execute:
cat sudokus.txt | ./sudoku
