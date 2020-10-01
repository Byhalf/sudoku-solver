import Data.List

data Sudoku  = Sudoku { horizontal :: [String]
                        ,vertical :: [String]
                        ,squares :: [String]} deriving (Eq)

sudokuMaker :: [String] -> Sudoku 
sudokuMaker l = Sudoku {horizontal=l,vertical=transpose l,squares=getSquares l}

makeMove :: Sudoku -> ((Int,Int),String) -> Sudoku
makeMove (Sudoku h v sq) ((x,y), symb)=
    let newLine = take x (h!!y) ++ symb ++ drop (x+1) (h!!y)
        in sudokuMaker $take y h ++ [newLine]  ++ drop (y+1) h




getBestMove ::  [((Int,Int),String)] ->((Int,Int),String)
getBestMove  = minimumBy (\x y -> if length (snd x)> length (snd y) then GT else LT)  

solveSudoku :: Sudoku -> Sudoku
solveSudoku s1 =  
    let 
        --s2 = keepMakingObviousMoves s1
        possibileMoves =  sortBy (\x y -> if length (snd x)> length (snd y) then GT else LT) $ possibilityList s1
        
    in
        solveSudoku' s1 possibileMoves
 

solveSudoku' :: Sudoku -> [((Int,Int),String)] -> Sudoku
solveSudoku' s [] = s
solveSudoku' s l
    -- | isItValid s = s if is valid empty list so useless
    | length m == 1 = solveSudoku' (makeMove s ((x, y), m)) updatedl 
    | otherwise =
        let movedS = makeMove s ((x, y), [head m]) 
            fsudoku = solveSudoku' movedS updatedl
        in
            if isItValid fsudoku then fsudoku else
                solveSudoku' s  (reduceMove ((x, y), [head m]) l)
    where 
        bestMove = getBestMove l
        ((x,y),m) = bestMove 
        updatedl = updatePossibilityList ((x, y), [head m]) l

reduceMove :: ((Int,Int),String) -> [((Int,Int),String)] -> [((Int,Int),String)]
reduceMove m [] = []
reduceMove ((x,y),m) (((x2,y2),m2):ls) = if x==x2 && y==y2 then ((x2,y2), m2\\m ):ls else ((x2,y2),m2): reduceMove ((x,y),m)  ls



updatePossibilityList :: ((Int,Int),String) ->  [((Int,Int),String)] -> [((Int,Int),String)]
updatePossibilityList m [] = []
updatePossibilityList ((x1,y1),m) (((x2,y2),m2):ls) 
  | x1==x2 && y1==y2 = updatePossibilityList ((x1,y1),m) ls
  | x1==x2 || y1==y2 || (3*div y1 3 + div x1 3) == (3*div y2 3 + div x2 3)= 
    if m2\\m /= "" then ((x2,y2), m2\\m ):updatePossibilityList ((x1,y1),m) ls else []
  |otherwise = ((x2,y2),m2):updatePossibilityList ((x1,y1),m) ls



isValidMove :: Sudoku -> ((Int,Int),String) -> Bool
isValidMove s m = isItMaybeValid $ makeMove s m   

--Is the sudoku valid and finished
isItValid :: Sudoku -> Bool
isItValid (Sudoku h v sq) = and $ map validHelper h ++ map validHelper v ++ map validHelper sq

validHelper :: String -> Bool
validHelper s = "123456789" `intersect` s == "123456789"




--does it respect sudoku rules even if it cant be finished
isItMaybeValid :: Sudoku -> Bool
isItMaybeValid (Sudoku h v sq) = and $ map validHelper' h ++ map validHelper' v ++ map validHelper' sq


validHelper' :: String -> Bool
validHelper' s = let s' = filter (/='0') s in nub s' == s'

----------------------------------------------------------------------------------------------------------



possibilityList :: Sudoku -> [((Int,Int),String)]
possibilityList s = filter (\x-> snd x /= "") [possibilitiesSquare x y s | x <- [0 .. 8], y <- [0 .. 8]]



possibileNbrs :: String -> String
possibileNbrs s = "123456789" \\ s

possibilitiesSquare :: Int -> Int -> Sudoku -> ((Int,Int),String)
possibilitiesSquare x y (Sudoku h v sq) =
    if h!!y!!x == '0'
        then
            let posib = intersect ((possibileNbrs $h !! y) `intersect` (possibileNbrs $v !! x))  (possibileNbrs $sq!!(3*div y 3 + div x 3))
            in
                ((x,y),posib)
        else
            ((x,y),"")


toString1Line :: String -> String
toString1Line  =  drop 3 . foldl (\x y-> x++" | "++y) "" . split3 

toString3Squares :: [String] -> String
toString3Squares  = unlines .  map toString1Line 


toStringHori :: [String] -> String
toStringHori = drop 17 . foldl (\x y-> x++"----------------\n"++y) "" . map toString3Squares . split3

split3 :: [a] -> [[a]]
split3 [] = []
split3 s  = take 3 s : split3 (drop 3 s) 

getSquares' :: [[a]] -> [[a]]
getSquares' l = [map (!!0) l , map (!!1) l, map (!!2) l]

getSquares :: [[a]] -> [[a]]
getSquares hori = map concat $ concat $map getSquares' (split3 (map split3 hori))

main = do 
    content <- getContents
    let c = lines content
    let res = solveSudokus c
    mapM putStrLn res
    print $ checkSudokus res


solveSudokus :: [String] -> [String]
solveSudokus [] = []
solveSudokus l = 
    let s = drop 1 $ take 10 l in
        take 1 l ++ horizontal ( solveSudoku  (sudokuMaker s))  ++ solveSudokus (drop 10 l ) 

checkSudokus :: [String] -> [Bool]
checkSudokus [] = []

checkSudokus l =
    let s = sudokuMaker $ drop 1 $ take 10 l in
        isItValid s : checkSudokus (drop 10 l ) 

