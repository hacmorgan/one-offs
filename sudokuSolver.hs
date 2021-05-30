import Data.List

type Board = [[Int]]
type Box = (Int, Int)

solveSudoku :: Board -> Maybe Board
solveSudoku board 
  | nextOpenSquare board == (-1,-1)   = Just board
  | otherwise = go board (y,x)
    where
      (y,x) = nextOpenSquare board
      go :: Board -> Box -> Maybe Board
      go board' (b,a)
        | canIncrement board' (b,a) = do
            let board'' = solveSudoku $ validIncrement board' (b,a)
            if board'' == Nothing
              then go (validIncrement board' (b,a)) (b,a)
              else board''
        | otherwise = Nothing
      
nextOpenSquare :: Board -> Box
nextOpenSquare board = search (0,0) board
  where
    search :: Box -> Board -> Box
    search (y,x) board'
      | y > 8 || x > 8        = (-1,-1) 
      | board' !! y !! x /= 0 = search (nextValidIndex (y,x)) board'
      | otherwise             = (y,x)
    nextValidIndex :: Box -> Box
    nextValidIndex (y,x)
      | x == 8    = (y+1, 0)
      | otherwise = (y,   x+1)

isValid :: Board -> Box -> Bool
isValid board (y,x) =  checkRow board y
                    && checkColumn board x
                    && check3x3Box board (y,x)
  where
    checkRow :: Board -> Int -> Bool
    checkRow board' row = go (board' !! row) [1..9]
      where
        go :: [Int] -> [Int] -> Bool
        go [] _ = True
        go (a:as) is
          | a == 0    = go as is
          | otherwise = elem a is && go as (delete a is)
    checkColumn :: Board -> Int -> Bool
    checkColumn board' col = go (transpose board' !! col) [1..9]
      where
        go :: [Int] -> [Int] -> Bool
        go [] _ = True
        go (b:bs) is
          | b == 0    = go bs is
          | otherwise = elem b is && go bs (delete b is)
    check3x3Box :: Board -> Box ->  Bool
    check3x3Box board' (row,col) = go board' (3*(row `div` 3), 3*(col `div` 3)) bs as ks
      where
        bs = [0,0,0,1,1,1,2,2,2]
        as = [0,1,2,0,1,2,0,1,2]
        ks = [1..9]
        go :: Board -> Box -> [Int] -> [Int] -> [Int] -> Bool
        go _ _ [] _ _ = True
        go board'' (ym,xm) (j:js) (i:is) ns
          | board'' !! (ym+j) !! (xm+i) == 0 =  go board'' (ym,xm) js is ns
          | otherwise                        =  elem (board'' !! (ym+j) !! (xm+i)) ns
                                             && go board'' (ym,xm) js is (delete (board'' !! (ym+j) !! (xm+i)) ns)

canIncrement :: Board -> Box -> Bool
canIncrement board (y,x)
  | board !! y !! x == 9 = False
  | otherwise = go (blindIncrement board (y,x)) (y,x)
    where
      go :: Board -> Box -> Bool
      go board' (b,a)
        | isValid board' (b,a)  = True
        | board' !! b !! a == 9 = False
        | otherwise             = go (blindIncrement board' (b,a)) (b,a)

validIncrement :: Board -> Box -> Board
validIncrement board (y,x)
  | isValid (blindIncrement board (y,x)) (y,x) = blindIncrement board (y,x)
  | board !! y !! x > 9                        = error "this would be bad"
  | otherwise                                  = validIncrement (blindIncrement board (y,x)) (y,x)

blindIncrement :: [[Int]] -> (Int, Int) -> [[Int]]
blindIncrement = go 0 
  where
    go :: Int -> [[Int]] -> (Int, Int) -> [[Int]]
    go n (xs:xss) (row,col) 
      | n == row  = (goRow 0 xs col) : xss
      | otherwise = xs               : go (n+1) xss (row,col)
    goRow :: Int -> [Int] -> Int -> [Int]
    goRow n (x:xs) col
      | n == col  = (x+1) : xs
      | otherwise = x     : goRow (n+1) xs col


printBoard :: Show a => [[a]] -> IO ()
printBoard [] = pure ()
printBoard (xs:xss) = putStrLn (show xs) >> printBoard xss

printSolution :: Maybe Board -> IO ()
printSolution Nothing  = putStrLn "failure :("
printSolution (Just a) = printBoard a


main :: IO () 
main = printSolution $ solveSudoku trishBoard
  

-- impossibleBoard :: Board
-- impossibleBoard  = [ [0,0,0,0,0,0,0,2,6],
--                      [0,0,0,4,7,0,9,0,1],
--                      [0,5,0,3,0,0,0,0,0],
--                      [0,0,3,0,0,0,2,0,0],
--                      [0,0,9,7,6,8,4,0,0],
--                      [0,0,1,0,0,0,8,0,0],
--                      [9,0,0,0,0,7,0,9,0],
--                      [7,0,6,0,1,4,0,0,0],
--                      [9,3,0,0,0,0,0,0,0] ]
blankBoard :: Board
blankBoard  = [ [0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0] ]


trishBoard :: Board
trishBoard  = [ [7,0,0,0,0,0,0,0,0],
                [0,0,2,0,1,9,4,5,0],
                [9,0,5,0,8,6,7,0,0],
                [0,2,0,0,0,1,0,0,0],
                [1,8,3,4,0,7,5,2,9],
                [0,0,0,8,0,0,0,7,0],
                [0,0,9,6,4,0,1,0,7],
                [0,1,6,3,7,0,9,0,0],
                [0,0,0,0,0,0,0,0,2] ]

-- incompleteBoard0 :: Board
-- incompleteBoard0  = [ [0,0,0,0,0,0,0,2,6],
--                       [0,0,0,4,7,0,9,0,1],
--                       [0,5,0,3,0,0,0,0,0],
--                       [0,0,3,0,0,0,2,0,0],
--                       [0,0,9,7,6,8,4,0,0],
--                       [0,0,1,0,0,0,8,0,0],
--                       [0,0,0,0,0,7,0,9,0],
--                       [7,0,6,0,1,4,0,0,0],
--                       [9,3,0,0,0,0,0,0,0] ]

-- incompleteBoard1 :: Board
-- incompleteBoard1 = [ [0,0,1,0,0,0,3,0,4],
--                      [0,9,0,0,0,2,0,0,5],
--                      [0,5,0,0,3,0,0,0,0],
--                      [4,0,0,6,0,0,8,0,0],
--                      [1,0,7,3,0,4,5,0,2],
--                      [0,0,5,0,0,1,0,0,7],
--                      [0,0,0,0,8,0,0,3,0],
--                      [7,0,0,4,0,0,0,5,0],
--                      [5,0,3,0,0,0,1,0,0] ]

-- completeBoard1 :: Board
-- completeBoard1 = [ [8,7,1,9,5,6,3,2,4],
--                    [3,9,6,1,4,2,7,8,5],
--                    [2,5,4,7,3,8,6,9,1],
--                    [4,2,9,6,7,5,8,1,3],
--                    [1,8,7,3,9,4,5,6,2],
--                    [6,3,5,8,2,1,9,4,7],
--                    [9,1,2,5,8,7,4,3,6],
--                    [7,6,8,4,1,3,2,5,9],
--                    [5,4,3,2,6,9,1,7,8] ]


{-
null :: [a] -> Bool
concat :: [[a]] -> [a]
map (filter (== 0)) :: [[a]] -> [[a]]
map :: (a -> b) -> [a] -> [b]
filter (== 0) :: [a] -> [a]
-}
