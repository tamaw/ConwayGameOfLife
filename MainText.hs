{-# LANGUAGE OverloadedStrings #-}

import GHC.Base
import qualified Control.Monad (when)
data State =
    Alive
    | Dead
    deriving (Enum, Eq, Show)

type Grid = [State]
type X = Int
type Y = Int
newtype Cell = Cell (X, Y)

width :: X
width = 50
height :: Y
height = 10

grid :: [State]
grid = replicate (width * height) Dead

main :: IO ()
main = do
    putStrLn "Press enter to evolve or 'q' to quit."
    let myGrid = populate grid
    printGrid myGrid
    untilQuit myGrid
    where
        untilQuit :: Grid -> IO ()
        untilQuit g = do
            input <- getLine
            when (input /= "q")
                    $ do
                        let ng = tessellation g
                        printGrid ng
                        untilQuit ng

tessellation :: Grid -> Grid
tessellation grid = go grid 0 []
    where
        go :: Grid -> Int -> Grid -> Grid
        go [] _ a = a
        go (x:xs) i a = go xs (i+1) (a ++ [newState])
            where
                newState = evolve neighboursCount currentState
                currentState = x
                neighboursCount = cellNeighbourCount (indexToCell i) grid -- full grid!
                evolve n s
                        | n < 2 = Dead -- underpopulation
                        | n > 3 = Dead -- overpopulation
                        | n == 3 = Alive -- reproduction
                        | otherwise = if s == Alive then Alive else Dead


-- could pull up??
cellNeighbourCount :: Cell -> Grid -> Int
cellNeighbourCount (Cell (x,y)) g = sum $ map aliveAsInt cells
    where
        cells = [ Cell (x   , y+1)
                , Cell (x+1 , y+1)
                , Cell (x+1 , y)
                , Cell (x+1 , y-1)
                , Cell (x   , y-1)
                , Cell (x-1 , y-1)
                , Cell (x-1 , y)
                , Cell (x-1 , y+1)
                ]
        aliveAsInt c = fromEnum $ isCellAlive c g

isCellAlive :: Cell -> Grid -> Bool
isCellAlive c g = (index >= 0 && index < width*height) && checkForLife (g !! index )
    where
        index = cellToIndex c
        checkForLife :: State -> Bool
        checkForLife s = case s of
                    Alive -> True
                    Dead  -> False

changeCellState :: Cell -> State -> Grid -> Grid
changeCellState c s g = output
    where
        output = before ++ [s] ++ after
        before = take index g
        after = drop (index+1) g
        index = cellToIndex c

cellToIndex :: Cell -> Int
cellToIndex (Cell (x,y)) = (width * y) + x

indexToCell :: Int -> Cell
indexToCell i = Cell (i `mod` width, i `div` width)


printGrid :: Grid -> IO ()
printGrid g = putStrLn $ lineBreaker $ map stateToChar g
    where
        stateToChar s = case s of
            Alive -> '◽'
            Dead  -> '◾'

lineBreaker :: String -> String
lineBreaker s = reverse $ insertBreaks s 0 []
    where
        insertBreaks :: String -> Int -> String -> String
        insertBreaks [] _ a = a
        insertBreaks (x:xs) i a = if i /= 0 && (i+1) `mod` width == 0
                                then insertBreaks xs (i+1) a ++ "\n" ++ [x]
                                else insertBreaks xs (i+1) a ++ [x]

populate :: Grid -> Grid
populate g = foldl (\b a -> changeCellState a Alive b) g toLive
    where
        -- toLive = [ Cell (12, 4)
        --          , Cell (13, 4)
        --          , Cell (14, 4)
        --          , Cell (11, 5)
        --          , Cell (12, 5)
        --          , Cell (13, 5)
        --          ]
        toLive = [ Cell (9, 4)
                 , Cell (14,4)
                 , Cell (7, 5)
                 , Cell (8, 5)
                 , Cell (10, 5)
                 , Cell (11, 5)
                 , Cell (12, 5)
                 , Cell (13, 5)
                 , Cell (15, 5)
                 , Cell (16, 5)
                 , Cell (9, 6)
                 , Cell (14, 6)
                ]

