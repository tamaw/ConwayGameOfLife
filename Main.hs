{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import SDL (($=))
import SDL.Vect
import qualified SDL
import Control.Monad (unless)
import Foreign.C.Types
import Data.Foldable (for_)
import qualified Data.Vector.Storable.Mutable as MSV
import System.Random (randomRIO)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
-- import Prelude hiding (catch)

data State =
    Dead
    | Alive
    deriving (Enum, Eq, Show)

type Grid = MSV.IOVector Int
type X = Int
type Y = Int

width :: X
width = 800
height :: Y
height = 600

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow
     "Conway Game of Life"
      SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    SDL.showWindow window

    renderer <- SDL.createRenderer
       window
       (-1)
       SDL.RendererConfig
       { SDL.rendererType  = SDL.AcceleratedVSyncRenderer
       , SDL.rendererTargetTexture = False
       }

    -- grid <- MSV.replicate (width*height) (fromEnum Dead)
    let size = (width * height)+1
    gridA <- MSV.replicate size (fromEnum Dead)
    gridB <- MSV.replicate size (fromEnum Dead)

    -- gridPoints <- MSV.replicate (width*height) (P (V2 0 (0::CInt)))

    -- random alive cells
    for_ [0.. (width * height)-1] $ \i -> do
        v <- randomRIO (0, 1::Int)
        MSV.write gridA i (if v >= 1 then fromEnum Alive else fromEnum Dead)

    -- for_ [0 .. (screenWidth-1)] $ \i ->
    --     for_ [0 .. (screenHeight-1)] $ \j -> do
    --         let index = (fromIntegral screenHeight * fromIntegral j) + (fromIntegral i)
    --         if even index then
    --             MSV.write gridA index (fromEnum Alive)
    --         else
    --             MSV.write gridA index (fromEnum Dead)

    mv <- newEmptyMVar
    mv2 <- newEmptyMVar

    -- ch <- newChan
    -- ch2 <- newChan

    let loop n = do
        let frontGrid = if even n then gridA else gridB
        let backGrid = if odd n then gridA else gridB

        -- events <- SDL.pollEvents
        -- mousepos <- SDL.getAbsoluteMouseLocation

        -- don't really need to split
        -- let (a,b) = MSV.splitAt (width * height `div` 2) frontGrid
        -- step frontGrid backGrid renderer (0, width * (height `div` 2))
        -- step frontGrid backGrid renderer (width * (height `div` 2), width * height)

        forkIO $ do
           step frontGrid backGrid renderer (0, width * height `div` 2)
        --    step2 a backGrid renderer (width * (height-3) `div` 2)
           putMVar mv True
        --    writeChan ch True
           return ()


        forkIO $ do
           step frontGrid backGrid renderer (width * height `div` 2, width * height)
        --    step2 b backGrid renderer (width * (height-3) `div` 2)
        --    writeChan ch True
           putMVar mv2 True
           return ()

        readMVar mv2
        readMVar mv
        -- readChan ch
        -- readChan ch

        -- do box <- writeMVar
        -- forkIO (MSV.write frontGrid 0 0)

        -- let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
        let quit = n == 100

        -- step frontGrid backGrid renderer (width * height)
        draw frontGrid backGrid renderer

        SDL.present renderer
        unless quit $ loop (n+1)

    loop 0

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
    where
        step2 frontGrid backGrid renderer length =
            for_ [0.. length] $ \i -> do
                v <- MSV.read frontGrid (fromIntegral i)

                let (x,y) = toCell i
                let neighbours = getNeighbours (x,y)
                let currentState = (toEnum v :: State)

                aliveCount <- mapM (getCellState frontGrid) neighbours

                let newState = evolve (sum aliveCount) currentState

                MSV.write backGrid i (fromEnum newState)


        -- read from frontgrid and write to backgrid
        step frontGrid backGrid renderer (from,to) =
            for_ [from.. to] $ \i -> do
                v <- MSV.read frontGrid (fromIntegral i)

                let (x,y) = toCell i
                let neighbours = getNeighbours (x,y)
                let currentState = (toEnum v :: State)

                aliveCount <- mapM (getCellState frontGrid) neighbours

                let newState = evolve (sum aliveCount) currentState

                MSV.write backGrid i (fromEnum newState)

        draw frontGrid backGrid renderer =
            for_ [0 .. (screenWidth -1)] $ \i ->
                for_ [0 .. (screenHeight -1)] $ \j -> do
                    let index = (fromIntegral j * fromIntegral screenHeight) + fromIntegral i
                    v <- MSV.read backGrid index

                    if v == fromEnum Alive then
                        SDL.rendererDrawColor renderer $= V4 0 0 0 0
                    else
                        SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

                    SDL.drawPoint renderer (P (V2 i j))



-- handleEvent :: Point V2 CInt -> SDL.EventPayload -> Int
-- handleEvent mousepos ev =
--     let x = case ev of
--                 SDL.MouseButtonEvent e
--                     | SDL.mouseButtonEventMotion e == SDL.Pressed -> 1
--                     | otherwise -> 0
--                 _ -> 0


    -- for_ [0.. (width * height)] $ \i -> do
    --     neighbourCells <- map toCell (getNeighbours (toCell i))
    --     aliveCount <- sum $ map getCellState neighbourCells
    --     -- aliveCount
    --     cell
    -- for_ [0.. (width * height)] $ \i ->
    --     MSV.write gridA (fromIntegral i) (fromEnum Alive)
    -- gridA
        -- count n =
            -- for_ [0..8] $ \j -> do
                -- v <- MSV.read gridA j

countNeighbours :: [Int] -> Grid -> IO Int
countNeighbours neighbours grid = do go neighbours grid 0 0
    where
        go n g i acc
            | i >= length n = return acc
            | otherwise = do
                v <- MSV.read g (n !! i)
                -- v <- getCellState g (n !! i)
                go n g (i+1) (v+acc)

evolve :: Int -> State -> State
evolve n s
        | n < 2 = Dead -- underpopulation
        | n > 3 = Dead -- overpopulation
        | n == 3 = Alive -- reproduction
        | otherwise = if s == Alive then Alive else Dead

toCell :: Int -> (Int, Int)
toCell i = (i `rem` width, i `div` width)

getNeighbours :: (Int, Int) -> [Int]
getNeighbours (x, y) = filter (\i -> i >= 0 && i < width * height)
    [ (width * (y+1)) + x
    , (width * (y+1)) + x+1
    , (width * y)     + x+1
    , (width * (y-1)) + x+1
    , (width * (y-1)) + x
    , (width * (y-1)) + x-1
    , (width * y)     + x-1
    , (width * (y+1)) + x-1
    ]

getCellState :: Grid -> Int -> IO Int
getCellState g i =
    if i >= 0 && i < width * height then do MSV.read g i
    else return 0

