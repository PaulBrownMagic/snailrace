module Game
    ( gameLoop
    ) where

import System.IO
import System.Exit (exitSuccess)
import Control.Monad
import Data.Char
import Art

player :: Float
player = 0.0

trim :: Float -> Float
trim n = (/10) . fromIntegral . round $ n * 10

charTest :: Char -> Char -> Char -> Float -> Bool
charTest k1 k2 c f
  | even dc && c == k1 = True
  | odd dc && c == k2 = True
  | otherwise = False
  where
    dc = flip mod 10 $ round (f * 10)

p1CharTest :: Char -> Float -> Bool
p1CharTest = charTest 'a' 's'

p2CharTest :: Char -> Float -> Bool
p2CharTest = charTest 'k' 'l'

incr :: Float -> Float
incr = (+0.1)

movePlayer :: Float -> Float
movePlayer p = trim $ incr p

runLogic :: Char -> Float -> Float -> IO ()
runLogic c p1 p2
    | p1CharTest c p1 = gameLoop' (movePlayer p1) p2
    | p2CharTest c p2 = gameLoop' p1 $ movePlayer p2
    | otherwise = gameLoop' p1 p2

gameLoop' :: Float -> Float -> IO ()
gameLoop' p1 p2 = do
      blitScene p1 p2
      gameOver p1 p2
      c <- hGetChar stdin
      runLogic c p1 p2

gameWin :: String -> Float -> IO ()
gameWin s p = if p >= 70.5
              then do
                  putStrLn s
                  getLine
                  exitSuccess
              else return ()

p1GameWin :: Float -> IO ()
p1GameWin = gameWin "Player 1 Wins!"

p2GameWin :: Float -> IO ()
p2GameWin = gameWin "Player 2 Wins!"

gameOver :: Float -> Float -> IO ()
gameOver p1 p2 = do
    p1GameWin p1
    p2GameWin p2

gameLoop :: IO ()
gameLoop = do
    hSetBuffering stdin NoBuffering
    gameLoop' player player
