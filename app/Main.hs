module Main where

import Game (gameLoop)
import Art (drawTitle, drawScene)

main :: IO ()
main = do
    drawTitle
    putStrLn "Welcome to the Snail Race"
    putStrLn "Be the first snail to cross the line"
    putStrLn "Player1 keys: A & S     Player2 keys: K & L"
    drawScene 0.0 0.0
    putStrLn "Are you ready?"
    getLine
    gameLoop
