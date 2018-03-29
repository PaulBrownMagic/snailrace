module Main where

import Game (gameLoop)
import Art (drawTitle,
            drawScene,
            clear,
            setGreenText,
            setBrownText,
            shift
            )

main :: IO ()
main = do
    clear
    drawTitle
    setGreenText
    putStrLn $ shift 20 ++ "Welcome to the Snail Race"
    putStrLn $ shift 20 ++ "Be the first snail to cross the line"
    putStrLn $ shift 20 ++ "Snail 1 keys: A & S     Snail 2 keys: K & L"
    drawScene 30.4 31.7
    setGreenText
    putStrLn $ shift 30 ++ "Are you ready?"
    getLine
    setBrownText
    gameLoop
