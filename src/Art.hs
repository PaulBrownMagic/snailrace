module Art (drawTitle,
            drawScene,
            blitScene,
            clear,
            setGreenText,
            setBrownText,
            shift,
            resetColor,
            ) where

import Conf (raceLength)

shift' :: Char -> Int -> String
shift' c x = take x $ repeat c

shift :: Int -> String
shift = shift' ' '

slimeTrail' :: Int -> String
slimeTrail' n = setColor (fg 's') ++ shift' '_' n

slimeTrail :: Int -> String
slimeTrail n = slimeTrail' n ++ setColor (fg 'b')

col :: String -> Char -> String
col g c = case c of
    'b' -> mk 88
    'g' -> mk 150
    'o' -> mk 16
    'w' -> mk 231
    's' -> mk 58
    _ -> mk 0
  where
      mk n = g ++ ":5:" ++ (show n)

fg :: Char -> String
fg = col "38"

bg :: Char -> String
bg = col "48"

setGreenBG :: IO ()
setGreenBG = putStrLn "\ESC[49:5:150m"

setColor :: String -> String
setColor c = "\ESC[1;" ++ c ++ "m"

setColors :: String -> String -> String
setColors fg bg = setColor $ fg ++ ";" ++ bg

setBrownText :: IO ()
setBrownText = putStr $ setColor (fg 'b')      --"48:5:150;38:5:88m"

setGreenText ::  IO ()
setGreenText = putStr $ setColor (fg 'g')

setWhiteText ::  IO ()
setWhiteText = putStr $ setColor (fg 'w')

resetColor :: IO ()
resetColor = putStrLn "\ESC[0m"

clear :: IO ()
clear = putStr "\ESC[2J"

snailA :: [String]
snailA = ["       _________             @"
         ,"      /         \\       ____/"
         ,"     /    ____   \\     /    \\"
         ,"    /    /    \\   \\   /    _/"
         ,"   (    (  \\__/    ) /    /"
         ,"    \\    \\______/ /_/    /"
         ,"  ___\\           /      /"
         ,"-\"____\\_________/______/"]

snailB :: [String]
snailB = ["       _________           @"
         ,"      /         \\      ____|"
         ,"     /    ____   \\    /    \\"
         ,"    /    /    \\   \\  /    _/"
         ,"   (    (  \\__/    )/    /"
         ,"    \\    \\______/ //    /"
         ,"  ___\\           /     /"
         ,"-\"____\\_________/_____/"]

drawTitle :: IO ()
drawTitle = do
    setGreenText
    putStrLn "\n"
    put "   _____             _ ______"
    put "  / ___/____  ____ _(_) / __ \\____ _________"
    put "  \\__ \\/ __ \\/ __ `/ / / /_/ / __ `/ ___/ _ \\"
    put " ___/ / / / / /_/ / / / _, _/ /_/ / /__/  __/"
    putStrLn $ (shift' '_' 25) ++ "/____/_/ /_/\\__,_/_/_/_/ |_|\\__,_/\\___/\\___/\n"
    where
        put s = putStrLn $ gap ++ s
        gap = shift 25

putFinLn' :: Int -> String -> IO ()
putFinLn' l s = do
    putStr s
    putStr $ shift $ raceLength - l
    setWhiteText
    putStrLn "#"
    setBrownText

putFinLn :: String -> IO ()
putFinLn s = putFinLn' (length s) s

chooseSnail :: Float -> [String]
chooseSnail f = if f - (fromIntegral $ floor f) > 0.5
                then snailA
                else snailB

drawList :: Int -> [String] -> IO ()
drawList dist [x] = putFinLn' dif s
                    where
                        s = (slimeTrail dist) ++ x
                        dif = length s - length (setColor (fg 'g') ++ setColor (fg 'b')) + 1
drawList dist (x:xs) = do
    putFinLn s
    drawList dist xs
    where
        s = shift dist ++ x

drawSnail :: Float -> IO ()
drawSnail f = do
    drawList dist snail
    putFinLn ""
    putFinLn ""
        where
             snail = chooseSnail f
             dist = floor f

drawScene :: Float -> Float -> IO ()
drawScene f1 f2 = do
    setBrownText
    putFinLn ""
    putFinLn "Snail 1"
    drawSnail f1
    putFinLn ""
    putFinLn "Snail 2"
    drawSnail f2

blitScene :: Float -> Float -> IO ()
blitScene f1 f2 = do
    clear
    drawTitle
    drawScene f1 f2

