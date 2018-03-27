module Art (drawTitle,
            drawScene,
            blitScene) where

shift' :: Char -> Int -> String
shift' c x = take x $ repeat c

shift :: Int -> String
shift = shift' ' '


clear :: IO ()
clear = putStr "\ESC[2J"

snailA :: [String]
snailA = ["        _________             @"
         ,"       /         \\       ____/"
         ,"      /    ____   \\     /    \\"
         ,"     /    /    \\   \\   /    _/"
         ,"    (    (  \\__/    ) /    /"
         ,"     \\    \\______/ /_/    /"
         ,"   ___\\           /      /"
         ,"_-\"____\\_________/______/"]

snailB :: [String]
snailB = ["        _________           @"
         ,"       /         \\      ____|"
         ,"      /    ____   \\    /    \\"
         ,"     /    /    \\   \\  /    _/"
         ,"    (    (  \\__/    )/    /"
         ,"     \\    \\______/ //    /"
         ,"   ___\\           /     /"
         ,"_-\"____\\_________/_____/"]

drawTitle :: IO ()
drawTitle = do
    putStrLn "\n"
    put "   _____             _ ______"
    put "  / ___/____  ____ _(_) / __ \\____ _________"
    put "  \\__ \\/ __ \\/ __ `/ / / /_/ / __ `/ ___/ _ \\"
    put " ___/ / / / / /_/ / / / _, _/ /_/ / /__/  __/"
    put "/____/_/ /_/\\__,_/_/_/_/ |_|\\__,_/\\___/\\___/\n"
    where
        put s = putStrLn $ gap ++ s
        gap = shift 25

finishLine :: String -> String
finishLine s = s ++ toLine ++ "#"
    where
        toLine = shift $ 100 - length s

putFinLn :: String -> IO ()
putFinLn s = putStrLn $ finishLine s

chooseSnail :: Float -> [String]
chooseSnail f = if f - (fromIntegral $ floor f) > 0.5
                then snailA
                else snailB

drawList :: Int -> [String] -> IO ()
drawList dist [x] = putFinLn $ (shift' '_' dist) ++ x
drawList dist (x:xs) = do
    putFinLn $ (shift dist) ++ x
    drawList dist xs

drawSnail :: Float -> IO ()
drawSnail f = do
    putFinLn ""
    putFinLn ""
    drawList dist snail
    putFinLn ""
    putFinLn ""
        where
             snail = chooseSnail f
             dist = floor f

drawScene :: Float -> Float -> IO ()
drawScene f1 f2 = do
    drawSnail f1
    drawSnail f2
    putStrLn "\n"

blitScene :: Float -> Float -> IO ()
blitScene f1 f2 = do
    clear
    drawTitle
    drawScene f1 f2
