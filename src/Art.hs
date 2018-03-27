module Art where

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
    putStrLn $ gap ++ "   _____             _ ______"
    putStrLn $ gap ++ "  / ___/____  ____ _(_) / __ \\____ _________"
    putStrLn $ gap ++ "  \\__ \\/ __ \\/ __ `/ / / /_/ / __ `/ ___/ _ \\"
    putStrLn $ gap ++" ___/ / / / / /_/ / / / _, _/ /_/ / /__/  __/"
    putStrLn $ gap ++ "/____/_/ /_/\\__,_/_/_/_/ |_|\\__,_/\\___/\\___/\n"
    where
        gap = take 25 $ repeat ' '



finishLine :: String -> String
finishLine s = s ++ toLine ++ "#"
    where
        toLine = take x $ repeat ' '
        x = 100 - length s

putFinLn :: String -> IO ()
putFinLn s = putStrLn $ finishLine s

chooseSnail :: Float -> [String]
chooseSnail f = if f - (fromIntegral $ floor f) > 0.5
                then snailA
                else snailB

shift :: Char -> Int -> String
shift c x = take x $ repeat c

clear :: IO ()
clear = putStr "\ESC[2J"

drawList :: Int -> [String] -> IO ()
drawList dist [x] = putFinLn $ (shift '_' dist) ++ x
drawList dist (x:xs) = do
    putFinLn $ (shift ' ' dist) ++ x
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
