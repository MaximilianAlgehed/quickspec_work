import System.IO

main = do
    loop False

loop b = do
    end <- isEOF
    if not end then
        do
            s <- getLine
            case s of
                [] -> do
                        putStrLn []
                        loop $ not b
                xs -> if not b then
                        do
                            putStrLn xs
                            loop b
                      else
                        loop b
    else
        return ()
