module Main where

import Reader
import System.Environment
import System.IO
import System.Exit

main = do
        args <- System.Environment.getArgs
        let filename = head args
        raw <- readFile filename
        let src = makeSourceCode raw
            ss  = maybe [] (\xs -> xs) $ findStart src 
            res = map (\(p, d) -> readThrough src p d) ss
        putStrLn $ head res
        
