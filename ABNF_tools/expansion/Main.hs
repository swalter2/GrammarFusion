module Main where

import System.IO
import System.Environment
import System.Exit

import Parser 
import CleanABNF
import Expander



main :: IO ()
main = getArgs >>= dispatch


dispatch (f:[]) = doExpand f

usage = putStrLn "Usage: ./expand <file>"
exit  = exitWith ExitSuccess


doExpand :: String -> IO ()
doExpand f = do 
    handle  <- openFile f ReadMode  
    content <- hGetContents handle
    out     <- return $ f ++ "_all"
    grammar <- return $ cleanUp $ parseABNF $ preprocess content
    writeFile out $ foldl (++) "" $ filter (/= "") $ expand grammar
    hClose handle
    putStrLn $ "Expansion written to: " ++ out
    exit
