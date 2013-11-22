module Main where

import System.IO
import System.Environment
import System.Exit

import Parser 
import Converter
import CleanABNF

import Debug.Trace


main :: IO ()
main = getArgs >>= dispatch


dispatch ("--abnf":f:[]) = convert ABNF      f
dispatch ("--frag":f:[]) = convert Fragments f
dispatch _               = usage >> exit

usage = putStrLn "Usage: ./convert [--abnf|--frag] file"
exit  = exitWith ExitSuccess


convert :: Format -> String -> IO ()
convert ABNF f = do 
    handle  <- openFile f ReadMode  
    content <- hGetContents handle
    out     <- return $ f ++ "_converted"
    tmp     <- return $ f ++ "_tmp"
    cleaned <- return $ cleanUp $ parseABNF $ preprocess ABNF content
    writeFile tmp $ postprocess ABNF $ printABNF cleaned
    putStrLn $ "Intermediated ABNF grammar written to: " ++ tmp
    writeFile out $ postprocess Fragments $ printFragments $ convertABNF2Fragments cleaned
    hClose handle
    putStrLn $ "Converted grammar written to: " ++ out
    exit
convert Fragments f = do 
    handle  <- openFile f ReadMode  
    content <- hGetContents handle
    out     <- return $ f ++ "_converted"
    writeFile out $ postprocess ABNF $ printABNF $ convertFragments2ABNF $ parseFragments $ preprocess Fragments content
    hClose handle
    putStrLn $ "Converted grammar written to: " ++ out
    exit