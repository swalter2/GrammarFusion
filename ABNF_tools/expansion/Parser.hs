module Parser where

import AbsABNF as ABNF
import LexABNF
import ParABNF
import PrintABNF

import Data.List (isPrefixOf)
import Data.String.Utils (replace)

import ErrM


parseABNF :: String -> ABNF.Grammar
parseABNF s = case ParABNF.pGrammar (ParABNF.myLexer s) of 
                   Ok e -> e
                   _    -> error $ "Failed when parsing ABNF file:\n" ++ s

printABNF :: ABNF.Grammar -> String
printABNF = PrintABNF.printTree


---- Pre- and postprocessing

preprocess :: String -> String
preprocess s = foldl (++) "" $ filter (\ l -> not $ isPrefixOf "meta" l) (lines s)

postprocess :: String -> String
postprocess s = (replace "$ " "$") s
