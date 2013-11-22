module Parser where

import AbsABNF as ABNF
import LexABNF
import ParABNF
import PrintABNF
import AbsFragments as Frags
import LexFragments
import ParFragments
import PrintFragments

import Data.List (isPrefixOf)
import Data.String.Utils (replace)

import ErrM



data Format = ABNF | Fragments


parseABNF :: String -> ABNF.Grammar
parseABNF s = case ParABNF.pGrammar (ParABNF.myLexer s) of 
                   Ok e -> e
                   _    -> error $ "Failed when parsing ABNF file:\n" ++ s

printABNF :: ABNF.Grammar -> String
printABNF = PrintABNF.printTree

parseFragments :: String -> Frags.Grammar
parseFragments s = case ParFragments.pGrammar (ParFragments.myLexer s) of 
                        Ok e -> e
                        _    -> error $ "Failed when parsing Fragments file:\n" ++ s

printFragments :: Frags.Grammar -> String
printFragments = PrintFragments.printTree


---- Pre- and postprocessing

preprocess :: Format -> String -> String
preprocess ABNF      s = foldl (++) "" $ filter (\ l -> not $ isPrefixOf "meta" l) (lines s)
preprocess Fragments s = foldl (++) "" $ map ((++) " . ") (lines s)

postprocess :: Format -> String -> String
postprocess ABNF      s = (replace "$ " "$") s
postprocess Fragments s = (replace "< " "<" . replace " >" ">" . replace " - " "-" . replace ", " "," . replace ". " "\n") s