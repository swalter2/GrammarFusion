module Converter where

import AbsABNF      as ABNF
import AbsFragments as Frag

import Data.List (nub)
import Data.Char (toUpper)



{- Conversion ABNF -> Fragments -}

convertABNF2Fragments :: ABNF.Grammar -> Frag.Grammar
convertABNF2Fragments (ABNF.Grammar defs) = Frag.Grammar $ map convertRuleABNF2Fragments defs

convertRuleABNF2Fragments :: ABNF.Def -> Frag.Def 
convertRuleABNF2Fragments (ABNF.Rule lhs frags) = Frag.Rule (convertLHS lhs : nub (map convertABNFFragments frags))
convertRuleABNF2Fragments _                     = Frag.Rule []

convertLHS :: ABNF.LHS -> Frag.Fragment 
convertLHS (ABNF.Internal (ABNF.Var (ABNF.Ident s))) = Frag.Fragment [ Frag.NonTerminal $ Frag.Ident s ]
convertLHS (ABNF.Public   (ABNF.Var (ABNF.Ident s))) = Frag.Fragment [ Frag.NonTerminal $ Frag.Ident s ]

convertABNFFragments :: ABNF.Fragment -> Frag.Fragment 
convertABNFFragments (ABNF.Fragment items) = Frag.Fragment (map convertABNFItem items)

convertABNFItem :: ABNF.Item -> Frag.Item
convertABNFItem (ABNF.NonTerminal (ABNF.Var (ABNF.Ident s))) = Frag.NonTerminal $ Frag.Ident s
convertABNFItem (ABNF.Terminal              (ABNF.Ident s))  = Frag.Terminal    $ Frag.Ident (map toUpper s)
convertABNFItem _                                            = error "Oops."



{- Conversion Fragments -> ABNF -}

convertFragments2ABNF :: Frag.Grammar -> ABNF.Grammar
convertFragments2ABNF (Frag.Grammar defs) = ABNF.Grammar $ (ABNF.Header : mapOverRules defs [])

mapOverRules :: [Frag.Def] -> [ABNF.Def] -> [ABNF.Def]
mapOverRules [] done = done
mapOverRules ((Frag.Rule []):defs) done = mapOverRules defs done
mapOverRules (def:defs) done = (convertRuleFragments2ABNF def) : mapOverRules defs done

convertRuleFragments2ABNF :: Frag.Def -> ABNF.Def 
convertRuleFragments2ABNF (Frag.Rule [])     = error "I encountered an empty rule."
convertRuleFragments2ABNF (Frag.Rule (f:fs)) = ABNF.Rule (convertFragment2LHS f) (map convertFragFragments fs) 

convertFragFragments :: Frag.Fragment -> ABNF.Fragment
convertFragFragments (Frag.Fragment items) = ABNF.Fragment (map convertFragItem items)

convertFragItem :: Frag.Item -> ABNF.Item 
convertFragItem (Frag.Terminal    (Frag.Ident s)) = ABNF.Terminal (ABNF.Ident s)
convertFragItem (Frag.NonTerminal (Frag.Ident s)) = ABNF.NonTerminal (ABNF.Var (ABNF.Ident s))

convertFragment2LHS :: Frag.Fragment -> ABNF.LHS 
convertFragment2LHS (Frag.Fragment [ Frag.NonTerminal (Frag.Ident s) ]) = ABNF.Internal (ABNF.Var (ABNF.Ident s))
convertFragment2LHS _ = error "Left-hand side must be exactly one non-terminal!"

