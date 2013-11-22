module Converter where

import AbsABNF      as ABNF
import AbsFragments as Frag

import Data.List (nub)



{- Conversion ABNF -> Fragments -}

convertABNF2Fragments :: ABNF.Grammar -> Frag.Grammar
convertABNF2Fragments (ABNF.Grammar defs) = Frag.Grammar $ map convertRuleABNF2Fragments defs

convertRuleABNF2Fragments :: ABNF.Def -> Frag.Def 
convertRuleABNF2Fragments (ABNF.Rule lhs frags) = Frag.Rule (convertLHS lhs : nub (map convertFragments frags))
convertRuleABNF2Fragments _                     = Frag.Rule []

convertLHS :: ABNF.LHS -> Frag.Fragment 
convertLHS (ABNF.Internal (ABNF.Var (ABNF.Ident s))) = Frag.Fragment [ Frag.NonTerminal $ Frag.Ident s ]
convertLHS (ABNF.Public   (ABNF.Var (ABNF.Ident s))) = Frag.Fragment [ Frag.NonTerminal $ Frag.Ident s ]

convertFragments :: ABNF.Fragment -> Frag.Fragment 
convertFragments (ABNF.Fragment items) = Frag.Fragment (map convertItem items)

convertItem :: ABNF.Item -> Frag.Item
convertItem (ABNF.NonTerminal (ABNF.Var (ABNF.Ident s))) = Frag.NonTerminal $ Frag.Ident s
convertItem (ABNF.Terminal              (ABNF.Ident s))  = Frag.Terminal    $ Frag.Ident s
convertItem _                                            = error "Oops."



{- Conversion Fragments -> ABNF -}

convertFragments2ABNF :: Frag.Grammar -> ABNF.Grammar
convertFragments2ABNF (Frag.Grammar defs) = ABNF.Grammar [] -- map convertRuleFragments2ABNF defs

--convertRuleABNF2Fragments :: ABNF.Def -> ABNF.Def 
--convertRuleABNF2Fragments ...