module CleanABNF where

import AbsABNF


{- Cleaning ABNF grammar -}

cleanUp :: Grammar -> Grammar
cleanUp (Grammar defs) = Grammar $ blowUp [] defs


-- split rules containing options and groups into several rules

blowUp :: [Def] -> [Def] -> [Def] 
blowUp done []                      = done
blowUp done ((Rule lhs frags):defs) = blowUp (done ++ [Rule lhs (concat $ map expandFrag frags)]) defs
blowUp done (def:defs)              = blowUp (done ++ [def]) defs

expandFrag :: Fragment -> [Fragment]
expandFrag (Fragment [])     = []
expandFrag (Fragment (i:is)) = map (\ x -> Fragment x) (filter (/= []) (expandItems [] i is))

expandItems :: [[Item]] -> Item -> [Item] -> [[Item]]
expandItems done item []     = add done item
expandItems done item (i:is) = expandItems (add done item) i is

add :: [[Item]] -> Item -> [[Item]]
add []  t@(Terminal    _)  = [[t]] 
add is  t@(Terminal    _)  = map (++[t])  is
add [] nt@(NonTerminal _)  = [[nt]]
add is nt@(NonTerminal _)  = map (++[nt]) is
add is    (Option choices) = is ++ addOptions choices is
add is    (Group  choices) = addOptions choices is

addOptions :: [Choice] -> [[Item]] -> [[Item]]
addOptions [] done = done
addOptions ((Choice     []):cs) done = addOptions cs done
addOptions ((Choice (i:is)):cs) done = addOptions cs (done ++ (expandItems [] i is))

addItems :: [[Item]] -> [[Item]] -> [[Item]] 
addItems done [] = done 
addItems done ((i:is):items) = addItems (expandItems done i is) items
