module CleanABNF where

import AbsABNF

import Data.Map.Lazy as Hash hiding (map,filter,foldl)

import Debug.Trace


{- Cleaning ABNF grammar -}

cleanUp :: Grammar -> Grammar
cleanUp (Grammar defs) = Grammar $ blowUp [] defs -- (contractAndFilter . blowUp []) defs


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

{-
-- contract and filter

contractAndFilter :: [Def] -> [Def]
contractAndFilter defs = buildRules $ removeNonsense $ contract (buildHash empty defs) defs

buildHash :: Hash.Map Var [Fragment] -> [Def] -> Hash.Map Var [Fragment]
buildHash hash [] = hash
buildHash hash ((Header):defs) = buildHash hash defs
buildHash hash ((Root _):defs) = buildHash hash defs
buildHash hash ((Rule lhs frags):defs) = buildHash (updateHash hash lhs frags) defs
       where updateHash hash lhs frags = if member (var lhs) hash 
	                                    then Hash.adjust (++frags) (var lhs) hash
                                            else Hash.insert (var lhs) frags hash

contract :: Hash.Map Var [Fragment] -> [Def] -> Hash.Map Var [Fragment]
contract hash [] = hash 
contract hash (def@(Rule lhs [Fragment [NonTerminal v]]):defs) = 
  case Hash.lookup v hash of 
             Just frags -> contract (Hash.insert v (hash ! v) hash) defs
             Nothing    -> contract hash defs
contract hash (def:defs) = contract hash defs

removeNonsense :: Hash.Map Var [Fragment] -> Hash.Map Var [Fragment]
removeNonsense hash = foldl blah hash (keys hash)
     where blah h k = case (filterFrags h [] (h ! k)) of 
                            [] -> Hash.delete k h
                            fs -> Hash.insert k fs h
-- TODO delete all keys never occur on any RHS

filterFrags :: Hash.Map Var [Fragment] -> [Fragment] -> [Fragment] -> [Fragment]
filterFrags _    keep [] = keep
filterFrags hash keep ((Fragment []):fs) = filterFrags hash keep fs
filterFrags hash keep ((Fragment is):fs) = filterFrags hash (keep++[Fragment (filter (noUndefinedNonTerminalIn hash) is)]) fs

noUndefinedNonTerminalIn :: Hash.Map Var [Fragment] -> Item -> Bool 
noUndefinedNonTerminalIn hash (Terminal _)     = True 
noUndefinedNonTerminalIn hash (NonTerminal v)  = Hash.member v hash
noUndefinedNonTerminalIn hash (Option choices) = any (noUndefinedNonTerminalIn hash) $ concat $ map (\ (Choice is) -> is) choices
noUndefinedNonTerminalIn hash (Group  choices) = any (noUndefinedNonTerminalIn hash) $ concat $ map (\ (Choice is) -> is) choices

var :: LHS -> Var
var (Internal v) = v
var (Public   v) = v

buildRules :: Hash.Map Var [Fragment] -> [Def] 
buildRules hash = map (\ k -> Rule (Internal k) (hash ! k)) (keys hash) 

-}
