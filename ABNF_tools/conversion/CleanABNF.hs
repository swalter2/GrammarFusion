module CleanABNF where

import AbsABNF

import Data.List (nub)
import Data.Map.Lazy as Hash hiding (map,filter,foldl)

import Debug.Trace


{- Cleaning ABNF grammar -}

cleanUp :: Grammar -> Grammar
cleanUp (Grammar defs) = Grammar $ (contractAndFilter . blowUp []) defs


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


-- contract and filter

type RuleHash   = Hash.Map Var [Fragment]
type RenameHash = Hash.Map Var Var

contractAndFilter :: [Def] -> [Def]
contractAndFilter defs = pipeline (buildHash empty defs)
        where pipeline = buildRules . removeDuplicateRHSs . removeDuplicateLHSs . removeDuplicateRHSs

buildHash :: RuleHash -> [Def] -> RuleHash
buildHash hash [] = hash
buildHash hash ((Header):defs) = buildHash hash defs
buildHash hash ((Root _):defs) = buildHash hash defs
buildHash hash ((Rule lhs frags):defs) = buildHash (updateHash hash lhs frags) defs
       where updateHash hash lhs frags = if member (var lhs) hash 
	                                        then Hash.adjust (++frags) (var lhs) hash
                                            else Hash.insert (var lhs) frags hash

removeDuplicateRHSs :: RuleHash -> RuleHash
removeDuplicateRHSs hash = foldl blah hash (keys hash)  

blah :: RuleHash -> Var -> RuleHash
blah hash var = adjust nub var hash

removeDuplicateLHSs :: RuleHash -> RuleHash
removeDuplicateLHSs hash = foldl blubb hash (keys hash)

blubb :: RuleHash -> Var -> RuleHash
blubb hash lhs = if member lhs hash 
                    then let duplicates = [ lhs' | lhs' <- keys hash, lhs' /= lhs && (hash ! lhs') == (hash ! lhs) ]
                             hashWithDuplicatesRemoved = foldl (flip delete) hash duplicates
                         in  foldl (\ h v -> foldl (flip $ adjust (rename v lhs)) h (keys h)) hashWithDuplicatesRemoved duplicates 
                 else hash

var :: LHS -> Var
var (Internal v) = v
var (Public   v) = v

buildRules :: RuleHash -> [Def] 
buildRules hash = map (\ k -> Rule (Internal k) (hash ! k)) (keys hash) 

-- renaming

rename :: Var -> Var -> [Fragment] -> [Fragment]
rename _   _   []  = []
rename dup lhs fs = map (renameInFragment dup lhs) fs

renameInFragment :: Var -> Var -> Fragment -> Fragment 
renameInFragment _ _  (Fragment []) = Fragment []
renameInFragment v v' (Fragment is) = Fragment (map (renameInItem v v') is)

renameInItem :: Var -> Var -> Item -> Item 
renameInItem (Var i1) (Var i2) (Terminal i) | i == i1   = Terminal i2
                                            | otherwise = Terminal i   
renameInItem v v' (NonTerminal var) | var == v  = NonTerminal v'
                                    | otherwise = NonTerminal var
renameInItem v v' (Option choices) = Option (map (\ (Choice is) -> Choice (map (renameInItem v v') is)) choices)
renameInItem v v' (Group  choices) = Group  (map (\ (Choice is) -> Choice (map (renameInItem v v') is)) choices)