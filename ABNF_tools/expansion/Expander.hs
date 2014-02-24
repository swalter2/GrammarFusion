module Expander where 

import AbsABNF as ABNF

import Data.Map.Lazy as Hash hiding (map,filter,foldl)
import Data.List (nub)
import Debug.Trace


type RuleHash  = Hash.Map Var [Fragment]
type CountHash = Hash.Map Var Int


expand :: ABNF.Grammar -> [String]
expand (Grammar defs) = expandRule h c (access h (getRoot defs))
          where (h,c) = buildHash empty empty defs

-- build hash

buildHash :: RuleHash -> CountHash -> [ABNF.Def] -> (RuleHash,CountHash)
buildHash h c []                      = (h,c)
buildHash h c ((Rule lhs frags):defs) = buildHash (updateHash h (var lhs) frags) (updateCount c (var lhs) 0) defs
buildHash h c (_               :defs) = buildHash h c defs

var :: ABNF.LHS -> ABNF.Var
var (Internal v) = v
var (Public   v) = v

updateHash :: RuleHash -> ABNF.Var -> [Fragment] -> RuleHash
updateHash h v frags = if member v h 
	                      then Hash.adjust (++frags) v h
                          else Hash.insert v frags h

updateCount :: CountHash -> ABNF.Var -> Int -> CountHash 
updateCount c v i = if member v c 
	                   then c
	                   else Hash.insert v i c

count :: CountHash -> ABNF.Var -> CountHash 
count c v = if member v c 
	           then Hash.adjust (+1) v c
	           else Hash.insert v 1 c

-- access root and rules

getRoot :: [ABNF.Def] -> ABNF.Var
getRoot []              = error "Grammar does not specify a root."
getRoot ((Root v):defs) = v
getRoot (_:defs)        = getRoot defs

access :: RuleHash -> ABNF.Var -> [ABNF.Fragment]
access h v = if member v h 
	            then (h ! v)
	            else []

getCount :: CountHash -> ABNF.Var -> Int
getCount c v = if member v c
	            then (c ! v)
	            else 0	            


-- expand rules

startContext = [""]
limit        = 2

expandRule :: RuleHash -> CountHash -> [ABNF.Fragment] -> [String]
expandRule h c frags = nub $ map (toString " . ") $ recurse h c frags

recurse :: RuleHash -> CountHash -> [ABNF.Fragment] -> [ABNF.Fragment] 
recurse h c fs | stop      = filter onlyTerminals fs
	           | otherwise = recurse h new_c (concat $ map fst step)
    where 
          vars  = concat $ map (collectVars [] . getItems) fs
          stop  = (all onlyTerminals fs) || (any (>limit) $ map (getCount c) vars)
          step  = map (oneStep h c) fs
          new_c = sumupCountHash c step

oneStep :: RuleHash -> CountHash -> ABNF.Fragment -> ([ABNF.Fragment],CountHash)
oneStep h c (Fragment items) = (join $ map (expandItem h new_c) items, new_c)
                 where new_c = foldl count c (collectVars [] items)

expandItem :: RuleHash -> CountHash -> ABNF.Item -> [ABNF.Fragment]
expandItem h c i@(NonTerminal v) = access h v
expandItem h c i                 = [Fragment [i]]



join :: [[ABNF.Fragment]] -> [ABNF.Fragment]
join []     = []
join [f]    = f
join (f:fs) = foldl addTo f fs

addTo :: [ABNF.Fragment] -> [ABNF.Fragment] -> [ABNF.Fragment]
addTo fs1 fs2 = [ Fragment (getItems f1 ++ getItems f2) | f1 <- fs1, f2 <- fs2 ] 

getItems :: ABNF.Fragment -> [ABNF.Item] 
getItems (Fragment is) = is

collectVars :: [ABNF.Var] -> [ABNF.Item] -> [ABNF.Var]
collectVars vars []                   = vars
collectVars vars ((NonTerminal v):is) = collectVars (v:vars) is
collectVars vars (_              :is) = collectVars    vars  is

onlyTerminals :: ABNF.Fragment -> Bool 
onlyTerminals (Fragment [])     = True 
onlyTerminals (Fragment (i:is)) = isTerminal i && onlyTerminals (Fragment is)

isTerminal :: ABNF.Item -> Bool 
isTerminal (Terminal    _)           = True
isTerminal (NonTerminal _)           = False
isTerminal (Option [])               = True
isTerminal (Group  [])               = True
isTerminal (Option ((Choice is):cs)) = all isTerminal is && isTerminal (Option cs)
isTerminal (Group  ((Choice is):cs)) = all isTerminal is && isTerminal (Group  cs)

toString :: String -> ABNF.Fragment -> String 
toString s (Fragment []) = s
toString s (Fragment is) = foldl (++) s (map itemToString is)

itemToString :: ABNF.Item -> String
itemToString (Terminal         (Ident s))  = s ++ " "
itemToString (NonTerminal (Var (Ident s))) = s ++ " "
itemToString (Option [])                   = ""
itemToString (Group  [])                   = ""
itemToString (Option ((Choice is):cs))     = (foldl (++) "" (map itemToString is)) ++ itemToString (Option cs)
itemToString (Group  ((Choice is):cs))     = (foldl (++) "" (map itemToString is)) ++ itemToString (Group  cs)


sumupCountHash :: CountHash -> [([ABNF.Fragment],CountHash)] -> CountHash
sumupCountHash c [] = c
sumupCountHash c ls = foldl merge c (map snd ls)
  
merge :: CountHash -> CountHash -> CountHash
merge c1 c2 = updateToMax c2 (Hash.toList c1)
        where updateToMax c2 []            = c2
              updateToMax c2 ((k,v):items) = if member k c2 && v > (c2 ! k)
                                                then Hash.adjust (\ x -> v) k c2
                                                else updateToMax c2 items
