-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParFragments where
import AbsFragments
import LexFragments
import ErrM

}

%name pGrammar Grammar
%name pDef Def
%name pFragment Fragment
%name pItem Item
%name pListDef ListDef
%name pListItem ListItem
%name pListFragment ListFragment

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 ',' { PT _ (TS _ 1) }
 '.' { PT _ (TS _ 2) }
 '<' { PT _ (TS _ 3) }
 '>' { PT _ (TS _ 4) }

L_ident  { PT _ (TV $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }

Grammar :: { Grammar }
Grammar : ListDef { Grammar (reverse $1) } 


Def :: { Def }
Def : ListFragment { Rule $1 } 


Fragment :: { Fragment }
Fragment : ListItem { Fragment (reverse $1) } 


Item :: { Item }
Item : Ident { Terminal $1 } 
  | '<' Ident '>' { NonTerminal $2 }


ListDef :: { [Def] }
ListDef : {- empty -} { [] } 
  | ListDef Def '.' { flip (:) $1 $2 }


ListItem :: { [Item] }
ListItem : {- empty -} { [] } 
  | ListItem Item { flip (:) $1 $2 }


ListFragment :: { [Fragment] }
ListFragment : {- empty -} { [] } 
  | Fragment { (:[]) $1 }
  | Fragment ',' ListFragment { (:) $1 $3 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

