-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParABNF where
import AbsABNF
import LexABNF
import ErrM

}

%name pGrammar Grammar
%name pDef Def
%name pLHS LHS
%name pFragment Fragment
%name pItem Item
%name pChoice Choice
%name pVar Var
%name pListDef ListDef
%name pListItem ListItem
%name pListFragment ListFragment
%name pListChoice ListChoice

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '#ABNF 1.0 UTF-8' { PT _ (TS _ 1) }
 '$' { PT _ (TS _ 2) }
 '(' { PT _ (TS _ 3) }
 ')' { PT _ (TS _ 4) }
 ';' { PT _ (TS _ 5) }
 '=' { PT _ (TS _ 6) }
 '[' { PT _ (TS _ 7) }
 ']' { PT _ (TS _ 8) }
 'public' { PT _ (TS _ 9) }
 'root' { PT _ (TS _ 10) }
 '|' { PT _ (TS _ 11) }

L_ident  { PT _ (TV $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }

Grammar :: { Grammar }
Grammar : ListDef { Grammar (reverse $1) } 


Def :: { Def }
Def : '#ABNF 1.0 UTF-8' { Header } 
  | 'root' Var { Root $2 }
  | LHS '=' ListFragment { Rule $1 $3 }


LHS :: { LHS }
LHS : Var { Internal $1 } 
  | 'public' Var { Public $2 }


Fragment :: { Fragment }
Fragment : ListItem { Fragment (reverse $1) } 


Item :: { Item }
Item : Ident { Terminal $1 } 
  | Var { NonTerminal $1 }
  | '[' ListChoice ']' { Option $2 }
  | '(' ListChoice ')' { Group $2 }


Choice :: { Choice }
Choice : ListItem { Choice (reverse $1) } 


Var :: { Var }
Var : '$' Ident { Var $2 } 


ListDef :: { [Def] }
ListDef : {- empty -} { [] } 
  | ListDef Def ';' { flip (:) $1 $2 }


ListItem :: { [Item] }
ListItem : {- empty -} { [] } 
  | ListItem Item { flip (:) $1 $2 }


ListFragment :: { [Fragment] }
ListFragment : {- empty -} { [] } 
  | Fragment { (:[]) $1 }
  | Fragment '|' ListFragment { (:) $1 $3 }


ListChoice :: { [Choice] }
ListChoice : {- empty -} { [] } 
  | Choice { (:[]) $1 }
  | Choice '|' ListChoice { (:) $1 $3 }



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

