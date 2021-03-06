module SkelFragments where

-- Haskell module generated by the BNF converter

import AbsFragments
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transGrammar :: Grammar -> Result
transGrammar x = case x of
  Grammar defs  -> failure x


transDef :: Def -> Result
transDef x = case x of
  Rule fragments  -> failure x


transFragment :: Fragment -> Result
transFragment x = case x of
  Fragment items  -> failure x


transItem :: Item -> Result
transItem x = case x of
  Terminal id  -> failure x
  NonTerminal id  -> failure x



