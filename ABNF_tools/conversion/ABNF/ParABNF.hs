{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParABNF where
import AbsABNF
import LexABNF
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.9

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn14 :: (Ident) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Ident)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Grammar) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Grammar)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Def) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Def)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (LHS) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (LHS)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Fragment) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Fragment)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Item) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Item)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Choice) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Choice)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Var) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Var)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([Def]) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ([Def])
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ([Item]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ([Item])
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([Fragment]) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([Fragment])
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ([Choice]) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ([Choice])
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x16\x00\x04\x00\x00\x00\x05\x00\x00\x00\x5a\x00\x00\x00\x00\x00\x57\x00\x55\x00\x56\x00\x00\x00\x54\x00\x05\x00\x53\x00\x50\x00\x05\x00\x4f\x00\x02\x00\x01\x00\x4f\x00\x4e\x00\x4a\x00\x00\x00\x4a\x00\x00\x00\x52\x00\x4d\x00\x46\x00\x46\x00\x00\x00\x51\x00\x41\x00\x4c\x00\x00\x00\x4b\x00\x3b\x00\x16\x00\x39\x00\x00\x00\x0f\x00\x00\x00\x3e\x00\x35\x00\x00\x00\x00\x00\x0f\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x12\x00\x49\x00\x3d\x00\x2a\x00\x21\x00\x48\x00\x30\x00\x1a\x00\x10\x00\x27\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x1e\x00\x40\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x00\x32\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\xfa\xff\x00\x00\x33\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xe6\xff\x00\x00\x00\x00\xe4\xff\x00\x00\xe4\xff\x00\x00\xe6\xff\xe4\xff\xe4\xff\xe4\xff\x00\x00\xf4\xff\xde\xff\xe8\xff\x00\x00\xe1\xff\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xec\xff\x00\x00\xeb\xff\xe4\xff\xe4\xff\x00\x00\x00\x00\xef\xff\x00\x00\x00\x00\x00\x00\xf2\xff\x00\x00\x00\x00\xf3\xff\x00\x00\xf1\xff\xe4\xff\xee\xff\x00\x00\x00\x00\xe7\xff\xe3\xff\xe4\xff\xe4\xff\xdd\xff\xe0\xff\xe9\xff\xea\xff\xf0\xff\xe5\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x07\x00\x01\x00\x02\x00\x02\x00\x03\x00\x02\x00\x02\x00\x03\x00\x07\x00\x09\x00\x0a\x00\x07\x00\x09\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x04\x00\x01\x00\x05\x00\x00\x00\x08\x00\x01\x00\x02\x00\x09\x00\x08\x00\x07\x00\x0e\x00\x0e\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x08\x00\x05\x00\x04\x00\x07\x00\x05\x00\x04\x00\x07\x00\x09\x00\x0a\x00\x04\x00\x09\x00\x0a\x00\x04\x00\x06\x00\x09\x00\x0a\x00\x09\x00\x09\x00\x0b\x00\x02\x00\x03\x00\x07\x00\x06\x00\x04\x00\x07\x00\x09\x00\x06\x00\x0b\x00\x05\x00\x09\x00\x03\x00\x0b\x00\x02\x00\x03\x00\x07\x00\x06\x00\x08\x00\x07\x00\x09\x00\x0e\x00\x0b\x00\x02\x00\x03\x00\x02\x00\x06\x00\x0e\x00\x07\x00\x09\x00\x06\x00\x02\x00\x0e\x00\x08\x00\x04\x00\xff\xff\x0e\x00\xff\xff\x0c\x00\x0b\x00\x02\x00\x0e\x00\xff\xff\x0b\x00\xff\xff\x0e\x00\x0c\x00\x0e\x00\xff\xff\x0e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x28\x00\x24\x00\x17\x00\x17\x00\x1c\x00\x17\x00\x17\x00\x1c\x00\x1d\x00\x21\x00\x25\x00\x1d\x00\x21\x00\x0d\x00\xff\xff\xff\xff\x0d\x00\xdf\xff\x25\x00\xe2\xff\x2d\x00\xdf\xff\x24\x00\x17\x00\x13\x00\x26\x00\x2a\x00\xdf\xff\xe2\xff\x18\x00\x21\x00\x25\x00\x18\x00\x14\x00\x2e\x00\x10\x00\x1a\x00\x19\x00\x10\x00\x1a\x00\x11\x00\x32\x00\x10\x00\x11\x00\x35\x00\x1d\x00\x0d\x00\x11\x00\x12\x00\x0e\x00\x11\x00\x31\x00\x27\x00\x22\x00\x15\x00\x0d\x00\x34\x00\x1f\x00\x0e\x00\x0d\x00\x2b\x00\x37\x00\x0e\x00\x1e\x00\x2c\x00\x27\x00\x22\x00\x1f\x00\x0d\x00\x35\x00\x1f\x00\x0e\x00\xff\xff\x0f\x00\x21\x00\x22\x00\x17\x00\x17\x00\xff\xff\x1f\x00\x0e\x00\x2a\x00\x17\x00\xff\xff\xdf\xff\xdf\xff\x00\x00\xff\xff\x00\x00\x0d\x00\x30\x00\x17\x00\xff\xff\x00\x00\x31\x00\x00\x00\xff\xff\x0d\x00\xdf\xff\x00\x00\xe2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (11, 34) [
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34)
	]

happy_n_terms = 15 :: Int
happy_n_nonterms = 12 :: Int

happyReduce_11 = happySpecReduce_1  0# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn14
		 (Ident happy_var_1
	)}

happyReduce_12 = happySpecReduce_1  1# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (Grammar (reverse happy_var_1)
	)}

happyReduce_13 = happySpecReduce_1  2# happyReduction_13
happyReduction_13 happy_x_1
	 =  happyIn16
		 (Header
	)

happyReduce_14 = happySpecReduce_2  2# happyReduction_14
happyReduction_14 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (Root happy_var_2
	)}

happyReduce_15 = happySpecReduce_3  2# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (Rule happy_var_1 happy_var_3
	)}}

happyReduce_16 = happySpecReduce_1  3# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (Internal happy_var_1
	)}

happyReduce_17 = happySpecReduce_2  3# happyReduction_17
happyReduction_17 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (Public happy_var_2
	)}

happyReduce_18 = happySpecReduce_1  4# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (Fragment (reverse happy_var_1)
	)}

happyReduce_19 = happySpecReduce_1  5# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (Terminal happy_var_1
	)}

happyReduce_20 = happySpecReduce_1  5# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (NonTerminal happy_var_1
	)}

happyReduce_21 = happySpecReduce_3  5# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (Option happy_var_2
	)}

happyReduce_22 = happySpecReduce_3  5# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (Group happy_var_2
	)}

happyReduce_23 = happySpecReduce_1  6# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (Choice (reverse happy_var_1)
	)}

happyReduce_24 = happySpecReduce_2  7# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn21
		 (Var happy_var_2
	)}

happyReduce_25 = happySpecReduce_0  8# happyReduction_25
happyReduction_25  =  happyIn22
		 ([]
	)

happyReduce_26 = happySpecReduce_3  8# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn22
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_27 = happySpecReduce_0  9# happyReduction_27
happyReduction_27  =  happyIn23
		 ([]
	)

happyReduce_28 = happySpecReduce_2  9# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_29 = happySpecReduce_0  10# happyReduction_29
happyReduction_29  =  happyIn24
		 ([]
	)

happyReduce_30 = happySpecReduce_1  10# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((:[]) happy_var_1
	)}

happyReduce_31 = happySpecReduce_3  10# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_32 = happySpecReduce_0  11# happyReduction_32
happyReduction_32  =  happyIn25
		 ([]
	)

happyReduce_33 = happySpecReduce_1  11# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 ((:[]) happy_var_1
	)}

happyReduce_34 = happySpecReduce_3  11# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 14# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TV happy_dollar_dollar) -> cont 12#;
	_ -> cont 13#;
	_ -> happyError' (tk:tks)
	}

happyError_ 14# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut15 x))

pDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut16 x))

pLHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut17 x))

pFragment tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut18 x))

pItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut19 x))

pChoice tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut20 x))

pVar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut21 x))

pListDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut22 x))

pListItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut23 x))

pListFragment tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut24 x))

pListChoice tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut25 x))

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n Happy_GHC_Exts.<# (0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where (new_state) = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where (off)    = indexShortOffAddr happyActOffsets st
         (off_i)  = (off Happy_GHC_Exts.+# i)
	 check  = if (off_i Happy_GHC_Exts.>=# (0# :: Happy_GHC_Exts.Int#))
			then (indexShortOffAddr happyCheck off_i Happy_GHC_Exts.==#  i)
			else False
         (action)
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 130 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
	Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 163 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             (off) = indexShortOffAddr happyGotoOffsets st1
             (off_i) = (off Happy_GHC_Exts.+# nt)
             (new_state) = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where (off) = indexShortOffAddr happyGotoOffsets st
         (off_i) = (off Happy_GHC_Exts.+# nt)
         (new_state) = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
