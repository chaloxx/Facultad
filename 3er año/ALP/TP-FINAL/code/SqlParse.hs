{-# OPTIONS_GHC -w #-}
module SqlParse (sqlParse) where
import Parsing (parse,identifier,integer,char,many,letter,alphanum,string2)
import AST
import Data.Char
import ParseResult
import qualified Data.HashMap.Strict as H
import qualified Avl
import Error (errorComOpen,errorComClose)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t9 t10 t11 t12 t13 t14 t15
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (SQL)
	| HappyAbsSyn5 (ManUsers)
	| HappyAbsSyn6 (DML)
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 ([Args])
	| HappyAbsSyn17 (Args)
	| HappyAbsSyn21 (BoolExp)
	| HappyAbsSyn27 (Aggregate)
	| HappyAbsSyn28 (O)
	| HappyAbsSyn29 (Avl.AVL [Args])
	| HappyAbsSyn31 (([String],[Args]))
	| HappyAbsSyn32 (DDL)
	| HappyAbsSyn33 ([CArgs])
	| HappyAbsSyn34 (CArgs)
	| HappyAbsSyn35 ([String])
	| HappyAbsSyn36 (RefOption)
	| HappyAbsSyn38 (Type)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,632) ([0,0,960,0,16,956,30,0,0,120,0,2,0,0,0,0,0,0,0,0,0,0,0,896,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,16,0,0,0,0,0,0,2,0,0,0,0,0,49951,8,1,0,0,2048,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,4096,0,0,0,0,0,0,512,0,0,0,0,0,0,64,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,128,0,0,0,0,0,0,16,0,0,0,0,0,0,2,0,0,0,0,0,16384,0,0,0,0,0,0,2048,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,8,4,0,0,15,16384,61440,30734,0,0,0,0,4096,0,0,0,0,16384,15,1040,0,0,0,0,0,0,49408,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,512,0,0,0,0,0,0,64,0,0,0,0,0,0,8,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,32768,0,34366,16,2,0,0,0,0,32768,0,0,0,0,0,63488,16920,2048,0,0,0,0,7936,2115,256,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,1,0,0,0,0,0,0,0,32,0,0,0,0,128,0,4,0,0,0,0,16,32768,0,0,0,0,0,2,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,24,0,0,0,32768,34367,16,3,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,7808,8192,8,0,0,0,0,0,0,8,0,0,0,0,0,4096,0,0,0,0,0,0,2560,30,0,0,0,0,0,3072,0,0,0,0,0,0,384,0,0,0,0,0,0,48,0,0,0,0,0,0,6,0,0,0,0,0,49152,0,0,0,0,0,0,2048,0,0,0,0,0,31744,8460,1024,0,0,0,0,36736,1057,128,0,0,0,0,12784,132,16,0,0,0,0,34366,16,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1056,0,0,0,0,0,16256,4230,768,0,0,0,0,32768,16,0,0,0,0,0,6394,66,12,0,0,0,0,16896,0,0,0,0,0,0,0,8192,0,0,0,0,31744,8460,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,1,0,0,0,0,64,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,4096,2048,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,49152,8,0,0,0,0,0,0,0,0,0,0,0,1,2048,0,0,0,0,0,0,2048,0,0,0,0,0,0,49408,3,0,0,0,0,0,0,0,0,0,0,50176,2048,0,0,0,0,0,0,0,0,0,0,0,0,7168,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,384,33280,0,0,0,0,0,1592,64,0,0,0,0,0,0,0,0,0,0,0,0,224,0,0,0,0,0,0,32,0,0,0,0,0,0,16384,0,0,0,0,0,0,3199,33,6,0,0,8192,32768,8591,32772,0,0,0,0,0,12288,0,0,0,0,15360,16384,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,48,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,4,0,0,0,0,0,2048,0,0,0,0,0,0,4096,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,0,0,32768,0,0,0,0,0,0,0,1,0,0,0,0,0,512,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,256,0,0,0,0,0,16,0,0,0,0,0,32768,1,0,0,0,0,0,0,49152,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,57344,8591,49156,0,0,0,0,64512,33841,6144,0,0,0,0,15872,4230,768,0,0,0,0,51184,528,96,0,0,0,0,0,2,0,0,0,0,0,16384,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1056,0,0,0,0,0,0,4,0,0,0,0,0,0,16,0,0,0,0,0,24576,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,256,0,8,0,0,0,0,0,0,0,64,0,0,0,0,12784,132,24,0,0,0,0,34366,16,3,0,0,0,49152,4295,24578,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,3196,33,6,0,0,0,32768,8591,49156,0,0,0,0,61440,33841,6144,0,0,0,0,0,0,0,0,0,0,0,51152,528,96,0,0,0,0,6394,66,12,0,0,0,0,16384,0,0,0,0,0,28,128,0,0,0,0,0,0,256,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,8,0,0,0,0,0,0,0,16384,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,1,8,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,1,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,24576,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,3,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,56,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_sql","SQL","MANUSERS","DML","Query","Query0","Query1","Query2","Query3","Query4","Query5","Query6","Query7","ArgS","Exp","IntExp","ArgF","Fields","BoolExpW","BoolExpH","Value","ValueH","ValueW","Var","Aggregate","Order","TreeListArgs","ListArgs","ToUpdate","DDL","LCArgs","CArgs","FieldList2","DelReferenceOption","UpdReferenceOption","TYPE","INSERT","DELETE","UPDATE","SELECT","FROM","';'","WHERE","GROUPBY","HAVING","ORDERBY","UNION","DIFF","INTERSECT","AND","OR","'='","'>'","'<'","LIKE","EXIST","NOT","Sum","Count","Avg","Min","Max","LIMIT","Asc","Desc","ALL","'('","')'","','","AS","SET","FIELD","DISTINCT","IN","'.'","'+'","'-'","'*'","'/'","NEG","CTABLE","CBASE","DTABLE","DBASE","PKEY","USE","SHOWB","SHOWT","STR","NUM","NULL","INT","FLOAT","STRING","BOOL","SRC","CUSER","DUSER","SUSER","FKEY","REFERENCE","DEL","UPD","RESTRICTED","CASCADES","NULLIFIES","%eof"]
        bit_start = st * 109
        bit_end = (st + 1) * 109
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..108]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (39) = happyShift action_6
action_0 (40) = happyShift action_7
action_0 (41) = happyShift action_8
action_0 (42) = happyShift action_9
action_0 (69) = happyShift action_10
action_0 (83) = happyShift action_14
action_0 (84) = happyShift action_15
action_0 (85) = happyShift action_16
action_0 (86) = happyShift action_17
action_0 (88) = happyShift action_18
action_0 (89) = happyShift action_19
action_0 (90) = happyShift action_20
action_0 (98) = happyShift action_21
action_0 (99) = happyShift action_22
action_0 (100) = happyShift action_23
action_0 (101) = happyShift action_24
action_0 (4) = happyGoto action_11
action_0 (5) = happyGoto action_12
action_0 (6) = happyGoto action_2
action_0 (7) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 (9) = happyGoto action_5
action_0 (32) = happyGoto action_13
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (39) = happyShift action_6
action_1 (40) = happyShift action_7
action_1 (41) = happyShift action_8
action_1 (42) = happyShift action_9
action_1 (69) = happyShift action_10
action_1 (6) = happyGoto action_2
action_1 (7) = happyGoto action_3
action_1 (8) = happyGoto action_4
action_1 (9) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (49) = happyShift action_54
action_3 (50) = happyShift action_55
action_3 (51) = happyShift action_56
action_3 _ = happyReduce_12

action_4 _ = happyReduce_16

action_5 _ = happyReduce_17

action_6 (74) = happyShift action_53
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (74) = happyShift action_52
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (74) = happyShift action_51
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (60) = happyShift action_40
action_9 (61) = happyShift action_41
action_9 (62) = happyShift action_42
action_9 (63) = happyShift action_43
action_9 (64) = happyShift action_44
action_9 (68) = happyShift action_45
action_9 (69) = happyShift action_46
action_9 (74) = happyShift action_47
action_9 (75) = happyShift action_48
action_9 (79) = happyShift action_49
action_9 (92) = happyShift action_50
action_9 (16) = happyGoto action_36
action_9 (17) = happyGoto action_37
action_9 (18) = happyGoto action_38
action_9 (27) = happyGoto action_39
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (42) = happyShift action_9
action_10 (9) = happyGoto action_35
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (44) = happyShift action_34
action_11 (109) = happyAccept
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_3

action_13 _ = happyReduce_2

action_14 (74) = happyShift action_33
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (74) = happyShift action_32
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (74) = happyShift action_31
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (74) = happyShift action_30
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (74) = happyShift action_29
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_103

action_20 _ = happyReduce_104

action_21 (91) = happyShift action_28
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (74) = happyShift action_27
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (74) = happyShift action_26
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (74) = happyShift action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (74) = happyShift action_101
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (74) = happyShift action_100
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (74) = happyShift action_99
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_5

action_29 _ = happyReduce_102

action_30 _ = happyReduce_101

action_31 _ = happyReduce_99

action_32 _ = happyReduce_100

action_33 (74) = happyShift action_96
action_33 (87) = happyShift action_97
action_33 (102) = happyShift action_98
action_33 (33) = happyGoto action_94
action_33 (34) = happyGoto action_95
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (39) = happyShift action_6
action_34 (40) = happyShift action_7
action_34 (41) = happyShift action_8
action_34 (42) = happyShift action_9
action_34 (69) = happyShift action_10
action_34 (83) = happyShift action_14
action_34 (84) = happyShift action_15
action_34 (85) = happyShift action_16
action_34 (86) = happyShift action_17
action_34 (88) = happyShift action_18
action_34 (89) = happyShift action_19
action_34 (90) = happyShift action_20
action_34 (98) = happyShift action_21
action_34 (99) = happyShift action_22
action_34 (100) = happyShift action_23
action_34 (101) = happyShift action_24
action_34 (4) = happyGoto action_93
action_34 (5) = happyGoto action_12
action_34 (6) = happyGoto action_2
action_34 (7) = happyGoto action_3
action_34 (8) = happyGoto action_4
action_34 (9) = happyGoto action_5
action_34 (32) = happyGoto action_13
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (70) = happyShift action_92
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (43) = happyShift action_85
action_36 (45) = happyShift action_86
action_36 (46) = happyShift action_87
action_36 (47) = happyShift action_88
action_36 (48) = happyShift action_89
action_36 (65) = happyShift action_90
action_36 (71) = happyShift action_91
action_36 (10) = happyGoto action_79
action_36 (11) = happyGoto action_80
action_36 (12) = happyGoto action_81
action_36 (13) = happyGoto action_82
action_36 (14) = happyGoto action_83
action_36 (15) = happyGoto action_84
action_36 _ = happyReduce_32

action_37 (72) = happyShift action_74
action_37 (78) = happyShift action_75
action_37 (79) = happyShift action_76
action_37 (80) = happyShift action_77
action_37 (81) = happyShift action_78
action_37 _ = happyReduce_34

action_38 _ = happyReduce_40

action_39 _ = happyReduce_36

action_40 (69) = happyShift action_73
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (69) = happyShift action_72
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (69) = happyShift action_71
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (69) = happyShift action_70
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (69) = happyShift action_69
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_41

action_46 (42) = happyShift action_9
action_46 (60) = happyShift action_40
action_46 (61) = happyShift action_41
action_46 (62) = happyShift action_42
action_46 (63) = happyShift action_43
action_46 (64) = happyShift action_44
action_46 (68) = happyShift action_45
action_46 (69) = happyShift action_46
action_46 (74) = happyShift action_47
action_46 (79) = happyShift action_49
action_46 (92) = happyShift action_50
action_46 (9) = happyGoto action_67
action_46 (17) = happyGoto action_68
action_46 (18) = happyGoto action_38
action_46 (27) = happyGoto action_39
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (77) = happyShift action_66
action_47 _ = happyReduce_35

action_48 (60) = happyShift action_40
action_48 (61) = happyShift action_41
action_48 (62) = happyShift action_42
action_48 (63) = happyShift action_43
action_48 (64) = happyShift action_44
action_48 (68) = happyShift action_45
action_48 (69) = happyShift action_46
action_48 (74) = happyShift action_47
action_48 (79) = happyShift action_49
action_48 (92) = happyShift action_50
action_48 (16) = happyGoto action_65
action_48 (17) = happyGoto action_37
action_48 (18) = happyGoto action_38
action_48 (27) = happyGoto action_39
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (60) = happyShift action_40
action_49 (61) = happyShift action_41
action_49 (62) = happyShift action_42
action_49 (63) = happyShift action_43
action_49 (64) = happyShift action_44
action_49 (68) = happyShift action_45
action_49 (69) = happyShift action_46
action_49 (74) = happyShift action_47
action_49 (79) = happyShift action_49
action_49 (92) = happyShift action_50
action_49 (17) = happyGoto action_64
action_49 (18) = happyGoto action_38
action_49 (27) = happyGoto action_39
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_48

action_51 (73) = happyShift action_63
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (45) = happyShift action_62
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (69) = happyShift action_61
action_53 (29) = happyGoto action_60
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (42) = happyShift action_9
action_54 (69) = happyShift action_10
action_54 (7) = happyGoto action_59
action_54 (8) = happyGoto action_4
action_54 (9) = happyGoto action_5
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (42) = happyShift action_9
action_55 (69) = happyShift action_10
action_55 (7) = happyGoto action_58
action_55 (8) = happyGoto action_4
action_55 (9) = happyGoto action_5
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (42) = happyShift action_9
action_56 (69) = happyShift action_10
action_56 (7) = happyGoto action_57
action_56 (8) = happyGoto action_4
action_56 (9) = happyGoto action_5
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (49) = happyShift action_54
action_57 (50) = happyShift action_55
action_57 (51) = happyShift action_56
action_57 _ = happyReduce_15

action_58 (49) = happyShift action_54
action_58 (50) = happyShift action_55
action_58 (51) = happyShift action_56
action_58 _ = happyReduce_14

action_59 (49) = happyShift action_54
action_59 (50) = happyShift action_55
action_59 (51) = happyShift action_56
action_59 _ = happyReduce_13

action_60 (71) = happyShift action_160
action_60 _ = happyReduce_9

action_61 (91) = happyShift action_158
action_61 (92) = happyShift action_159
action_61 (30) = happyGoto action_157
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (58) = happyShift action_130
action_62 (59) = happyShift action_131
action_62 (60) = happyShift action_40
action_62 (61) = happyShift action_41
action_62 (62) = happyShift action_42
action_62 (63) = happyShift action_43
action_62 (64) = happyShift action_44
action_62 (68) = happyShift action_45
action_62 (69) = happyShift action_132
action_62 (74) = happyShift action_133
action_62 (79) = happyShift action_49
action_62 (91) = happyShift action_124
action_62 (92) = happyShift action_50
action_62 (17) = happyGoto action_117
action_62 (18) = happyGoto action_118
action_62 (21) = happyGoto action_156
action_62 (23) = happyGoto action_127
action_62 (25) = happyGoto action_128
action_62 (26) = happyGoto action_129
action_62 (27) = happyGoto action_39
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (74) = happyShift action_155
action_63 (31) = happyGoto action_154
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_47

action_65 (43) = happyShift action_85
action_65 (45) = happyShift action_86
action_65 (46) = happyShift action_87
action_65 (47) = happyShift action_88
action_65 (48) = happyShift action_89
action_65 (65) = happyShift action_90
action_65 (71) = happyShift action_91
action_65 (10) = happyGoto action_153
action_65 (11) = happyGoto action_80
action_65 (12) = happyGoto action_81
action_65 (13) = happyGoto action_82
action_65 (14) = happyGoto action_83
action_65 (15) = happyGoto action_84
action_65 _ = happyReduce_32

action_66 (74) = happyShift action_152
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (70) = happyShift action_151
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (70) = happyShift action_150
action_68 (72) = happyShift action_74
action_68 (78) = happyShift action_75
action_68 (79) = happyShift action_76
action_68 (80) = happyShift action_77
action_68 (81) = happyShift action_78
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (74) = happyShift action_148
action_69 (75) = happyShift action_149
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (74) = happyShift action_146
action_70 (75) = happyShift action_147
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (74) = happyShift action_144
action_71 (75) = happyShift action_145
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (74) = happyShift action_142
action_72 (75) = happyShift action_143
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (74) = happyShift action_140
action_73 (75) = happyShift action_141
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (74) = happyShift action_139
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (60) = happyShift action_40
action_75 (61) = happyShift action_41
action_75 (62) = happyShift action_42
action_75 (63) = happyShift action_43
action_75 (64) = happyShift action_44
action_75 (68) = happyShift action_45
action_75 (69) = happyShift action_46
action_75 (74) = happyShift action_47
action_75 (79) = happyShift action_49
action_75 (92) = happyShift action_50
action_75 (17) = happyGoto action_138
action_75 (18) = happyGoto action_38
action_75 (27) = happyGoto action_39
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (60) = happyShift action_40
action_76 (61) = happyShift action_41
action_76 (62) = happyShift action_42
action_76 (63) = happyShift action_43
action_76 (64) = happyShift action_44
action_76 (68) = happyShift action_45
action_76 (69) = happyShift action_46
action_76 (74) = happyShift action_47
action_76 (79) = happyShift action_49
action_76 (92) = happyShift action_50
action_76 (17) = happyGoto action_137
action_76 (18) = happyGoto action_38
action_76 (27) = happyGoto action_39
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (60) = happyShift action_40
action_77 (61) = happyShift action_41
action_77 (62) = happyShift action_42
action_77 (63) = happyShift action_43
action_77 (64) = happyShift action_44
action_77 (68) = happyShift action_45
action_77 (69) = happyShift action_46
action_77 (74) = happyShift action_47
action_77 (79) = happyShift action_49
action_77 (92) = happyShift action_50
action_77 (17) = happyGoto action_136
action_77 (18) = happyGoto action_38
action_77 (27) = happyGoto action_39
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (60) = happyShift action_40
action_78 (61) = happyShift action_41
action_78 (62) = happyShift action_42
action_78 (63) = happyShift action_43
action_78 (64) = happyShift action_44
action_78 (68) = happyShift action_45
action_78 (69) = happyShift action_46
action_78 (74) = happyShift action_47
action_78 (79) = happyShift action_49
action_78 (92) = happyShift action_50
action_78 (17) = happyGoto action_135
action_78 (18) = happyGoto action_38
action_78 (27) = happyGoto action_39
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_19

action_80 _ = happyReduce_22

action_81 _ = happyReduce_24

action_82 _ = happyReduce_26

action_83 _ = happyReduce_28

action_84 _ = happyReduce_30

action_85 (69) = happyShift action_115
action_85 (74) = happyShift action_116
action_85 (19) = happyGoto action_134
action_85 (20) = happyGoto action_114
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (58) = happyShift action_130
action_86 (59) = happyShift action_131
action_86 (60) = happyShift action_40
action_86 (61) = happyShift action_41
action_86 (62) = happyShift action_42
action_86 (63) = happyShift action_43
action_86 (64) = happyShift action_44
action_86 (68) = happyShift action_45
action_86 (69) = happyShift action_132
action_86 (74) = happyShift action_133
action_86 (79) = happyShift action_49
action_86 (91) = happyShift action_124
action_86 (92) = happyShift action_50
action_86 (17) = happyGoto action_117
action_86 (18) = happyGoto action_118
action_86 (21) = happyGoto action_126
action_86 (23) = happyGoto action_127
action_86 (25) = happyGoto action_128
action_86 (26) = happyGoto action_129
action_86 (27) = happyGoto action_39
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (69) = happyShift action_115
action_87 (74) = happyShift action_116
action_87 (19) = happyGoto action_125
action_87 (20) = happyGoto action_114
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (58) = happyShift action_123
action_88 (60) = happyShift action_40
action_88 (61) = happyShift action_41
action_88 (62) = happyShift action_42
action_88 (63) = happyShift action_43
action_88 (64) = happyShift action_44
action_88 (68) = happyShift action_45
action_88 (69) = happyShift action_46
action_88 (74) = happyShift action_47
action_88 (79) = happyShift action_49
action_88 (91) = happyShift action_124
action_88 (92) = happyShift action_50
action_88 (17) = happyGoto action_117
action_88 (18) = happyGoto action_118
action_88 (22) = happyGoto action_119
action_88 (23) = happyGoto action_120
action_88 (24) = happyGoto action_121
action_88 (27) = happyGoto action_122
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (69) = happyShift action_115
action_89 (74) = happyShift action_116
action_89 (19) = happyGoto action_113
action_89 (20) = happyGoto action_114
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (92) = happyShift action_112
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (60) = happyShift action_40
action_91 (61) = happyShift action_41
action_91 (62) = happyShift action_42
action_91 (63) = happyShift action_43
action_91 (64) = happyShift action_44
action_91 (68) = happyShift action_45
action_91 (69) = happyShift action_46
action_91 (74) = happyShift action_47
action_91 (79) = happyShift action_49
action_91 (92) = happyShift action_50
action_91 (16) = happyGoto action_111
action_91 (17) = happyGoto action_37
action_91 (18) = happyGoto action_38
action_91 (27) = happyGoto action_39
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_18

action_93 (44) = happyShift action_34
action_93 _ = happyReduce_4

action_94 (71) = happyShift action_110
action_94 _ = happyReduce_98

action_95 _ = happyReduce_105

action_96 (94) = happyShift action_106
action_96 (95) = happyShift action_107
action_96 (96) = happyShift action_108
action_96 (97) = happyShift action_109
action_96 (38) = happyGoto action_105
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (74) = happyShift action_104
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (74) = happyShift action_103
action_98 (35) = happyGoto action_102
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_6

action_100 _ = happyReduce_7

action_101 _ = happyReduce_8

action_102 (71) = happyShift action_209
action_102 (103) = happyShift action_210
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_111

action_104 _ = happyReduce_109

action_105 (93) = happyShift action_208
action_105 _ = happyReduce_108

action_106 _ = happyReduce_121

action_107 _ = happyReduce_122

action_108 _ = happyReduce_124

action_109 _ = happyReduce_123

action_110 (74) = happyShift action_96
action_110 (87) = happyShift action_97
action_110 (102) = happyShift action_98
action_110 (33) = happyGoto action_207
action_110 (34) = happyGoto action_95
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (71) = happyShift action_91
action_111 _ = happyReduce_33

action_112 _ = happyReduce_31

action_113 (66) = happyShift action_205
action_113 (67) = happyShift action_206
action_113 (71) = happyShift action_181
action_113 (28) = happyGoto action_204
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (71) = happyShift action_203
action_114 _ = happyReduce_49

action_115 (42) = happyShift action_9
action_115 (69) = happyShift action_10
action_115 (7) = happyGoto action_202
action_115 (8) = happyGoto action_4
action_115 (9) = happyGoto action_5
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (72) = happyShift action_201
action_116 _ = happyReduce_53

action_117 (72) = happyShift action_74
action_117 (78) = happyShift action_75
action_117 (79) = happyShift action_76
action_117 (80) = happyShift action_77
action_117 (81) = happyShift action_78
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (72) = happyReduce_40
action_118 (78) = happyReduce_40
action_118 (79) = happyReduce_40
action_118 (80) = happyReduce_40
action_118 (81) = happyReduce_40
action_118 _ = happyReduce_72

action_119 (48) = happyShift action_89
action_119 (52) = happyShift action_199
action_119 (53) = happyShift action_200
action_119 (65) = happyShift action_90
action_119 (14) = happyGoto action_198
action_119 (15) = happyGoto action_84
action_119 _ = happyReduce_32

action_120 _ = happyReduce_73

action_121 (54) = happyShift action_195
action_121 (55) = happyShift action_196
action_121 (56) = happyShift action_197
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (72) = happyReduce_36
action_122 (78) = happyReduce_36
action_122 (79) = happyReduce_36
action_122 (80) = happyReduce_36
action_122 (81) = happyReduce_36
action_122 _ = happyReduce_74

action_123 (69) = happyShift action_194
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_71

action_125 (47) = happyShift action_88
action_125 (48) = happyShift action_89
action_125 (65) = happyShift action_90
action_125 (71) = happyShift action_181
action_125 (13) = happyGoto action_193
action_125 (14) = happyGoto action_83
action_125 (15) = happyGoto action_84
action_125 _ = happyReduce_32

action_126 (46) = happyShift action_87
action_126 (47) = happyShift action_88
action_126 (48) = happyShift action_89
action_126 (52) = happyShift action_164
action_126 (53) = happyShift action_165
action_126 (65) = happyShift action_90
action_126 (12) = happyGoto action_192
action_126 (13) = happyGoto action_82
action_126 (14) = happyGoto action_83
action_126 (15) = happyGoto action_84
action_126 _ = happyReduce_32

action_127 _ = happyReduce_76

action_128 (54) = happyShift action_189
action_128 (55) = happyShift action_190
action_128 (56) = happyShift action_191
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (57) = happyShift action_188
action_129 _ = happyReduce_75

action_130 (69) = happyShift action_187
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (58) = happyShift action_130
action_131 (59) = happyShift action_131
action_131 (60) = happyShift action_40
action_131 (61) = happyShift action_41
action_131 (62) = happyShift action_42
action_131 (63) = happyShift action_43
action_131 (64) = happyShift action_44
action_131 (68) = happyShift action_45
action_131 (69) = happyShift action_132
action_131 (74) = happyShift action_133
action_131 (79) = happyShift action_49
action_131 (91) = happyShift action_124
action_131 (92) = happyShift action_50
action_131 (17) = happyGoto action_117
action_131 (18) = happyGoto action_118
action_131 (21) = happyGoto action_186
action_131 (23) = happyGoto action_127
action_131 (25) = happyGoto action_128
action_131 (26) = happyGoto action_129
action_131 (27) = happyGoto action_39
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (42) = happyShift action_9
action_132 (60) = happyShift action_40
action_132 (61) = happyShift action_41
action_132 (62) = happyShift action_42
action_132 (63) = happyShift action_43
action_132 (64) = happyShift action_44
action_132 (68) = happyShift action_45
action_132 (69) = happyShift action_46
action_132 (74) = happyShift action_185
action_132 (79) = happyShift action_49
action_132 (92) = happyShift action_50
action_132 (9) = happyGoto action_67
action_132 (17) = happyGoto action_68
action_132 (18) = happyGoto action_38
action_132 (20) = happyGoto action_184
action_132 (27) = happyGoto action_39
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (72) = happyReduce_35
action_133 (76) = happyShift action_182
action_133 (77) = happyShift action_183
action_133 (78) = happyReduce_35
action_133 (79) = happyReduce_35
action_133 (80) = happyReduce_35
action_133 (81) = happyReduce_35
action_133 _ = happyReduce_77

action_134 (45) = happyShift action_86
action_134 (46) = happyShift action_87
action_134 (47) = happyShift action_88
action_134 (48) = happyShift action_89
action_134 (65) = happyShift action_90
action_134 (71) = happyShift action_181
action_134 (11) = happyGoto action_180
action_134 (12) = happyGoto action_81
action_134 (13) = happyGoto action_82
action_134 (14) = happyGoto action_83
action_134 (15) = happyGoto action_84
action_134 _ = happyReduce_32

action_135 _ = happyReduce_45

action_136 _ = happyReduce_44

action_137 (80) = happyShift action_77
action_137 (81) = happyShift action_78
action_137 _ = happyReduce_43

action_138 (80) = happyShift action_77
action_138 (81) = happyShift action_78
action_138 _ = happyReduce_42

action_139 _ = happyReduce_37

action_140 (70) = happyShift action_179
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (74) = happyShift action_178
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (70) = happyShift action_177
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (74) = happyShift action_176
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (70) = happyShift action_175
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (74) = happyShift action_174
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (70) = happyShift action_173
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (74) = happyShift action_172
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (70) = happyShift action_171
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (74) = happyShift action_170
action_149 _ = happyFail (happyExpListPerState 149)

action_150 _ = happyReduce_46

action_151 (72) = happyShift action_169
action_151 _ = happyFail (happyExpListPerState 151)

action_152 _ = happyReduce_39

action_153 _ = happyReduce_20

action_154 (45) = happyShift action_167
action_154 (71) = happyShift action_168
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (54) = happyShift action_166
action_155 _ = happyFail (happyExpListPerState 155)

action_156 (52) = happyShift action_164
action_156 (53) = happyShift action_165
action_156 _ = happyReduce_10

action_157 (70) = happyShift action_162
action_157 (71) = happyShift action_163
action_157 _ = happyFail (happyExpListPerState 157)

action_158 _ = happyReduce_93

action_159 _ = happyReduce_94

action_160 (69) = happyShift action_61
action_160 (29) = happyGoto action_161
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (71) = happyShift action_160
action_161 _ = happyReduce_92

action_162 _ = happyReduce_91

action_163 (91) = happyShift action_158
action_163 (92) = happyShift action_159
action_163 (30) = happyGoto action_246
action_163 _ = happyFail (happyExpListPerState 163)

action_164 (58) = happyShift action_130
action_164 (59) = happyShift action_131
action_164 (60) = happyShift action_40
action_164 (61) = happyShift action_41
action_164 (62) = happyShift action_42
action_164 (63) = happyShift action_43
action_164 (64) = happyShift action_44
action_164 (68) = happyShift action_45
action_164 (69) = happyShift action_132
action_164 (74) = happyShift action_133
action_164 (79) = happyShift action_49
action_164 (91) = happyShift action_124
action_164 (92) = happyShift action_50
action_164 (17) = happyGoto action_117
action_164 (18) = happyGoto action_118
action_164 (21) = happyGoto action_245
action_164 (23) = happyGoto action_127
action_164 (25) = happyGoto action_128
action_164 (26) = happyGoto action_129
action_164 (27) = happyGoto action_39
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (58) = happyShift action_130
action_165 (59) = happyShift action_131
action_165 (60) = happyShift action_40
action_165 (61) = happyShift action_41
action_165 (62) = happyShift action_42
action_165 (63) = happyShift action_43
action_165 (64) = happyShift action_44
action_165 (68) = happyShift action_45
action_165 (69) = happyShift action_132
action_165 (74) = happyShift action_133
action_165 (79) = happyShift action_49
action_165 (91) = happyShift action_124
action_165 (92) = happyShift action_50
action_165 (17) = happyGoto action_117
action_165 (18) = happyGoto action_118
action_165 (21) = happyGoto action_244
action_165 (23) = happyGoto action_127
action_165 (25) = happyGoto action_128
action_165 (26) = happyGoto action_129
action_165 (27) = happyGoto action_39
action_165 _ = happyFail (happyExpListPerState 165)

action_166 (60) = happyShift action_40
action_166 (61) = happyShift action_41
action_166 (62) = happyShift action_42
action_166 (63) = happyShift action_43
action_166 (64) = happyShift action_44
action_166 (68) = happyShift action_45
action_166 (69) = happyShift action_46
action_166 (74) = happyShift action_47
action_166 (79) = happyShift action_49
action_166 (91) = happyShift action_124
action_166 (92) = happyShift action_50
action_166 (17) = happyGoto action_117
action_166 (18) = happyGoto action_118
action_166 (23) = happyGoto action_243
action_166 (27) = happyGoto action_39
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (58) = happyShift action_130
action_167 (59) = happyShift action_131
action_167 (60) = happyShift action_40
action_167 (61) = happyShift action_41
action_167 (62) = happyShift action_42
action_167 (63) = happyShift action_43
action_167 (64) = happyShift action_44
action_167 (68) = happyShift action_45
action_167 (69) = happyShift action_132
action_167 (74) = happyShift action_133
action_167 (79) = happyShift action_49
action_167 (91) = happyShift action_124
action_167 (92) = happyShift action_50
action_167 (17) = happyGoto action_117
action_167 (18) = happyGoto action_118
action_167 (21) = happyGoto action_242
action_167 (23) = happyGoto action_127
action_167 (25) = happyGoto action_128
action_167 (26) = happyGoto action_129
action_167 (27) = happyGoto action_39
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (74) = happyShift action_155
action_168 (31) = happyGoto action_241
action_168 _ = happyFail (happyExpListPerState 168)

action_169 (74) = happyShift action_240
action_169 _ = happyFail (happyExpListPerState 169)

action_170 (70) = happyShift action_239
action_170 _ = happyFail (happyExpListPerState 170)

action_171 _ = happyReduce_87

action_172 (70) = happyShift action_238
action_172 _ = happyFail (happyExpListPerState 172)

action_173 _ = happyReduce_85

action_174 (70) = happyShift action_237
action_174 _ = happyFail (happyExpListPerState 174)

action_175 _ = happyReduce_83

action_176 (70) = happyShift action_236
action_176 _ = happyFail (happyExpListPerState 176)

action_177 _ = happyReduce_81

action_178 (70) = happyShift action_235
action_178 _ = happyFail (happyExpListPerState 178)

action_179 _ = happyReduce_79

action_180 _ = happyReduce_21

action_181 (69) = happyShift action_115
action_181 (74) = happyShift action_116
action_181 (19) = happyGoto action_234
action_181 (20) = happyGoto action_114
action_181 _ = happyFail (happyExpListPerState 181)

action_182 (69) = happyShift action_233
action_182 _ = happyFail (happyExpListPerState 182)

action_183 (74) = happyShift action_232
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (70) = happyShift action_231
action_184 (71) = happyShift action_203
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (70) = happyReduce_53
action_185 (72) = happyReduce_35
action_185 (77) = happyShift action_66
action_185 (78) = happyReduce_35
action_185 (79) = happyReduce_35
action_185 (80) = happyReduce_35
action_185 (81) = happyReduce_35
action_185 _ = happyReduce_53

action_186 _ = happyReduce_60

action_187 (42) = happyShift action_9
action_187 (69) = happyShift action_10
action_187 (7) = happyGoto action_230
action_187 (8) = happyGoto action_4
action_187 (9) = happyGoto action_5
action_187 _ = happyFail (happyExpListPerState 187)

action_188 (91) = happyShift action_229
action_188 _ = happyFail (happyExpListPerState 188)

action_189 (60) = happyShift action_40
action_189 (61) = happyShift action_41
action_189 (62) = happyShift action_42
action_189 (63) = happyShift action_43
action_189 (64) = happyShift action_44
action_189 (68) = happyShift action_45
action_189 (69) = happyShift action_46
action_189 (74) = happyShift action_226
action_189 (79) = happyShift action_49
action_189 (91) = happyShift action_124
action_189 (92) = happyShift action_50
action_189 (17) = happyGoto action_117
action_189 (18) = happyGoto action_118
action_189 (23) = happyGoto action_127
action_189 (25) = happyGoto action_228
action_189 (26) = happyGoto action_225
action_189 (27) = happyGoto action_39
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (60) = happyShift action_40
action_190 (61) = happyShift action_41
action_190 (62) = happyShift action_42
action_190 (63) = happyShift action_43
action_190 (64) = happyShift action_44
action_190 (68) = happyShift action_45
action_190 (69) = happyShift action_46
action_190 (74) = happyShift action_226
action_190 (79) = happyShift action_49
action_190 (91) = happyShift action_124
action_190 (92) = happyShift action_50
action_190 (17) = happyGoto action_117
action_190 (18) = happyGoto action_118
action_190 (23) = happyGoto action_127
action_190 (25) = happyGoto action_227
action_190 (26) = happyGoto action_225
action_190 (27) = happyGoto action_39
action_190 _ = happyFail (happyExpListPerState 190)

action_191 (60) = happyShift action_40
action_191 (61) = happyShift action_41
action_191 (62) = happyShift action_42
action_191 (63) = happyShift action_43
action_191 (64) = happyShift action_44
action_191 (68) = happyShift action_45
action_191 (69) = happyShift action_46
action_191 (74) = happyShift action_226
action_191 (79) = happyShift action_49
action_191 (91) = happyShift action_124
action_191 (92) = happyShift action_50
action_191 (17) = happyGoto action_117
action_191 (18) = happyGoto action_118
action_191 (23) = happyGoto action_127
action_191 (25) = happyGoto action_224
action_191 (26) = happyGoto action_225
action_191 (27) = happyGoto action_39
action_191 _ = happyFail (happyExpListPerState 191)

action_192 _ = happyReduce_23

action_193 _ = happyReduce_25

action_194 (42) = happyShift action_9
action_194 (9) = happyGoto action_223
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (60) = happyShift action_40
action_195 (61) = happyShift action_41
action_195 (62) = happyShift action_42
action_195 (63) = happyShift action_43
action_195 (64) = happyShift action_44
action_195 (68) = happyShift action_45
action_195 (69) = happyShift action_46
action_195 (74) = happyShift action_47
action_195 (79) = happyShift action_49
action_195 (91) = happyShift action_124
action_195 (92) = happyShift action_50
action_195 (17) = happyGoto action_117
action_195 (18) = happyGoto action_118
action_195 (23) = happyGoto action_120
action_195 (24) = happyGoto action_222
action_195 (27) = happyGoto action_122
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (60) = happyShift action_40
action_196 (61) = happyShift action_41
action_196 (62) = happyShift action_42
action_196 (63) = happyShift action_43
action_196 (64) = happyShift action_44
action_196 (68) = happyShift action_45
action_196 (69) = happyShift action_46
action_196 (74) = happyShift action_47
action_196 (79) = happyShift action_49
action_196 (91) = happyShift action_124
action_196 (92) = happyShift action_50
action_196 (17) = happyGoto action_117
action_196 (18) = happyGoto action_118
action_196 (23) = happyGoto action_120
action_196 (24) = happyGoto action_221
action_196 (27) = happyGoto action_122
action_196 _ = happyFail (happyExpListPerState 196)

action_197 (60) = happyShift action_40
action_197 (61) = happyShift action_41
action_197 (62) = happyShift action_42
action_197 (63) = happyShift action_43
action_197 (64) = happyShift action_44
action_197 (68) = happyShift action_45
action_197 (69) = happyShift action_46
action_197 (74) = happyShift action_47
action_197 (79) = happyShift action_49
action_197 (91) = happyShift action_124
action_197 (92) = happyShift action_50
action_197 (17) = happyGoto action_117
action_197 (18) = happyGoto action_118
action_197 (23) = happyGoto action_120
action_197 (24) = happyGoto action_220
action_197 (27) = happyGoto action_122
action_197 _ = happyFail (happyExpListPerState 197)

action_198 _ = happyReduce_27

action_199 (58) = happyShift action_123
action_199 (60) = happyShift action_40
action_199 (61) = happyShift action_41
action_199 (62) = happyShift action_42
action_199 (63) = happyShift action_43
action_199 (64) = happyShift action_44
action_199 (68) = happyShift action_45
action_199 (69) = happyShift action_46
action_199 (74) = happyShift action_47
action_199 (79) = happyShift action_49
action_199 (91) = happyShift action_124
action_199 (92) = happyShift action_50
action_199 (17) = happyGoto action_117
action_199 (18) = happyGoto action_118
action_199 (22) = happyGoto action_219
action_199 (23) = happyGoto action_120
action_199 (24) = happyGoto action_121
action_199 (27) = happyGoto action_122
action_199 _ = happyFail (happyExpListPerState 199)

action_200 (58) = happyShift action_123
action_200 (60) = happyShift action_40
action_200 (61) = happyShift action_41
action_200 (62) = happyShift action_42
action_200 (63) = happyShift action_43
action_200 (64) = happyShift action_44
action_200 (68) = happyShift action_45
action_200 (69) = happyShift action_46
action_200 (74) = happyShift action_47
action_200 (79) = happyShift action_49
action_200 (91) = happyShift action_124
action_200 (92) = happyShift action_50
action_200 (17) = happyGoto action_117
action_200 (18) = happyGoto action_118
action_200 (22) = happyGoto action_218
action_200 (23) = happyGoto action_120
action_200 (24) = happyGoto action_121
action_200 (27) = happyGoto action_122
action_200 _ = happyFail (happyExpListPerState 200)

action_201 (74) = happyShift action_217
action_201 _ = happyFail (happyExpListPerState 201)

action_202 (49) = happyShift action_54
action_202 (50) = happyShift action_55
action_202 (51) = happyShift action_56
action_202 (70) = happyShift action_216
action_202 _ = happyFail (happyExpListPerState 202)

action_203 (74) = happyShift action_215
action_203 (20) = happyGoto action_214
action_203 _ = happyFail (happyExpListPerState 203)

action_204 (65) = happyShift action_90
action_204 (15) = happyGoto action_213
action_204 _ = happyReduce_32

action_205 _ = happyReduce_89

action_206 _ = happyReduce_90

action_207 (71) = happyShift action_110
action_207 _ = happyReduce_106

action_208 _ = happyReduce_107

action_209 (74) = happyShift action_103
action_209 (35) = happyGoto action_212
action_209 _ = happyFail (happyExpListPerState 209)

action_210 (74) = happyShift action_211
action_210 _ = happyFail (happyExpListPerState 210)

action_211 (104) = happyShift action_253
action_211 (36) = happyGoto action_252
action_211 _ = happyReduce_113

action_212 (71) = happyShift action_209
action_212 _ = happyReduce_112

action_213 _ = happyReduce_29

action_214 (71) = happyShift action_203
action_214 _ = happyReduce_54

action_215 _ = happyReduce_53

action_216 (72) = happyShift action_251
action_216 _ = happyFail (happyExpListPerState 216)

action_217 _ = happyReduce_50

action_218 _ = happyReduce_66

action_219 _ = happyReduce_65

action_220 _ = happyReduce_69

action_221 _ = happyReduce_68

action_222 _ = happyReduce_67

action_223 (70) = happyShift action_250
action_223 _ = happyFail (happyExpListPerState 223)

action_224 _ = happyReduce_59

action_225 _ = happyReduce_75

action_226 (72) = happyReduce_35
action_226 (77) = happyShift action_183
action_226 (78) = happyReduce_35
action_226 (79) = happyReduce_35
action_226 (80) = happyReduce_35
action_226 (81) = happyReduce_35
action_226 _ = happyReduce_77

action_227 _ = happyReduce_58

action_228 _ = happyReduce_57

action_229 _ = happyReduce_62

action_230 (49) = happyShift action_54
action_230 (50) = happyShift action_55
action_230 (51) = happyShift action_56
action_230 (70) = happyShift action_249
action_230 _ = happyFail (happyExpListPerState 230)

action_231 (76) = happyShift action_248
action_231 _ = happyFail (happyExpListPerState 231)

action_232 (72) = happyReduce_39
action_232 (78) = happyReduce_39
action_232 (79) = happyReduce_39
action_232 (80) = happyReduce_39
action_232 (81) = happyReduce_39
action_232 _ = happyReduce_78

action_233 (91) = happyShift action_158
action_233 (92) = happyShift action_159
action_233 (30) = happyGoto action_247
action_233 _ = happyFail (happyExpListPerState 233)

action_234 (71) = happyShift action_181
action_234 _ = happyReduce_51

action_235 _ = happyReduce_80

action_236 _ = happyReduce_82

action_237 _ = happyReduce_84

action_238 _ = happyReduce_86

action_239 _ = happyReduce_88

action_240 _ = happyReduce_38

action_241 (71) = happyShift action_168
action_241 _ = happyReduce_97

action_242 (52) = happyShift action_164
action_242 (53) = happyShift action_165
action_242 _ = happyReduce_11

action_243 _ = happyReduce_96

action_244 _ = happyReduce_56

action_245 _ = happyReduce_55

action_246 (71) = happyShift action_163
action_246 _ = happyReduce_95

action_247 (70) = happyShift action_261
action_247 (71) = happyShift action_163
action_247 _ = happyFail (happyExpListPerState 247)

action_248 (69) = happyShift action_260
action_248 _ = happyFail (happyExpListPerState 248)

action_249 _ = happyReduce_61

action_250 _ = happyReduce_70

action_251 (74) = happyShift action_259
action_251 _ = happyFail (happyExpListPerState 251)

action_252 (105) = happyShift action_258
action_252 (37) = happyGoto action_257
action_252 _ = happyReduce_117

action_253 (106) = happyShift action_254
action_253 (107) = happyShift action_255
action_253 (108) = happyShift action_256
action_253 _ = happyFail (happyExpListPerState 253)

action_254 _ = happyReduce_114

action_255 _ = happyReduce_115

action_256 _ = happyReduce_116

action_257 _ = happyReduce_110

action_258 (106) = happyShift action_263
action_258 (107) = happyShift action_264
action_258 (108) = happyShift action_265
action_258 _ = happyFail (happyExpListPerState 258)

action_259 _ = happyReduce_52

action_260 (42) = happyShift action_9
action_260 (9) = happyGoto action_262
action_260 _ = happyFail (happyExpListPerState 260)

action_261 _ = happyReduce_64

action_262 (70) = happyShift action_266
action_262 _ = happyFail (happyExpListPerState 262)

action_263 _ = happyReduce_118

action_264 _ = happyReduce_119

action_265 _ = happyReduce_120

action_266 _ = happyReduce_63

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (S1 happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn4
		 (S2 happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (S3 happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  4 happyReduction_5
happyReduction_5 (HappyTerminal (TStr happy_var_2))
	_
	 =  HappyAbsSyn4
		 (Source happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyTerminal (TField happy_var_3))
	(HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn5
		 (CUser happy_var_2 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyTerminal (TField happy_var_3))
	(HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn5
		 (DUser happy_var_2 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyTerminal (TField happy_var_3))
	(HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn5
		 (SUser happy_var_2 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn6
		 (Insert happy_var_2 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 6 happyReduction_10
happyReduction_10 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TField happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Delete happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 6 6 happyReduction_11
happyReduction_11 ((HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TField happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Update happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Union happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Diff happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Intersect happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  7 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  8 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  8 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Select False happy_var_2 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 9 happyReduction_20
happyReduction_20 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Select True happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  10 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (From happy_var_2 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  10 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_3)
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Where happy_var_2 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (GroupBy happy_var_2 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  12 happyReduction_26
happyReduction_26 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  13 happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_3)
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Having happy_var_2 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  13 happyReduction_28
happyReduction_28 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 14 happyReduction_29
happyReduction_29 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (OrderBy happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_1  14 happyReduction_30
happyReduction_30 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  15 happyReduction_31
happyReduction_31 (HappyTerminal (TNum happy_var_2))
	_
	 =  HappyAbsSyn15
		 (Limit happy_var_2 End
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  15 happyReduction_32
happyReduction_32  =  HappyAbsSyn15
		 (End
	)

happyReduce_33 = happySpecReduce_3  16 happyReduction_33
happyReduction_33 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  16 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  17 happyReduction_35
happyReduction_35 (HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn17
		 (Field happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  17 happyReduction_36
happyReduction_36 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn17
		 (A2 happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  17 happyReduction_37
happyReduction_37 (HappyTerminal (TField happy_var_3))
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (As happy_var_1 (Field happy_var_3)
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 5 17 happyReduction_38
happyReduction_38 ((HappyTerminal (TField happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (As (Subquery happy_var_2) (Field happy_var_5)
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_3  17 happyReduction_39
happyReduction_39 (HappyTerminal (TField happy_var_3))
	_
	(HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn17
		 (Dot happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  17 happyReduction_40
happyReduction_40 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  17 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn17
		 (All
	)

happyReduce_42 = happySpecReduce_3  18 happyReduction_42
happyReduction_42 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  18 happyReduction_43
happyReduction_43 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  18 happyReduction_44
happyReduction_44 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Times happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  18 happyReduction_45
happyReduction_45 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Div happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  18 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (Brack happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  18 happyReduction_47
happyReduction_47 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (Negate happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  18 happyReduction_48
happyReduction_48 (HappyTerminal (TNum happy_var_1))
	 =  HappyAbsSyn17
		 (A3 happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  19 happyReduction_49
happyReduction_49 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  19 happyReduction_50
happyReduction_50 (HappyTerminal (TField happy_var_3))
	_
	(HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn16
		 ([As (Field happy_var_1) (Field happy_var_3)]
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  19 happyReduction_51
happyReduction_51 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 5 19 happyReduction_52
happyReduction_52 ((HappyTerminal (TField happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ([As (Subquery happy_var_2) (Field happy_var_5)]
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_1  20 happyReduction_53
happyReduction_53 (HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn16
		 ([Field happy_var_1]
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  20 happyReduction_54
happyReduction_54 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  21 happyReduction_55
happyReduction_55 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (And happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  21 happyReduction_56
happyReduction_56 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Or happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  21 happyReduction_57
happyReduction_57 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (Equal happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  21 happyReduction_58
happyReduction_58 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (Great happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  21 happyReduction_59
happyReduction_59 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (Less happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  21 happyReduction_60
happyReduction_60 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (Not happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 4 21 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Exist happy_var_3
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_3  21 happyReduction_62
happyReduction_62 (HappyTerminal (TStr happy_var_3))
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (Like happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happyReduce 7 21 happyReduction_63
happyReduction_63 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (InS happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 5 21 happyReduction_64
happyReduction_64 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TField happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (InV (Field happy_var_1) happy_var_4
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_3  22 happyReduction_65
happyReduction_65 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (And happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  22 happyReduction_66
happyReduction_66 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Or happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  22 happyReduction_67
happyReduction_67 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (Equal happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  22 happyReduction_68
happyReduction_68 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (Great happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  22 happyReduction_69
happyReduction_69 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (Less happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happyReduce 4 22 happyReduction_70
happyReduction_70 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Exist happy_var_3
	) `HappyStk` happyRest

happyReduce_71 = happySpecReduce_1  23 happyReduction_71
happyReduction_71 (HappyTerminal (TStr happy_var_1))
	 =  HappyAbsSyn17
		 (A1 happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  23 happyReduction_72
happyReduction_72 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  24 happyReduction_73
happyReduction_73 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  24 happyReduction_74
happyReduction_74 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn17
		 (A2 happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  25 happyReduction_75
happyReduction_75 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  25 happyReduction_76
happyReduction_76 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  26 happyReduction_77
happyReduction_77 (HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn17
		 (Field happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  26 happyReduction_78
happyReduction_78 (HappyTerminal (TField happy_var_3))
	_
	(HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn17
		 (Dot happy_var_1 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happyReduce 4 27 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyTerminal (TField happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Sum False happy_var_3
	) `HappyStk` happyRest

happyReduce_80 = happyReduce 5 27 happyReduction_80
happyReduction_80 (_ `HappyStk`
	(HappyTerminal (TField happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Sum True happy_var_4
	) `HappyStk` happyRest

happyReduce_81 = happyReduce 4 27 happyReduction_81
happyReduction_81 (_ `HappyStk`
	(HappyTerminal (TField happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Count False (A1 happy_var_3)
	) `HappyStk` happyRest

happyReduce_82 = happyReduce 5 27 happyReduction_82
happyReduction_82 (_ `HappyStk`
	(HappyTerminal (TField happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Count True (A1 happy_var_4)
	) `HappyStk` happyRest

happyReduce_83 = happyReduce 4 27 happyReduction_83
happyReduction_83 (_ `HappyStk`
	(HappyTerminal (TField happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Avg False happy_var_3
	) `HappyStk` happyRest

happyReduce_84 = happyReduce 5 27 happyReduction_84
happyReduction_84 (_ `HappyStk`
	(HappyTerminal (TField happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Avg True happy_var_4
	) `HappyStk` happyRest

happyReduce_85 = happyReduce 4 27 happyReduction_85
happyReduction_85 (_ `HappyStk`
	(HappyTerminal (TField happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Min False happy_var_3
	) `HappyStk` happyRest

happyReduce_86 = happyReduce 5 27 happyReduction_86
happyReduction_86 (_ `HappyStk`
	(HappyTerminal (TField happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Min True happy_var_4
	) `HappyStk` happyRest

happyReduce_87 = happyReduce 4 27 happyReduction_87
happyReduction_87 (_ `HappyStk`
	(HappyTerminal (TField happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Max False happy_var_3
	) `HappyStk` happyRest

happyReduce_88 = happyReduce 5 27 happyReduction_88
happyReduction_88 (_ `HappyStk`
	(HappyTerminal (TField happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Max True happy_var_4
	) `HappyStk` happyRest

happyReduce_89 = happySpecReduce_1  28 happyReduction_89
happyReduction_89 _
	 =  HappyAbsSyn28
		 (A
	)

happyReduce_90 = happySpecReduce_1  28 happyReduction_90
happyReduction_90 _
	 =  HappyAbsSyn28
		 (D
	)

happyReduce_91 = happySpecReduce_3  29 happyReduction_91
happyReduction_91 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (Avl.singletonT happy_var_2
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  29 happyReduction_92
happyReduction_92 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (Avl.join happy_var_1  happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  30 happyReduction_93
happyReduction_93 (HappyTerminal (TStr happy_var_1))
	 =  HappyAbsSyn16
		 ([A1 happy_var_1]
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  30 happyReduction_94
happyReduction_94 (HappyTerminal (TNum happy_var_1))
	 =  HappyAbsSyn16
		 ([A3 happy_var_1]
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  30 happyReduction_95
happyReduction_95 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_3  31 happyReduction_96
happyReduction_96 (HappyAbsSyn17  happy_var_3)
	_
	(HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn31
		 (([happy_var_1],[happy_var_3])
	)
happyReduction_96 _ _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_3  31 happyReduction_97
happyReduction_97 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (let ((k1,m1),(k2,m2)) = (happy_var_1,happy_var_3)
                                  in (k1 ++ k2, m1 ++ m2)
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_3  32 happyReduction_98
happyReduction_98 (HappyAbsSyn33  happy_var_3)
	(HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn32
		 (CTable happy_var_2 happy_var_3
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_2  32 happyReduction_99
happyReduction_99 (HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn32
		 (DTable happy_var_2
	)
happyReduction_99 _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_2  32 happyReduction_100
happyReduction_100 (HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn32
		 (CBase happy_var_2
	)
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_2  32 happyReduction_101
happyReduction_101 (HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn32
		 (DBase happy_var_2
	)
happyReduction_101 _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_2  32 happyReduction_102
happyReduction_102 (HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn32
		 (Use happy_var_2
	)
happyReduction_102 _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  32 happyReduction_103
happyReduction_103 _
	 =  HappyAbsSyn32
		 (ShowB
	)

happyReduce_104 = happySpecReduce_1  32 happyReduction_104
happyReduction_104 _
	 =  HappyAbsSyn32
		 (ShowT
	)

happyReduce_105 = happySpecReduce_1  33 happyReduction_105
happyReduction_105 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_3  33 happyReduction_106
happyReduction_106 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  34 happyReduction_107
happyReduction_107 _
	(HappyAbsSyn38  happy_var_2)
	(HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn34
		 (Col happy_var_1 happy_var_2 True
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_2  34 happyReduction_108
happyReduction_108 (HappyAbsSyn38  happy_var_2)
	(HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn34
		 (Col happy_var_1 happy_var_2 False
	)
happyReduction_108 _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_2  34 happyReduction_109
happyReduction_109 (HappyTerminal (TField happy_var_2))
	_
	 =  HappyAbsSyn34
		 (PKey happy_var_2
	)
happyReduction_109 _ _  = notHappyAtAll 

happyReduce_110 = happyReduce 6 34 happyReduction_110
happyReduction_110 ((HappyAbsSyn36  happy_var_6) `HappyStk`
	(HappyAbsSyn36  happy_var_5) `HappyStk`
	(HappyTerminal (TField happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (FKey happy_var_2 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_111 = happySpecReduce_1  35 happyReduction_111
happyReduction_111 (HappyTerminal (TField happy_var_1))
	 =  HappyAbsSyn35
		 ([happy_var_1]
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  35 happyReduction_112
happyReduction_112 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_0  36 happyReduction_113
happyReduction_113  =  HappyAbsSyn36
		 (Restricted
	)

happyReduce_114 = happySpecReduce_2  36 happyReduction_114
happyReduction_114 _
	_
	 =  HappyAbsSyn36
		 (Restricted
	)

happyReduce_115 = happySpecReduce_2  36 happyReduction_115
happyReduction_115 _
	_
	 =  HappyAbsSyn36
		 (Cascades
	)

happyReduce_116 = happySpecReduce_2  36 happyReduction_116
happyReduction_116 _
	_
	 =  HappyAbsSyn36
		 (Nullifies
	)

happyReduce_117 = happySpecReduce_0  37 happyReduction_117
happyReduction_117  =  HappyAbsSyn36
		 (Restricted
	)

happyReduce_118 = happySpecReduce_2  37 happyReduction_118
happyReduction_118 _
	_
	 =  HappyAbsSyn36
		 (Restricted
	)

happyReduce_119 = happySpecReduce_2  37 happyReduction_119
happyReduction_119 _
	_
	 =  HappyAbsSyn36
		 (Cascades
	)

happyReduce_120 = happySpecReduce_2  37 happyReduction_120
happyReduction_120 _
	_
	 =  HappyAbsSyn36
		 (Nullifies
	)

happyReduce_121 = happySpecReduce_1  38 happyReduction_121
happyReduction_121 _
	 =  HappyAbsSyn38
		 (Int
	)

happyReduce_122 = happySpecReduce_1  38 happyReduction_122
happyReduction_122 _
	 =  HappyAbsSyn38
		 (Float
	)

happyReduce_123 = happySpecReduce_1  38 happyReduction_123
happyReduction_123 _
	 =  HappyAbsSyn38
		 (Bool
	)

happyReduce_124 = happySpecReduce_1  38 happyReduction_124
happyReduction_124 _
	 =  HappyAbsSyn38
		 (String
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 109 109 tk (HappyState action) sts stk;
	TInsert -> cont 39;
	TDelete -> cont 40;
	TUpdate -> cont 41;
	TSelect -> cont 42;
	TFrom -> cont 43;
	TSemiColon -> cont 44;
	TWhere -> cont 45;
	TGroupBy -> cont 46;
	THaving -> cont 47;
	TOrderBy -> cont 48;
	TUnion -> cont 49;
	TDiff -> cont 50;
	TIntersect -> cont 51;
	TAnd -> cont 52;
	TOr -> cont 53;
	TEqual -> cont 54;
	TGreat -> cont 55;
	TLess -> cont 56;
	TLike -> cont 57;
	TExist -> cont 58;
	TNot -> cont 59;
	TSum -> cont 60;
	TCount -> cont 61;
	TAvg -> cont 62;
	TMin -> cont 63;
	TMax -> cont 64;
	TLimit -> cont 65;
	TAsc -> cont 66;
	TDesc -> cont 67;
	TAll -> cont 68;
	TOpen -> cont 69;
	TClose -> cont 70;
	TComa -> cont 71;
	TAs -> cont 72;
	TSet -> cont 73;
	TField happy_dollar_dollar -> cont 74;
	TDistinct -> cont 75;
	TIn -> cont 76;
	TDot -> cont 77;
	TPlus -> cont 78;
	TMinus -> cont 79;
	TTimes -> cont 80;
	TDiv -> cont 81;
	TNeg -> cont 82;
	TCTable -> cont 83;
	TCBase -> cont 84;
	TDTable -> cont 85;
	TDBase -> cont 86;
	TPkey -> cont 87;
	TUse -> cont 88;
	TShowB -> cont 89;
	TShowT -> cont 90;
	TStr happy_dollar_dollar -> cont 91;
	TNum happy_dollar_dollar -> cont 92;
	TNull -> cont 93;
	TInt -> cont 94;
	TFloat -> cont 95;
	TString -> cont 96;
	TBool -> cont 97;
	TSrc -> cont 98;
	TCUser -> cont 99;
	TDUser -> cont 100;
	TSUser -> cont 101;
	TFKey -> cont 102;
	TRef -> cont 103;
	TDel -> cont 104;
	TUpd -> cont 105;
	TRestricted -> cont 106;
	TCascades -> cont 107;
	TNullifies -> cont 108;
	_ -> happyError' (tk, [])
	})

happyError_ explist 109 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> P a
happyError' tk = (\(tokens, explist) -> happyError) tk
sql = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Token =   TCUser
             | TDUser
             | TSUser
             | TInsert
             | TDelete
             | TUpdate
             | TLimit
             | TSet
             | TSelect
             | TFrom
             | TWhere
             | TGroupBy
             | THaving
             | TOrderBy
             | TAnd
             | TOr
             | TEqual
             | TGreat
             | TLess
             | TLike
             | TIn
             | TNot
             | TExist
             | TSum
             | TCount
             | TMin
             | TMax
             | TDistinct
             | TAvg
             | TSemiColon
             | TField String
             | TAsc
             | TDesc
             | TAll
             | TComa
             | TOpen
             | TClose
             | TAs
             | TDot
             | TPlus
             | TMinus
             | TTimes
             | TDiv
             | TNeg
             | TEOF

             | TCTable
             | TDTable
             | TCBase
             | TDBase
             | TStr String
             | TNum Int
             | TDate String
             | TString
             | TFloat
             | TInt
             | TBool
             | TNull
             | TUse
             | TShowB
             | TShowT
             | TPkey
             | TSrc
             | TRef
             | TFKey
             | TDel
             | TUpd
             | TRestricted
             | TNullifies
             | TCascades

             | TUnion
             | TIntersect
             | TDiff



lexer cont  s = case s of
         [] -> cont TEOF []
         ('\n':xs) -> \(s1,s2) -> lexer cont xs (1 + s1,1)
         ('C':'R':'E':'A':'T':'E':' ':'U':'S':'E':'R':xs) -> \(s1,s2) ->  cont TCUser xs (s1,11 + s2)
         ('S':'E':'L':'E':'C':'T':' ':'U':'S':'E':'R':xs) -> \(s1,s2) ->  cont TSUser xs (s1,11 + s2)
         ('D':'E':'L':'E':'T':'E':' ':'U':'S':'E':'R':xs) -> \(s1,s2) ->  cont TDUser xs (s1,11 + s2)

         ('I':'N':'S':'E':'R':'T':xs) -> \(s1,s2) ->  cont TInsert xs (s1,6 + s2)
         ('D':'E':'L':'E':'T':'E':xs) -> \(s1,s2) ->  cont TDelete xs (s1,6 + s2)
         ('U':'P':'D':'A':'T':'E':xs) -> \(s1,s2) ->  cont TUpdate xs (s1,6 + s2)
         ('S':'E':'T':xs) -> \(s1,s2) -> cont TSet xs (s1,3 + s2)
         ('S':'E':'L':'E':'C':'T':xs) -> \(s1,s2) -> cont TSelect xs (s1,6 + s2)
         ('F':'R':'O':'M':xs) -> \(s1,s2) -> cont TFrom xs (s1,4 + s2)
         ('W':'H':'E':'R':'E':xs) -> \(s1,s2) -> cont TWhere xs (s1,5 + s2)
         ('G':'R':'O':'U':'P':' ':'B':'Y':xs) -> \(s1,s2) -> cont TGroupBy xs (s1,8 + s2)
         ('H':'A':'V':'I':'N':'G':xs) -> \(s1,s2) -> cont THaving xs (s1,6 + s2)
         ('O':'R':'D':'E':'R':' ':'B':'Y':xs) -> \(s1,s2) -> cont TGroupBy xs (s1,8 + s2)
         ('L':'I':'M':'I':'T':xs) -> \(s1,s2) -> cont TLimit xs (s1, 5 + s2)

         ('U':'N':'I':'O':'N':xs) -> \(s1,s2) -> cont TUnion xs (s1,5 + s2)
         ('I':'N':'T':'E':'R':'S':'E':'C':'T':xs) -> \(s1,s2) -> cont TIntersect xs (s1,9 + s2)
         ('D':'I':'F':'F':xs) -> \(s1,s2) -> cont TDiff xs (s1,5 + s2)

         ('A':'N':'D':xs) -> \(s1,s2) ->  cont TAnd xs (s1,3 + s2)
         ('O':'R':xs) ->  \(s1,s2) ->  cont TOr xs (s1,2 + s2)
         ('N':'O':'T':xs) -> \(s1,s2) ->  cont TNot xs (s1,3 + s2)
         ('L':'I':'K':'E':xs) -> \(s1,s2) ->  cont TLike xs (s1,4 + s2)
         ('E':'X':'I':'S':'T':xs) -> \(s1,s2) ->  cont TExist xs (s1,5 + s2)
         ('I':'N':xs) -> \(s1,s2) ->  cont TIn xs (s1,2 + s2)
         ('G':'R':'O':'U':'P':' ':'B':'Y':xs) -> \(s1,s2) ->  cont TGroupBy xs (s1,8 + s2)
         ('=':xs) -> \(s1,s2) ->  cont TEqual xs (s1,1 + s2)
         ('>':xs) -> \(s1,s2) ->  cont TGreat xs (s1,1 + s2)
         ('<':xs) -> \(s1,s2) ->  cont TLess xs (s1,1 + s2)

         ('C':'O':'U':'N':'T':xs) -> \(s1,s2) ->  cont TCount xs (s1,5 + s2)
         ('S':'U':'M':xs) -> \(s1,s2) ->  cont TSum xs (s1,3 + s2)
         ('A':'V':'G':xs) -> \(s1,s2) ->  cont TAvg xs (s1,3 + s2)
         ('M':'I':'N':xs) -> \(s1,s2) ->  cont TMin xs (s1,3 + s2)
         ('M':'A':'X':xs) -> \(s1,s2) ->  cont TMax xs (s1,3 + s2)
         ('D':'I':'S':'T':'I':'N':'C':'T':xs) -> \(s1,s2) ->  cont TDistinct xs (s1,8 + s2)
         ('A':'S':xs) -> \(s1,s2) ->  cont TAs xs (s1,2 + s2)
         ('A':'L':'L':xs) -> \(s1,s2) ->  cont TAll xs (s1,3 + s2)
         ('N':'E':'G':xs) -> \(s1,s2) ->  cont TNeg xs (s1,3 + s2)


         ('A':'s':'c':xs) -> \(s1,s2) ->  cont TAsc xs (s1,3 + s2)
         ('D':'e':'s':'c':xs) -> \(s1,s2) ->  cont TDesc xs (s1,4 + s2)

         ('C':'R':'E':'A':'T':'E':' ':'T':'A':'B':'L':'E':xs) -> \(s1,s2) ->  cont TCTable xs (s1,12 + s2)
         ('C':'R':'E':'A':'T':'E':' ':'D':'A':'T':'A':'B':'A':'S':'E':xs) -> \(s1,s2) -> cont TCBase xs (s1,14 + s2)
         ('D':'R':'O':'P':' ':'T':'A':'B':'L':'E':xs) -> \(s1,s2) -> cont TDTable xs (s1,10 + s2)
         ('D':'R':'O':'P':' ':'D':'A':'T':'A':'B':'A':'S':'E':xs) -> \(s1,s2) -> cont TDBase xs (s1,13 + s2)
         ('S':'H':'O':'W':' ':'D':'A':'T':'A':'B':'A':'S':'E':xs) -> \(s1,s2) -> cont TShowB xs (s1,13 + s2)
         ('S':'H':'O':'W':' ':'T':'A':'B':'L':'E':xs) -> \(s1,s2) -> cont TShowT xs (s1,10 + s2)
         ('K':'E':'Y':xs) -> \(s1,s2) -> cont TPkey xs (s1,3 + s2)
         ('U':'S':'E':xs) -> \(s1,s2) -> cont TUse xs (s1,3 + s2)
         ('S':'t':'r':'i':'n':'g':xs) -> \(s1,s2) -> cont TString xs (s1,6 + s2)
         ('F':'l':'o':'a':'t':xs) -> \(s1,s2) -> cont TFloat xs (s1,5 + s2)
         ('I':'n':'t':xs) -> \(s1,s2) -> cont TInt xs (s1,3 + s2)
         ('B':'o':'o':'l':xs) -> \(s1,s2) -> cont TBool xs (s1,4 + s2)

         ('S':'O':'U':'R':'C':'E':xs) -> \(s1,s2) -> cont TSrc xs (s1,6 + s2)

         ('F':'O':'R':'E':'I':'G':'N':' ':'K':'E':'Y':xs) -> \(s1,s2) -> cont TFKey xs (s1,11 + s2)
         ('R':'E':'F':'E':'R':'E':'N':'C':'E':'S':xs) -> \(s1,s2) -> cont TRef xs (s1,10 + s2)
         ('O':'N':' ':'D':'E':'L':'E':'T':'E':xs) -> \(s1,s2) -> cont TDel xs (s1,12 + s2)
         ('O':'N':' ':'U':'P':'D':'A':'T':'E':xs) -> \(s1,s2) -> cont TUpd xs (s1,12 + s2)
         ('R':'E':'S':'T':'R':'I':'C':'T':'E':'D':xs) -> \(s1,s2) -> cont TRestricted xs (s1,10 + s2)
         ('C':'A':'S':'C':'A':'D':'E':'S':xs) -> \(s1,s2) -> cont TCascades xs (s1,8 + s2)
         ('N':'U':'L':'L':'I':'F':'I':'E':'S':xs) -> \(s1,s2) -> cont TNullifies xs (s1,9 + s2)

         ('N':'U':'L':'L':xs) -> \(s1,s2) -> cont TNull xs (s1,4 + s2)

         ('.':xs) -> \(s1,s2) -> cont TDot xs (s1,1 + s2)
         (',':xs) -> \(s1,s2) -> cont TComa xs (s1,1 + s2)
         ('(':xs) -> \(s1,s2) -> cont TOpen xs (s1,1 + s2)
         (')':xs) -> \(s1,s2) -> cont TClose xs (s1,1 + s2)
         ('+':xs) -> \(s1,s2) -> cont TPlus xs (s1,1 + s2)
         ('-':xs) -> \(s1,s2) -> cont TMinus xs (s1,1 + s2)
         ('*':xs) -> \(s1,s2) -> cont TTimes xs (s1,1 + s2)
         ('/':'*':xs) -> consume xs
                         where consume ('/':'*':xs) = \i -> errorComOpen
                               consume ('*':'/':xs) = lexer cont xs
                               consume ('\n':xs) = \(s1,s2) -> consume xs (1 + s1,0)
                               consume (x:xs) = \(s1,s2) -> consume xs (s1,1 + s2)
                               consume "" =  \i -> errorComClose

         ('*':'/':xs) -> \i -> errorComClose

         ('/':xs) -> \(s1,s2) -> cont TDiv xs (s1,1 + s2)



         (';':xs) -> \(s1,s2) -> cont TSemiColon xs (s1,1 + s2)
         (x:xs)  | isSpace x -> \(s1,s2) ->  lexer cont xs (s1,s2+1)
                 | x == '\"' -> lexString cont (x:xs)
                 | isLetter x -> lexField cont (x:xs)
                 | isDigit x -> lexNum cont (x:xs)






lexNum cont xs =  \(s1,s2) -> cont (TNum n) r (s1,s2 + (length $ show n))
  where [(n,r)] = parse integer xs


lexField cont xs = \(s1,s2) -> cont (TField s) r (s1,s2 + (length s))
  where [(s,r)] = parse identifier xs


lexString cont  xs = \(s1,s2) -> cont (TStr s) r (s1,s2 + (length s))
   where [(s,r)] = parse st xs
         st = do char '\"'
                 s <- string2
                 char '\"'
                 return s


sqlParse s = sql s (1,1)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
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
