module ParseResult where

import AST (ParseResult (..),P (..),Info)

getLineNo :: P Info
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ aux s i
  where aux s (s1,s2) = (show (s1::Int)) ++ ":" ++
                        (show (s2::Int)) ++ ":" ++
                          "Error de parseo"
