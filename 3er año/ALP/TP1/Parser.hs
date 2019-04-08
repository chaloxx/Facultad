module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end", "while","do","repeat","until"]
                                  })
  
----------------------------------
--- Parser de expressiones enteras
-----------------------------------


 

intexp :: Parser IntExp
intexp = term `chainl1` addop

term :: Parser IntExp
term = factor `chainl1` mulop

factor :: Parser IntExp
factor = do reservedOp lis "-";
            n <- intexp
            return (UMinus n)
            <|> value
           
value :: Parser IntExp           
value = do n <- integer lis
           return (Const n)
           <|> do n <- identifier lis
                  return (Var n)
                  <|> parens lis intexp

mulop   =     do{ reservedOp lis "*"; return (Times)}
          <|> do{ reservedOp lis "/"; return (Div) }


addop   =     do { reservedOp lis "+"; return (Plus)}
          <|> do {reservedOp lis "-"; return (Minus)}






-----------------------------------
--- Parser de expressiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp =  disjunction 

disjunction :: Parser BoolExp
disjunction = conjunction `chainl1` orOp

conjunction :: Parser BoolExp
conjunction = negation `chainl1` andOp

negation :: Parser BoolExp
negation  = do reservedOp lis "~" 
               n <- boolexp
               return (Not n)
               <|> bvalue

bvalue :: Parser BoolExp
bvalue = do reserved lis "true"
            return (BTrue)
            <|> do reserved lis "false"
                   return (BFalse)
                   <|> parens lis disjunction
                   <|> compareint

compareint :: Parser BoolExp
compareint = do e <- intexp
                op' <- compareOp 
                e' <- intexp
                return (op' e e') 


compareOp = do {reservedOp lis "="; return (Eq)}
               <|> do {reservedOp lis ">"; return (Gt)}
                       <|> do {reservedOp lis "<"; return (Lt)}

orOp = do {reservedOp lis "|"; return (Or)}

andOp = do {reservedOp lis "&"; return (And)}


-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = asignation `chainl1` seqOp


asignation :: Parser Comm
asignation = do b <- identifier lis
                reservedOp lis ":="
                n <- intexp
                return (Let b n)
                <|> flow

flow :: Parser Comm
flow = do reserved lis "if"
          b <- boolexp
          reserved lis "then"
          n <- comm
          reserved lis "else"
          m <- comm
          reserved lis "end"
          return (Cond b n m)
          <|> do reserved lis "repeat"
                 c <- comm
                 reserved lis "until"
                 b <- boolexp
                 reserved lis "end"
                 return(Repeat c b)
                 <|> do reserved lis "while"
                        b <- boolexp
                        reserved lis "do"
                        c <- comm
                        return (While b c)
                        <|> do reserved lis "skip"
                               return (Skip)


seqOp = do {reservedOp lis ";"; return (Seq)}

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
