Basado en:
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Modificado por Mauro Jaskelioff

> module Parsing  where
>
> import Data.Char
> import Control.Monad
> import Control.Applicative

The monad of parsers
--------------------

> newtype Parser a              =  P (String -> [(a,String)])
>
> instance Functor Parser where
>    fmap = liftM
>
> instance Applicative Parser where
>    pure = return
>    (<*>) = ap
> 
> instance Monad Parser where
>    return v                   =  P (\inp -> [(v,inp)])
>    p >>= f                    =  P (\inp -> [ x | (v,out) <- parse p inp, x <- parse (f v) out])
>
> instance Alternative Parser where
>    empty = mzero
>    (<|>) = mplus
> 
> instance MonadPlus Parser where
>    mzero                      =  P (\_   -> [])
>    p `mplus` q                =  P (\inp -> case parse p inp of
>                                                []        -> parse q inp
>                                                x         -> x)

Basic parsers
-------------

> failure                       :: Parser a
> failure                       =  mzero
>
> item                          :: Parser Char
> item                          =  P (\inp -> case inp of
>                                                []     -> []
>                                                (x:xs) -> [(x,xs)])
> 
> parse                         :: Parser a -> String -> [(a,String)]
> parse (P p) inp               =  p inp

Derived primitives
------------------

> sat                           :: (Char -> Bool) -> Parser Char
> sat p                         =  do x <- item
>                                     if p x then return x else failure
> 
> digit                         :: Parser Char
> digit                         =  sat isDigit
> 
> lower                         :: Parser Char
> lower                         =  sat isLower
> 
> upper                         :: Parser Char
> upper                         =  sat isUpper
> 
> letter                        :: Parser Char
> letter                        =  sat isAlpha
> 
> alphanum                      :: Parser Char
> alphanum                      =  sat isAlphaNum
> 
> char                          :: Char -> Parser Char
> char x                        =  sat (== x)
> 
> string                        :: String -> Parser String
> string []                     =  return []
> string (x:xs)                 =  do char x
>                                     string xs
>                                     return (x:xs)
> 
> many                          :: Parser a -> Parser [a]
> many p                        =  many1 p <|> return []
> 
> many1                         :: Parser a -> Parser [a]
> many1 p                       =  do v <- p
>                                     vs <- many p
>                                     return (v:vs)
> 
> ident                         :: Parser String
> ident                         =  do x  <- lower
>                                     xs <- many alphanum
>                                     return (x:xs)
> 
> nat                           :: Parser Int
> nat                           =  do xs <- many1 digit
>                                     return (read xs)
>
> int                           :: Parser Int
> int                           =  do char '-'
>                                     n <- nat
>                                     return (-n)
>                                   <|> nat
> 
> space                         :: Parser ()
> space                         =  do many (sat isSpace)
>                                     return ()
> 
> sepBy                         :: Parser a -> Parser sep -> Parser [a]
> sepBy p sep                   =  sepBy1 p sep <|> return []
>
> sepBy1                        :: Parser a -> Parser sep -> Parser [a]
> sepBy1 p sep            = do{ x <- p
>                               ; xs <- many (sep >> p)
>                               ; return (x:xs) } 
>
> endBy1                        :: Parser a -> Parser sep -> Parser [a]
> endBy1 p sep                  = many1 (do { x <- p; sep; return x })
>
> endBy                         :: Parser a -> Parser sep -> Parser [a]
> endBy p sep                     = many (do{ x <- p; sep; return x })
>
>

Ignoring spacing
----------------

> token                         :: Parser a -> Parser a
> token p                       =  do space
>                                     v <- p
>                                     space
>                                     return v
> 
> identifier                    :: Parser String
> identifier                    =  token ident
> 
> natural                       :: Parser Int
> natural                       =  token nat
> 
> integer                       :: Parser Int
> integer                       =  token int
>
> symbol                        :: String -> Parser String
> symbol xs                     =  token (string xs)
>
> p   :: Parser String
> p = do char '['
>        d <- digit
>        ds <- many ( char ',' >> digit);
>        char ']';
>        return (d:ds)


Ej 2:

> expr :: Parser Int
> expr = do t <- term
>           (do char '+';
>               e <- expr;
>               return (t+e)
>               <|> do char '-';
>                      d <- expr;
>                      return (t-d);
>                      <|> return (t))

> term :: Parser Int
> term = do f <- factor; 
>           (do char '*';
>               t <- term;
>               return (f*t)
>               <|> do char '/';
>                      e <- term;
>                      return (div f e);
>                      <|> return (f))

> factor :: Parser Int
> factor = do d <- digit;
>             ds <- (many digit);
>             return (digitsToInt (d:ds))
>             <|> do char '(';
>                    e <- expr;
>                    char ')';
>                    return (e)



> digitsToInt::String -> Int
> digitsToInt s = digitsToInt' s ((length s)-1)
>   where digitsToInt' s i | i == 0 = digitToInt (head s)
>                          | otherwise = (digitToInt (head s)) * (10^i) + (digitsToInt' (drop 1 s)((length (drop 1 s))-1))


Ej 3:

> transf2 :: Parser Char
> transf2 =  do char ')';
>               <|> do item;
>                      transf2;


> transf:: Parser a -> Parser a
> transf p = do char '(';
>               x <- p
>               transf2
>               return (x)
>               <|> p;


Ej 4:

> data Expr = NumInt | BinOp Op Expr Expr deriving Show
> data Op = Add | Mul | Min | Div deriving Show


> expr2 :: Parser Expr
> expr2 = do t <- term2
>            do char '+';
>               e <- expr2;
>               return (BinOp Add t e)
>              <|> do char '-';
>                     d <- expr2;
>                     return (BinOp Min t d);
>                     <|> return (t)

> term2 :: Parser Expr
> term2 = do f <- factor2; 
>            do char '*';
>               t <- term2;
>               return (BinOp Mul f t)
>               <|> do char '/';
>                      e <- term2;
>                      return (BinOp Div f e);
>                      <|> return (f)

> factor2 :: Parser Expr
> factor2 = do d <- digit;
>              ds <- (many digit);
>              return (NumInt);
>              <|> do char '(';
>                     e <- expr2;
>                     char ')';
>                     return (e)

Ej5 :

> data List = D Int List| C Char List | E deriving Show

> datos2:: List -> Parser List
> datos2 s = do char ']';
>               return (s)
>               <|> do d <- digit;
>                      let d' = digitToInt d
>                      do char ',' >> datos2 (D d' s);
>                         <|> datos2 (D d' s);
>                      <|> do c <- letter;
>                             do char ',' >> datos2 (C c s);
>                                <|> datos2 (C c s)


> datos :: Parser List
> datos = do char '[';
>            datos2 E;



Ej 6 :

> data Basetype = DInt | DChar | DFloat deriving Show
> type Hasktype = [Basetype]



> tipos :: Parser Hasktype
> tipos = tipos' []
>   where tipos' s = do string "Int";
>                       do string "->" >> tipos' (s++[DInt])
>                          <|> return (s++[DInt]);
>                       <|> do string "Char";
>                              do string "->" >> tipos' (s++[DChar])
>                                 <|> return (s++[DChar])
>                              <|> do string "Float";
>                                     do string "->" >> tipos' (s++[DFloat])
>                                        <|> return (s++[DFloat])
>                                     <|> return (s)  


> eval :: String -> Parser a -> a
> eval s p = fst (head (parse p s))


Ej 7:

> data Hasktype2 = DInt2 | DChar2 | Fun Hasktype2 Hasktype2 deriving Show

> tipos2 :: Parser Hasktype2
> tipos2 = do string "Int";
>             do string "->";
>                s <- tipos2; 
>                return (Fun DInt2 s)
>                <|> return (DInt2)
>             <|> do string "Char";
>                    do string "->";
>                       s <- tipos2;
>                       return (Fun DChar2 s)
>                       <|> return (DChar2);





Ej 9:


> data TypeSpecifier = Int | Char | Float deriving Show 
> data Declaration = D1 TypeSpecifier Declarator deriving Show
> data Declarator =  X1 Char Declarator | X2 DirectDeclarator deriving Show
> data DirectDeclarator = T1 DirectDeclarator Size | T2 Char DirectDeclarator Char | T3 [Char] deriving Show
> data Size = S1 Char Int Char Size | S2 Char Int Char deriving Show



> declaracion :: Parser Declaration
> declaracion = do symbol "int";
>                  v <- declarador;
>                  char ';'
>                  return (D1 Int v)
>                  <|> do symbol "char";
>                         v <- declarador;
>                         char ';'
>                         return (D1 Char v)
>                         <|> do symbol "Float";
>                                v <- declarador;
>                                char ';'
>                                return (D1 Float v)

declaracion s = do symbol s;
                   v <- declarador;
                   char ';'
                   



> declarador :: Parser Declarator
> declarador = do char '*';
>                 v <- declarador;
>                 return (X1 '*' v)
>                 <|> do char ' ';
>                        v <- declaradorDirecto;
>                        return (X2 v)
>                        <|> do v <- declaradorDirecto
>                               return (X2 v)


> declaradorDirecto :: Parser DirectDeclarator
> declaradorDirecto = do x <- letter;
>                        xs <- (many letter);
>                        do s <- size
>                           return (T1 (T3 (x:xs)) s)
>                           <|> return (T3 (x:xs)) 
>                        <|> do char '(';
>                               v <- declaradorDirecto;
>                               char ')';
>                               return (T2 '(' v ')')

> size :: Parser Size
> size = do char '[';
>           x <- int;
>           xs <- many int;
>           char ']';
>           let r = (listToInt (x:xs))
>           do v <- size;
>              return (S1 '[' r ']' v)
>              <|> return (S2 '[' r ']')








> listToInt::[Int] -> Int
> listToInt s = listToInt' s ((length s)-1)
>   where listToInt' s i | i == 0 = (head s)
>                          | otherwise = (head s) * (10^i) + (listToInt' (drop 1 s)((length (drop 1 s))-1))
