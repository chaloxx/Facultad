module AST where
import Avl (AVL)
import  Data.HashMap.Strict  (HashMap (..),insert,delete,empty)
import qualified Data.Set as S
import Data.Hashable
import Data.Typeable (TypeRep)


data Env = Env {name :: String, dataBase :: String} deriving Show
type State = (Env,TabReg,TabTypes) 
type Pattern = String
type FieldName = String
type TableName = String
type Column = [Int]          
type Table = String
type Database = [Table]
type Symbol = String
type Cola a = [(Int,a)]
type TabNames = [String]
type FieldNames = [String]
-- Definimos una answer como una respuesta a una consulta
-- Consta de 3 elementos: Un booleano para diferenciar si la consulta incluye
-- una claúsula group by, una lista de los atributos de las tablas actuales y las tablas
type Answer =  (State,Bool,[String],[String],[AVL (HashMap String Args)])
type TabReg = HashMap String Reg
type Reg = HashMap String Args
type Tab = AVL (Reg)
type Distinct = Bool
type TableInf = ([String],[Type],[Bool],[String])
type TabTypes = HashMap String Types
type Types = HashMap String Type
data Type = String | Int | Float | Bool deriving (Show,Eq,Ord)


-- Operadores de álgebra relacional (Operan sobre las BD)
data AR =     Pi Distinct [Args]  
            | Prod [Args]
            | Sigma BoolExp  
            | Difference Table Table
            | Hav BoolExp
            | Un Table Table
            | Order [Args] O
            | Group [Args]
            deriving Show
            
            
            
            

-- Lenguaje DML (Describen la información solicitada sobre la BD)
data DML =     Select Distinct [Args] DML
              | From [Args] DML 
              | Where BoolExp  DML 
              | GroupBy [Args] DML
              | Having BoolExp  DML
              | OrderBy [Args] O DML
              | Union DML DML
              | End 
              | Insert String (AVL ([Args]))
              | Delete String BoolExp
              | Update String ([String],[Args])  BoolExp 
              deriving (Show, Eq, Ord)

instance (Ord a, Ord v) => Ord (HashMap a v) where 
     t1 <= t2 = t1 == t2 || t1 < t2             

-- Arguments
data Args = A1 String
          | A2 Aggregate
          | A3 Int 
          | A4 Float
          | All  
          | Subquery DML
          | As Args Args
          | Nulo -- Valor nulo
          | Field String
          | Dot String String
          | Plus Args Args
          | Minus Args Args
          | Times Args Args
          | Div Args Args
          | Negate Args
          | Brack Args
          deriving (Eq,Ord,Show)
          

-- Funciones de agregado (Operan sobre columnas)
data Aggregate = Min Distinct String
               | Max Distinct String
               | Sum Distinct String
               | Count Distinct Args 
               | Avg Distinct String 
               | Top Int String
               deriving (Eq,Ord,Show)


-- Expresiones booleanas 
data BoolExp =  And BoolExp BoolExp
              | Or  BoolExp BoolExp
              | Equal Args Args
              | Great Args Args
              | Less Args Args
              | Not BoolExp
              | Exist DML
              | InS [Args] DML
              | InV Args [Args]
              | Like String String
              deriving (Show,Eq,Ord)

              





-- Order
data O = A | D deriving (Show,Eq,Ord)





 
-- DDL Language (Diseñan la BD)

type BaseName = String
type HaveNull = Bool
type Delete = Option
type Update = Option

data DDL = 
             CBase BaseName 
           | DBase BaseName
           | CTable TableName [CArgs]
           | DTable TableName
           | Seq DDL DDL
           | Use BaseName
           | ShowB
           | ShowT 
           deriving Show
           


data CArgs =  Col String Type HaveNull 
            | PKey FieldName
            | FKey FieldName TableName Delete Update
            deriving Show
            

data Option = Restricted | Cascades | Nullifies  deriving Show          


-- Tipo de dato para representar la descripción de una tabla
data TableDescript = TO String | TN String | TS [String] | TT [Type]  deriving (Show,Eq,Ord)


filterL = filter
insertH :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v 
insertH = insert
deleteH :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
deleteH = delete
emptyHM :: HashMap k v 
emptyHM = empty 

tryJoin :: (e -> b -> b) -> b -> [Either a e] -> Either a b
tryJoin u e [] = return e
tryJoin u e ((Left x):xs) = Left x
tryJoin u e ((Right x):xs) = do xs' <- tryJoin u e xs
                                return (u x xs')


eitherFilterL :: (e -> Either a Bool) -> [e] -> Either a [e]
eitherFilterL f (x:xs) = do xs' <- eitherFilterL f xs
                            b <- f x
                            case b of 
                              True -> return (x:xs')
                              False -> return xs



show2 (A1 s) = s
show2 (A2 f) = show3 f
show2 (A3 n) = show n
show2 (A4 f) = show f
show2 (Field e) = e
show2 (As _ s) = show2 s
show2 (Dot s1 s2) = s1 ++ "." ++ s2
show2 (Plus exp1 exp2) = (show2 exp1) ++ "+" ++ (show2 exp2)
show2 (Minus exp1 exp2) = (show2 exp1) ++ "-" ++ (show2 exp2)
show2 (Times exp1 exp2) = (show2 exp1) ++ "*" ++ (show2 exp2)
show2 (Div exp1 exp2) = (show2 exp1) ++ "/" ++ (show2 exp2)
show2 (Negate exp1) = "-" ++ (show2 exp1)  
show2 (Brack exp1) = "(" ++ (show2 exp1) ++ ")"
show2 (All) = "All"



show3 (Min _ s) = "Min " ++ s
show3 (Max _ s) = "Max " ++ s
show3 (Sum _ s) = "Sum " ++ s
show3 (Count _ s) = "Count " ++ (show2 s)
show3 (Avg _ s) = "Avg " ++ s
show3 (Top _ s) = "Top " ++ s


show4 (And e1 e2) = (show4 e1) ++ " AND " ++ (show4 e2)
show4 (Or e1 e2) = (show4 e1) ++ " OR " ++ (show4 e2)
show4 (Equal e1 e2) = (show2 e1) ++ " = " ++ (show2 e2)
show4 (Less e1 e2) = (show2 e1) ++ " < " ++ (show2 e2)
show4 (Great e1 e2) = (show2 e1) ++ " > " ++ (show2 e2)



fst' :: (a,b,c) -> a
fst' (x,_,_) = x


snd' :: (a,b,c) -> b
snd' (_,y,_) = y

trd' :: (a,b,c) -> c
trd' (x,y,z) = z


isInt :: RealFrac b => b -> Bool
isInt x = x - fromInteger(round x) == 0


