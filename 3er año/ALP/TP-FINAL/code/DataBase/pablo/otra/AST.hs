module AST where

data Env = Env {name :: String, dataBase :: String}

type Pattern = String
type Alias = String
type FieldName = String
type TableName = String
type Column = [Int]          
type Table = String
type Database = [Table]
type User = [Database] 
data Arg = ConstS String | ConstI Int deriving Show


-- Relational Algebra Operators
data AR =     Pi String  
            | Prod Table Table
            | Sigma BoolExp  
            | Minus Table Table
            | Un Table Table
            | Rename Table String
            | EndAR
            | SeqAR AR AR
            deriving Show
            
            
            
            

-- DML Language
data DML =     Select Args DML
              | From Args DML 
              | Where BoolExp  DML 
              | GroupBy Args DML
              | Having BoolExp  DML
              | OrderBy Args Order DML
              | Union DML DML
              | End 
              | Insert String [[Args]]
              | Delete String BoolExp
              | Update String [Args] BoolExp 
              deriving Show

-- Arguments for select , from, 
data Args = A1 String
          | A2 Aggregate
          | A3 Int 
          | A4 Float
          | Asterisco  
          | As Args Alias
          | E -- Valor nulo
          deriving Show
          

-- Aggregate functions
data Aggregate = Min String
               | Max String
               | Sum String
               | Count String 
               | Avg String 
               | Top Int String
               | Distinct String 
               deriving Show


-- Bool Expresions
data BoolExp =  And BoolExp BoolExp
              | Or  BoolExp BoolExp
              | Equal Args Args
              | Great Args Args
              | Less Args Args
              | Not BoolExp
              | Exist String DML
              | In Args Args
              | Like String String
              deriving Show
              





-- Order
data Order = ASC | DESC deriving Show





 
-- DDL Language

type BaseName = String
type HaveNull = Bool
type Delete = Option
type Update = Option
type Type = String 

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
           


data CArgs =  Col String Type Int HaveNull 
            | PKey FieldName
            | FKey FieldName TableName Delete Update
            deriving Show
            
data Option = Restricted | Cascades | Nullifies  deriving Show          

            


 