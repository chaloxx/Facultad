module Check where

import AST (Args(..),Type(..),Tab,BoolExp(.. ),TabTypes,Reg,ForeignKey,Env,show4,show2)
import Error (exitToInsert,fold,typeOfArgs,errorKey,typeError,lookupList,errorForKey)
import Data.HashMap.Strict
import Avl (isMember)
import System.IO.Unsafe (unsafePerformIO)
import DynGhc (obtainTable)
import Data.Maybe (isJust,fromJust)
import Url (url')


-- Chequea que la cantidad de args pasados sea correcta
checkLength :: Int -> [Args] -> Either String String
checkLength l types = if l == length types then exitToInsert types
                      else Left $  (fold types) ++ " tiene demasiados argumentos.."



-- Chequea que el tipo de los args pasados sea correcto
checkTyped ::  [Type] -> [Args] -> Either String String
checkTyped t r = if checkTyped' t r then exitToInsert r
                 else Left $ "Error de tipo en  " ++ (fold r)
 where checkTyped' [] (x:xs) = False
       checkTyped' _  [] = True
       checkTyped' (y:ys) (x:xs) = if y == (typeOfArgs x)  then checkTyped' ys xs
                                   else False


-- Chequea si la clave del registro r ya existe en la tabla t
checkKey :: Tab -> [String] -> [String] -> [Args] -> Either String String
checkKey t' k f x = let r = fromList $ zip f x in
                    if isMember k r t' then errorKey x
                    else return ""







-- Chequea que expresión sea segura comprobando los tipos de las subexpresiones
checkTypeBoolExp :: BoolExp -> [String] -> TabTypes -> Either String Type
checkTypeBoolExp e@(And exp1 exp2) s types = checkTypeBoolExp'' exp1 exp2 s types e
checkTypeBoolExp e@(Or exp1 exp2) s types = checkTypeBoolExp'' exp1 exp2 s types e
checkTypeBoolExp e@(Less exp1 exp2) s types = checkTypeBoolExp' exp1 exp2 s types e
checkTypeBoolExp e@(Great exp1 exp2) s types = checkTypeBoolExp' exp1 exp2 s types e
checkTypeBoolExp e@(Equal exp1 exp2) s types = checkTypeBoolExp' exp1 exp2 s types e
checkTypeBoolExp e@(Exist dml) _ _ = return Bool
checkTypeBoolExp e@(Like f w) s g = return Bool

checkTypeBoolExp'' exp1 exp2 s types e  = do t1 <- checkTypeBoolExp exp1 s types
                                             t2 <- checkTypeBoolExp exp2 s types
                                             case t2 == Bool && t1 == Bool  of
                                              True -> return Bool
                                              False -> typeError (show4 e)



checkTypeBoolExp' exp1 exp2 s types e =
   do t1 <- checkTypeExp s types exp1
      t2 <- checkTypeExp s types exp2
      case t1 == t2 of
         True -> return Bool
         False -> typeError (show4 e)




checkTypeExp :: [String] -> TabTypes -> Args -> Either String Type
checkTypeExp s g e@(Plus exp1 exp2) = checkTypeExp' s False g exp1 exp2 e
checkTypeExp s g e@(Minus exp1 exp2) = checkTypeExp' s False g exp1 exp2 e
checkTypeExp s g e@(Times exp1 exp2) = checkTypeExp' s False g exp1 exp2 e
checkTypeExp s g e@(Div exp1 exp2) = checkTypeExp' s True g exp1 exp2 e
checkTypeExp s g (A1 _) = return String
checkTypeExp s g (A3 _) = return Int
checkTypeExp s g (A4 _) = return Float
checkTypeExp s g (Field v) = lookupList g s v
checkTypeExp s g (Dot s1 s2) = lookupList g [s1] s2
checkTypeExp s g (Negate exp) = checkTypeExp s g exp
checkTypeExp s g (Brack exp) = checkTypeExp s g exp
checkTypeExp s g (As exp _) = checkTypeExp s g exp


checkTypeExp' s b g exp1 exp2 e =
  do t1 <- checkTypeExp s g exp1
     aux t1 e
     t2 <- checkTypeExp s g exp2
     aux t2 e
     case b of
       True -> return Float
       False -> case t1 == Float || t2 == Float of
                  True -> return Float
                  False -> return Int
  where aux t e = case t == Float || t == Int of
                     True -> return t
                     False -> typeError $ (show2 e)




checkForKey :: Env -> Reg -> [(String,[String])] -> Either String String
checkForKey _ _ []  = return ""
checkForKey e r ((x,xs):ys)  = do let p = url' e x
                                  let t = unsafePerformIO $ obtainTable p x
                                  if isJust t then if isMember xs r (fromJust t) then checkForKey e r ys
                                                   else errorForKey
                                  else error "Error fatal"
