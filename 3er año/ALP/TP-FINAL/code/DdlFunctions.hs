module DdlFunctions where
import System.Environment
import Prelude hiding (catch,lookup)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import AST (CArgs (..),TableInf,Type,Env(..),TableDescript(..),
            Reference,ForeignKey,Tab,
            fields,fields2,fields3)
import DynGhc (compile)
import Data.HashMap.Strict (fromList,HashMap,(!),update,lookup)
import Error (tablePath,errorCreateTable,errorDropTable,succesDropTable,
              errorCreateReference,succesCreateTable,succesCreateReference,
              errorCheckReference,put)
import Url
import Avl ((|||),toTree,deleteT,c2,write,c,AVL)
import DynGhc (appendLine,reWrite)
import Data.List (elem,sort)
import DynGhc (obtainTable,loadInfoTable)
import Data.Maybe (isNothing,fromJust,isJust)
import COrdering (COrdering(..))

defRecord :: [String] -> [String] -> String
defRecord [] _ = ""
defRecord (x:xs) (y:ys) = "," ++ x ++ "::" ++ y ++  defRecord xs ys


autoComplete2 :: String -> String
autoComplete2 t | t == "String" = "\"\""
                | t == "Int" = "0"
                | t == "Double" = "0.0"
                | otherwise = t


typesDef :: [String] -> [Type] -> String
typesDef _ [] = ""
typesDef (x:xs) (y:ys) = "," ++ "(\"" ++ x ++ "\"," ++ (show y) ++ ")" ++
                         typesDef xs ys


code :: String -> TableInf -> String
code f (n,t,h,k,_) =
                       "module " ++ f ++ " where \n" ++
                       "import AST (Args (..),Type(..),TableDescript(..))\n" ++
                       "import Data.Typeable\n" ++
                       "import Data.HashMap.Strict hiding (keys) \n" ++
                       "import Avl (AVL(..),m)\n" ++
                       "import COrdering\n" ++
                       "keys = " ++ (show k) ++ "\n\n" ++
                       "upd0 = E"



deleteFile :: FilePath -> IO ()
deleteFile e = removeFile e `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = error "No existe el archivo"
          | otherwise = throwIO e



createDataBase :: FilePath -> IO ()
createDataBase p = createDirectory p

dropDataBase :: FilePath -> IO ()
dropDataBase p =  removeDirectoryRecursive p


createTable :: Env -> String -> [CArgs]  -> IO ()
createTable e t c =
 do let (q@(n,types,h,k,f),r) = collect c ||| url' e t
    let b = (not $ isSubset k n) || ( not $ isSubset2 f n)
    if b then putStrLn $ errorCreateTable
    else do b <- checkReference (name e) f
            if not b  then errorCheckReference
            else do createReference t e f
                    let hs = r ++ ".hs"
                    writeFile hs $ code t q
                    compile r
                    removeFile $ r ++ ".hi"
                    let t' = aux [TO (name e),TN t,TS n,TT types,TK k,TFK (fk f), TR []]
                    appendLine tablePath t'
                    succesCreateTable t




-- Determina si la clave y la clave foránea estan dentro del esquema
  where aux l = toTree $ [fromList $ zip fields2 l]
        isSubset [] _ = True
        isSubset (k:ks) m = if k `elem` m then isSubset ks m
                            else False
        isSubset2 [] _ = True
        isSubset2 ((_,xs,_,_):ys) m = if isSubset xs m then isSubset2 ys m
                                      else False
        fk f = [(x,xs) | (x,xs,_,_) <- f]



--- Chequear que las referencias sean válidas
checkReference :: String -> ForeignKey -> IO (Bool)
checkReference  _ []  =  return True
checkReference  u ((x,xs,o1,o2):ys) =
  do r <- loadInfoTable ["key"] u x
     case r of
      [] -> return False
      [(TK k)] -> if (sort xs) /= (sort k) then return False
                  else checkReference u ys

-- Crear referencias
createReference :: String -> Env -> ForeignKey -> IO ()
createReference _ _ [] = return ()
createReference n e ((x,xs,o1,o2):ys) =
  do t <- obtainTable tablePath "Tables"
     case t of
      Nothing -> error "Error Fatal"
      Just t' -> do let r = createRegister (name e)  x
                    let t'' = write  (fun (n,o1,o2) fields r) t'
                    reWrite t'' tablePath
                    succesCreateReference n x
                    createReference n e ys

  where fun m k x y  = fun2 m $ c k x y
        fun2 m (Eq y) = fun3 m y
        fun2 _  t = t
        fun3 m y  = let f (TR l) = Just $ TR $ m:l
                    in Eq $ update f "refBy" y








dropTable :: Env -> String -> IO ()
dropTable e p = do t <- obtainTable tablePath "Tables"
                   if isNothing t then error "Error Fatal"
                   else do let r = createRegister (name e) p
                           case deleteT  (c2 fields r) (fromJust t) of
                            Nothing -> putStrLn $ errorDropTable p
                            Just t' -> do reWrite t' tablePath
                                          let r = url' e p
                                          deleteFile $ r ++ ".hs"
                                          deleteFile $ r ++ ".o"
                                          putStrLn $ succesDropTable p




-- Procesa las columnas para separar la información pertinente
collect :: [CArgs] -> TableInf
collect [] = ([],[],[],[],[])
collect((Col n t h):xs) = let (l1,l2,l3,k,f) = collect xs
                          in (n:l1,t:l2,h:l3,k,f)
collect ((PKey s):xs) = let (l1,l2,l3,k,f) = collect xs
                        in (l1,l2,l3,s:k,f)
collect ((FKey xs s o1 o2):ys) = let (l1,l2,l3,k,f) = collect ys
                                 in (l1,l2,l3,k,(s,xs,o1,o2):f)



createRegister :: String -> String -> HashMap String TableDescript
createRegister u n = fromList $ zip fields [TO u,TN n]
