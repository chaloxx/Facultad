module Run where
import SqlParse (sqlParse)
import AST
import DdlFunctions
import DmlFunctions
import Url
import System.TimeIt
import Control.Exception
import Data.List (isSuffixOf,dropWhileEnd)
import Error (errorSource,errorOpen,errorSelUser,errorSelBase)
import UserFunctions
import DynGhc (appendLine)
import qualified Data.HashMap.Strict as H
import System.Directory (doesDirectoryExist,doesFileExist,listDirectory)




parseCmd :: Env ->  String -> IO (Env)
parseCmd e s = case sqlParse s of
                Failed msg -> do putStrLn msg
                                 return e
                Ok cmd ->  runSql e cmd








runSql :: Env -> SQL -> IO (Env)
runSql e (S1 cmd) = runDml e cmd
runSql e (S2 cmd) = runDdl e cmd
runSql e (S3 cmd) = runManUser e cmd
runSql e (Seq cmd1 cmd2) = do e' <- runSql e cmd1
                              runSql e' cmd2
runSql e (Source p) = if ".sql" `isSuffixOf` p then read e p `catch` exception p e
                      else do putStrLn errorSource
                              return e

 where read e p = do s <- readFile p
                     let s' = process s
                     e' <- parseCmd e s'
                     return e'


       exception p e r = do let err = show (r :: IOException)
                            putStrLn $ errorOpen p err
                            return e
       -- Eliminar saltos de linea y espacios iniciales y finales
       process s = let s' =  dropWhileEnd f s in
                             dropWhile f s'
       f x = x == '\n' || x == ' '





runManUser :: Env -> ManUsers ->  IO (Env)
runManUser e (CUser u p) = do createUser u p;return e
runManUser e (SUser u p) = selectUser u p
runManUser e (DUser u p) = do deleteUser u p;return e



-- Ejecuta un comando DDL
runDdl :: Env -> DDL -> IO (Env)
runDdl e cmd = if checkSelectUser e then runDdl1 e cmd
               else  do putStrLn errorSelUser
                        return e

runDdl1 e cmd = case cmd of
  (CBase b) -> aux e $ createDataBase $ url (name e) b
  (DBase b) -> aux e $ dropDataBase $ url (name e) b
  (DTable t) -> runDdl2 e $ dropTable e t
  (CTable t c) -> runDdl2 e $  createTable e t  c
  (Use b) -> do v <- doesDirectoryExist (url (name e) b)
                if v then do putStrLn $ "Usando la base " ++ b
                             return (Env (name e) b)
                else do putStrLn $ "La base " ++ b ++ " no existe"
                        return e


  (ShowB) -> do printDirectory $ "DataBase/" ++ (name e)
                return e

  (ShowT) -> runDdl2 e $ printDirectory (url (name e) (dataBase e))

  where quitComa "" = ""
        quitComa (x:xs) = xs
        aux e f = do f
                     return e

runDdl2 e f = do if checkSelectBase e then f
                 else putStrLn errorSelBase
                 return e




-- Ejecuta un comando DML chequeando que previamente
-- se halla seleccionado una base o tabla válida
runDml :: Env -> DML -> IO (Env)
runDml e dml = let (b1,b2) = (checkSelectBase e ,checkSelectUser e) in
               if b1 && b2 then do runDml2 e dml;return e
               else do if b1 then putStrLn  errorSelUser
                       else putStrLn errorSelBase;
                       return e


runDml2 e (Insert m t) = insert e m t
runDml2 e (Delete m exp) = delete (e,H.empty,H.empty) m exp
runDml2 e (Update m v exp) = update (e,H.empty,H.empty) m v exp
runDml2 e q = runQuery (e,H.empty,H.empty) q



-- Determina si previamente  se ha elegido una BD
checkSelectBase :: Env ->  Bool
checkSelectBase e  = case dataBase e of
                        "" -> False
                        _  -> True

-- Determina si previamente  se ha elegido un usuario
checkSelectUser :: Env -> Bool
checkSelectUser e = case name e of
                      "" -> False
                      _ -> True

checkTable :: FilePath -> IO(Bool)
checkTable r = do b <- doesFileExist (r ++ ".hs")
                  return b








-- Imprime los directorios de una ruta dada
printDirectory :: FilePath -> IO ()
printDirectory p = do l <- listDirectory p
                      sequence_ (map putStrLn l)
