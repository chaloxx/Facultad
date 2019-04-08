module User where
import SqlParse (sqlParse)
import ParseResult (ParseResult(..))
import AST
import System.Console.Readline
import DdlFunctions
import System.Directory
import DmlFunctions
import Url
import qualified Data.HashMap.Strict as H
import System.TimeIt
import Control.Exception
import Data.List (isSuffixOf)
import Error (errorSource,errorOpen)





main :: IO ()
main = do maybeLine <- readline "SQL > "
          case maybeLine of
            Nothing     -> return () 
            Just "q" -> return ()
            Just line -> do addHistory line
                            case parseInit line of
                                     CmdError -> do print "Entrada inválida"
                                                    main
                                     cmd      -> run cmd 


runManUsers :: ManUsers ->  IO ()
runManUsers (CUser u p) = do createUser u p;main
runManUsers (SUser u p) = do selectUser u p;main
runManUsers (DUser u p) = do deleteUser u p;main 


 


-- Interfaz de usuario
user :: String -> IO ()
user s = do putStrLn ("Bienvenido " ++ s ++ "!")
            user' (Env s "") 
user' e = do maybeLine <- readline $ "SQL@" ++ (name e) ++ " > "
             case maybeLine of
                Nothing     -> user' e 
                Just "q" -> return ()
                Just line -> do addHistory line
                                parseCmd e line



parseCmd :: Env ->  String -> IO ()
parseCmd e s = case sqlParse s of 
                Failed msg -> do putStrLn msg
                                 user' e
                Ok cmd -> do timeItNamed "\nTime of answer" $ runSql e cmd
                             user' e

      







runSql :: Env -> SQL -> IO ()
runSql e (S1 cmd) = runDml e cmd
runSql e (S2 cmd) = runDdl e cmd
runSql _ (S3 cmd) = runManUsers cmd
runSql e (Seq cmd1 cmd2) = do runSql e cmd1
                              runSql e cmd2
runSql e (Source p) = case ".sql" `isSuffixOf` p of 
                       False -> putStrLn errorSource 
                       True -> read e p `catch` exception p
       where read e p = do s <- readFile p
                           parseCmd e s
             exception p e = do let err = show (e :: IOException)
                                putStrLn $ errorOpen p err
                                return ()




-- Ejecuta un comando DDL
runDdl :: Env -> DDL -> IO ()
runDdl e cmd = case cmd of 
  (CBase b) -> createDataBase $ url (name e) b
  (DBase b) -> dropDataBase $ url (name e) b
  (DTable t) -> checkSeletBase e $ dropTable (url' e t)
  (CTable t c) -> checkSeletBase e $ createTable e t  c
  (Use b) -> do v <- doesDirectoryExist (url (name e) b)
                if v then do putStrLn $ "Usando la base " ++ b
                             user' (Env (name e) b) 
                else putStrLn $ "La base " ++ b ++ " no existe"
  (ShowB) -> printDirectory $ "DataBase/" ++ (name e)
  (ShowT) -> checkSeletBase e $ printDirectory (url (name e) (dataBase e))
  where quitComa "" = ""
        quitComa (x:xs) = xs



-- Determina si previamente  se ha elegido una BD y ejecuta f
checkSeletBase :: Env -> IO () -> IO ()
checkSeletBase e f = case dataBase e of
                       "" -> do putStrLn "Primero debe seleccionar una base de datos..."
                                user' e
                       b  -> f 

checkTable :: FilePath -> IO(Bool)
checkTable r = do b <- doesFileExist (r ++ ".hs")
                  return b




-- Ejecuta un comando DML chequeando que previamente 
-- se halla seleccionado una base o tabla válida
runDml :: Env -> DML -> IO ()
runDml e (Insert m t) = runDml' (insert e m t) (url' e m) e
runDml e (Delete m exp) = runDml' (delete (e,H.empty,H.empty) m exp) (url' e m) e
runDml e (Update m v exp) = runDml' (update (e,H.empty,H.empty) m v exp) (url' e m) e 
runDml e q@(Select _ _ _w) = runQuery (e,H.empty,H.empty) q

runDml' :: IO ()-> FilePath -> Env -> IO ()
runDml' cmd r  e  = checkSeletBase e $
                    do b <- checkTable r
                       case b of
                          True ->  cmd 
                          False -> putStrLn "La tabla no existe"


-- Imprime los directorios de una ruta dada
printDirectory :: FilePath -> IO ()
printDirectory p = do l <- listDirectory p
                      sequence_ (map putStrLn l)


 
