module UserFunctions where
import qualified Data.HashMap.Strict as H
import AST (Env(..))
import Avl (toTree,isMember)
import System.Directory (createDirectory,removeDirectoryRecursive)
import DynGhc (appendLine,obtainTable)
import Error (userPath,logError,welcome)






createUser :: String -> String -> IO ()
createUser u p   = case u == "" || p == "" of
                     True -> putStrLn "Error al crear el usuario"
                     False-> do createDirectory ("DataBase/" ++ u)
                                let t' = toTree $ [H.fromList $ zip fields [u,p]]
                                appendLine userPath t'



selectUser :: String -> String -> IO (Env)
selectUser u p = if u == "" || p == "" then logError u
                 else do r <- obtainTable userPath "Users"
                         case r of
                          Nothing -> error ""
                          Just t -> do let m = H.fromList $ zip fields [u,p]
                                       if isMember fields m t then do putStrLn $ welcome u
                                                                      return (Env u "")
                                       else  logError u



deleteUser :: String -> String ->  IO ()
deleteUser u p = case u == "" || p == "" of
                  True -> putStrLn "Error al eliminar el usuario"
                  False -> removeDirectoryRecursive ("DataBase/" ++ u)



fields = ["userName","pass"]
