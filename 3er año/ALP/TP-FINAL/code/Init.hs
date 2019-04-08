import AST (Env(..))
import System.Console.Haskeline hiding (catch)
import System.Console.Haskeline.History
import Run (parseCmd,checkSelectUser)


main :: IO ()
main = main' (Env "" "") emptyHistory

d = defaultSettings
run = runInputT
{-
main' :: Env -> IO ()
main' e = do s <- txt e
             maybeLine <- getInputLine s
             case maybeLine of
              Nothing     -> main' e
              Just "q" -> return ()
              Just line -> do e' <- parseCmd e line
                              main' e'
  where -}

main' :: Env -> History -> IO ()
main' e h = do minput <- run d $ getInputLine (txt e)
               case minput of
                Nothing -> main' e h
                Just "q" -> return ()
                Just input -> do let h' = addHistory input h
                                 e' <- parseCmd e input
                                 main' e' h'
   
   where txt e = if checkSelectUser e then (name e) ++ "@> "
                 else "> "                               
                                






