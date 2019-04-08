{-# LANGUAGE MagicHash, UnboxedTuples #-}
module DynGhc where
import GHC.Exts         ( addrToAny# )
import GHC.Ptr          ( Ptr(..) )
import System.Info      ( os, arch )
import Encoding
import GHCi.ObjLink
import Data.Typeable (TypeRep)
import GHC.Paths
import AST (Symbol,Args(..),TableDescript(..))
import qualified GHC
import Data.Maybe
import Avl
import Data.HashMap.Strict hiding (foldr,map)
import System.IO
import Control.DeepSeq
import Data.Char
import Error (tablePath,userPath,put)
import Prelude hiding (lookup)

loadInfoTable :: [String] -> String -> String -> IO([TableDescript])
loadInfoTable s u m = do r <- obtainTable tablePath "Tables"
                         case r of
                          Nothing -> return []
                          Just t -> do let r = fromList $ zip f [TO u,TN m]
                                       let j = search f r t
                                       case j of
                                        Nothing -> return []
                                        Just r' -> return $ map  (\x -> case lookup x r' of
                                                                         Nothing -> error "Error Fatal"
                                                                         Just s -> s) s
 where f = ["owner","tableName"]

appendLine ::Show a => FilePath -> AVL (HashMap String a) -> IO ()
appendLine _ E = return ()
appendLine r t = do l <- obtainLastLine r
                    n <- obtainN l ""
                    appendFile (r ++ ".hs") (upd n t)
                    compile r >> return ()


     where    upd n t   = "\nupd" ++ (show (n+1)) ++ " = " ++
                          "m " ++ "keys " ++  "upd" ++ (show n) ++
                          " (" ++ (show t) ++ ")"









obtainLastLine :: FilePath -> IO (String)
obtainLastLine r = do h <- openFile (r ++ ".hs") ReadMode
                      obtainLastLine' h
 where obtainLastLine' h = do t <- hGetLine h
                              b <- hIsEOF h
                              case b of
                                 True -> do s <- hClose h
                                            s `deepseq` return t
                                 False -> obtainLastLine' h





iter :: Handle -> Int ->  IO ([String])
iter h 0 = return []
iter h n = do l <- hGetLine h
              ls <- iter h (n-1)
              return(l:ls)





reWrite :: Show a => AVL (HashMap String a ) -> FilePath -> IO ()
reWrite t r   = do h <- openFile (r ++ ".hs") ReadMode
                   l <- iter h 9
                   s <- hClose h
                   let l' = if isEmpty t then l
                            else l ++ ["upd1 = m keys upd0 " ++ "(" ++ (show t) ++ ")"]
                   s `deepseq`  writeFile (r ++ ".hs") $ foldr (\x y -> x ++ "\n" ++ y) "" l'
                   compile r
                   return ()


obtainTable :: FilePath -> String -> IO(Maybe (AVL (HashMap String a)))
obtainTable r m = do l <- obtainLastLine r
                     n <- obtainN l ""
                     load r m ("upd" ++ show n)


obtainN :: String ->String -> IO (Int)
obtainN ('u':'p':'d':xs) r = obtainN xs ""
obtainN (x:xs) r | isDigit x = obtainN xs (r ++ [x])
                 | otherwise = return (read r :: Int)



load :: FilePath -> String -> Symbol ->  IO (Maybe a)
load r m f =  do initObjLinker
                 loadObj (r ++ ".o")
                 loadObj ("Avl.o")
                 resolveObjs
                 ptr <- lookupSymbol (mangleSymbol Nothing m f)
                 unloadObj (r ++ ".o")
                 unloadObj "Avl.o"
                 case ptr of
                    Nothing -> return (Nothing)
                    Just (Ptr addr) -> case addrToAny# addr of
                                         (# t #) -> return (Just t)



mangleSymbol :: Maybe String -> String -> String -> String
mangleSymbol pkg module' valsym = prefixUnderscore ++ maybe "" (\p -> zEncodeString p ++ "_") pkg ++zEncodeString module' ++ "_" ++ zEncodeString valsym ++ "_closure"

prefixUnderscore :: String
prefixUnderscore =
  case (os,arch) of
    ("mingw32","x86_64") -> ""
    ("cygwin","x86_64") -> ""
    ("mingw32",_) -> "_"
    ("darwin",_) -> "_"
    ("cygwin",_) -> "_"
    _ -> ""






compile :: FilePath -> IO (GHC.SuccessFlag)
compile r = GHC.runGhc (Just libdir) $ do
            dflags <- GHC.getSessionDynFlags
            GHC.setSessionDynFlags dflags
            target <- GHC.guessTarget (r ++ ".hs") Nothing
            GHC.setTargets [target]
            GHC.load GHC.LoadAllTargets
