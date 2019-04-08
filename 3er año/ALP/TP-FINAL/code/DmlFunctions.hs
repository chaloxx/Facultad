module DmlFunctions where
import Data.Typeable
import AST
import System.IO
import Url
import System.Directory
import DynGhc
import Avl hiding (singleton,All)
import Data.Char
import Data.Maybe
import Prelude hiding (lookup,print)
import Data.HashMap.Strict hiding (foldr,map,size,null)
import Control.DeepSeq
import Error
import Data.Either
import Data.List (intersect,(\\),partition,isPrefixOf)
import qualified Data.Set as S
import System.Console.Terminal.Size  (size,width)
import Data.Hashable
import COrdering (COrdering (..))
import Parsing (parse,sepBy,char,identifier)
import Check

---- Insertar en una tabla
insert :: Env -> String -> AVL ([Args]) ->  IO ()
insert e m t = do let (r,u) = url' e m ||| name e
                  [TS scheme,TT types,TK k, TFK f] <- loadInfoTable ["scheme","types","key","fkey"] u m  :: IO ([TableDescript])
                  t' <- obtainTable r m :: IO (Maybe (Tab))
                  if isJust t'  then insert' e r types scheme k (fromJust t') t f
                  else putStrLn "Error al cargar datos.."

-- Segundo nivel de insertar
insert' ::Env -> FilePath -> [Type] -> [String] -> [String] -> Tab -> AVL ([Args]) -> [(String,[String])] -> IO ()
insert' _ _ _ _ _ _ E _= return ()
insert' e r types fields k t' t f = do --Nro de argumentos recibidos
                                       let l = length fields
                                       -- Separar entradas válidas de las inválidas
                                       let (t1,t2) = particionT2 (\x -> do checkLength l x;checkTyped types x;checkKey t' k fields x;checkForKey e (fromList $ zip fields x) f)
                                                                 (\x -> fromList $ zip fields x)
                                                                 t
                                       -- Escribir modificaciones en archivo fuente
                                       appendLine r t2
                                       -- Mostrar que entrada son inválidas
                                       sequence_ $ mapT putStrLn  t1





toText :: [Args] -> String
toText [] = ""
toText (x:[]) = (show x)
toText (x:xs) = (show x) ++ " , " ++ (toText xs)





-- Borrar de una tabla
delete :: State -> String ->  BoolExp -> IO ()
delete g n exp = do let e = fst' g
                    let r = url' e n
                    let u = name (fst' g)
                    -- Obtener tabla
                    t <- obtainTable r n
                    case t of
                        Nothing ->  putStrLn "Error al cargar tabla..."
                        (Just t') -> do [TK k,TR ref] <- loadInfoTable ["key","refBy"] u n
                                        let (xs,ys,zs) =  partRefDel  ref
                                        xs' <- obtainTableList e xs -- tablas que tienen una referencia de tipo restricted sobre t'
                                        ys' <- obtainTableList e ys -- tablas que tienen una referencia de tipo cascades sobre t'
                                        zs' <- obtainTableList e zs -- tablas que tienen una referencia de tipo nullifies sobre t'
                                        a <- ioEitherFilterT (fun k g n xs xs' ys ys' zs zs') t'
                                        case a of
                                         Left msg -> putStrLn msg
                                         Right t'' ->  reWrite t'' r

         -- Filtro complicado (evalua expresión booleana y chequea restricciones)
  where  fun k g n xs xs' ys ys' zs zs' x =
           do let r = singleton n x
              r <- evalBoolExp [n] (updateState2 g r) exp
              case r of
                Left msg -> retFail msg
                Right b -> do if not b then retOk True
                              else do a <- resolRestricted xs xs' k x
                                      case a of
                                        Left msg -> retFail msg
                                        _ ->  do resolCascades (fst' g) ys ys' k  (filterT (fun2 k x))  x
                                                 resolNull (fst' g) zs zs' k x
                                                 retOk False

                     -- Filtro simple (evalúa igualdad entre registros)
               where fun2 k x y = case c k x y of
                         Eq s -> False
                         _ -> True

         -- Separar las restricciones de cada tabla referenciadora
         partRefDel [] = ([],[],[])
         partRefDel ((x,v,_):r) =   let (xs,ys,zs) = partRefDel r in
                                 case  v of
                                   Restricted -> (x:xs,ys,zs)
                                   Cascades -> (xs, x:ys, zs)
                                   Nullifies -> (xs,ys,x:zs)



-- Determina si los atributos k (clave primaria) son clave foránea en una lista de tablas
resolRestricted :: [String] -> [Tab] -> [String] -> Reg -> IO (Either String () )
resolRestricted _ [] _ _ = retOk ()
resolRestricted (n:ns) (t:ts) k x = if isMember k x t then errorRestricted x n
                                    else resolRestricted ns ts k x


-- Esparce borrado o modificación sobre los registros cuya clave foránea es k de una lista de tablas
resolCascades :: Env  -> [String] -> [Tab] -> [String] -> (Tab -> Tab) -> Reg -> IO ()
resolCascades _ _ [] _ _  _ = return ()
resolCascades e (n:ns) (t:ts) k f x = do let (t',r) =  f t ||| url' e n
                                         b <- reWrite t' r
                                         b `deepseq` resolCascades e ns ts k f x



-- Convierte a Nulo los valores de las claves foráneas k de una lista de tablas
resolNull :: Env ->  [String] -> [Tab] -> [String] -> Reg -> IO ()
resolNull _ _ [] _ _ = return ()
resolNull e (n:ns) (t:ts) k x = do let (t',r) = mapT (fun k x) t ||| url' e n
                                   b <- reWrite t' r
                                   b `deepseq` resolNull e ns ts k x

              -- Comparar igualdad entre los valores de los atributos k
        where fun k x y = case c2 k x y of
                           EQ -> fun2 k y
                           _ -> y
              -- Volver Nulos los valores de los atributos k
              fun2 [] y = y
              fun2 (k:ks) y = fun2 ks (updateHM (\x -> Just $ Nulo) k y)




-- Actualizar tabla (primer nivel)
update :: State -> Table -> ([String],[Args]) -> BoolExp -> IO ()
update g n d exp =
   do let r = url' (fst' g) n -- obtener ruta de tabla
      [TS sch,TT typ,TR ref,TK key] <- loadInfoTable ["scheme","types","refBy","key"] (name (fst' g)) n -- cargar esquemas y tipos
      update' g d sch typ r n exp ref key

-- Actualizar tabla (segundo nivel)
update' g (k,v) sch typ r n exp ref key =
  let h = singleton n (fromList $ zip sch typ)
      at = trd' $ updateState3 g h  in
  case checkTyped typ v >> checkTypeBoolExp exp [n] at of -- chequear tipos de valores recibidos y expresión booleana
    Left msg -> putStrLn msg
    Right _ -> do  t <- obtainTable r n -- cargar tabla
                   case t of
                    Nothing -> putStrLn "Error al cargar tabla..."
                    Just t' -> do let vals = fromList $ zip k v
                                  let (xs,ys,zs) = partRefTable ref
                                  xs' <- obtainTableList (fst' g) xs
                                  ys' <- obtainTableList (fst' g) ys
                                  zs' <- obtainTableList (fst' g) zs
                                  h <- ioEitherMapT (fun key n g exp vals xs xs' ys ys' zs zs') t'
                                  case h of
                                    Right t'' -> reWrite t'' r
                                    Left msg -> putStrLn msg

                 -- Función para modificar valores en registro x
           where fun k s g exp vals xs xs' ys ys' zs zs' x =
                    do let r = singleton s x
                       h <- evalBoolExp [s] (updateState2 g r) exp
                       case h of
                         Left msg -> retFail msg
                         Right b ->  if b then do a <- resolRestricted xs xs' k x
                                                  case a of
                                                    Left msg -> retFail msg
                                                    _ -> do resolCascades (fst' g) ys ys' k (mapT (fun2 vals k x)) x
                                                            resolNull (fst' g) zs zs' k x
                                                            retOk $ mapWithKey (fun vals) x
                                     else retOk x

                          -- Actualiza el valor del atributo k
                    where fun vals k v = case lookup k vals of
                                         Nothing -> v
                                         Just v' -> v'
                          fun2 vals k x y = case c2 k x y of
                                             EQ -> mapWithKey (fun vals) y
                                             _ -> y

-- Separa una lista de tablas, clasificandolas segun sus opciones de referencia
partRefTable [] = ([],[],[])
partRefTable ((x,_,v):r) =   let (xs,ys,zs) = partRefTable r in
                           case  v of
                            Restricted -> (x:xs,ys,zs)
                            Cascades -> (xs, x:ys, zs)
                            Nullifies -> (xs,ys,x:zs)

-- Toma una lista de nombres de tabla y devuelve una lista de tablas
obtainTableList _ [] = return []
obtainTableList e (x:xs) = do let r = url' e x
                              t <- obtainTable r x
                              case t of
                               Nothing -> error r
                               Just t' -> do ts' <- obtainTableList e xs
                                             return (t':ts')




evalBoolExp :: [String] -> State -> BoolExp -> IO(Either String Bool)


evalBoolExp s g (And exp1 exp2)  = do  b1' <- (evalBoolExp s g exp1)
                                       b2' <- (evalBoolExp s g exp2)
                                       return $ do
                                       b1 <- b1'
                                       b2 <- b2'
                                       ok $ b1 && b2


evalBoolExp s g (Or exp1 exp2)   = do  b1' <- (evalBoolExp s g exp1)
                                       b2' <- (evalBoolExp s g exp2)
                                       return $ do
                                       b1 <- b1'
                                       b2 <- b2'
                                       ok $ b1 || b2


evalBoolExp s g (Equal exp1 exp2) = retOk $ evalBoolExp' (==) exp1 exp2 s (snd' g)

evalBoolExp s g (Great exp1 exp2) = retOk $ evalBoolExp' (>)  exp1 exp2 s (snd' g)

evalBoolExp s g (Less  exp1 exp2) = retOk $ evalBoolExp' (<)  exp1 exp2 s (snd' g)


-- Determina si la consulta dml es vacía en el contexto g
evalBoolExp s g (Exist dml) = do let c = conversion  dml
                                 r <- runQuery' g c
                                 return $ do
                                 (_,_,_,_,[t]) <- r
                                 ok $ not $ isEmpty t

-- Determina si el valor referido por el campo v pertenece a l
evalBoolExp s g (InV (Field v) l) = do let r = snd' g
                                       return $ do
                                       x <- lookupList r s v
                                       return $ memb x l

        -- Determina si y pertenece a la lista
 where  memb y [] = False
        memb y (x:xs) = if y == x then True else memb y xs

-- Proyecta los atributos solicitados en l1 en un registro y lo busca en la consulta dml
evalBoolExp s g (InS l1 dml) =
  do let c = conversion dml
     r <- runQuery' g c
     return $ do
     (_,_,_,l2,[t]) <- r
     m <- buildRegister s l1 l2 (snd' g)
     ok $ isMember l2 m t

          -- Construye el registro solicitado
    where buildRegister _ [] _ _ = return $ emptyHM
          buildRegister _ _ [] _ = errorSigma3
          buildRegister s ((Field v):vs) (y:ys) r =
            if v == y then case lookupList r s v of
                             Right x -> do m <- buildRegister s vs ys r
                                           return $ union (singleton v x) m
                             Left _ -> error "buildRegister error"

            else  errorSigma2

evalBoolExp s g (Like f p) = return $ do
                             t <- checkTypeExp s (trd' g) f
                             if t == String then let (A1 v) = obtainValue s f (snd' g) in
                                                  ok $ findPattern v p
                             else errorLike

  where findPattern [] [] = True
        findPattern (x:xs) ('_':ys) = findPattern xs ys -- '_' representa un solo caracter
        findPattern xs ['%'] = True -- '%' representa una cantidad indefinida de caracteres
        findPattern xs ('%':y:ys) = findPattern (dropWhile (\x -> x /= y) xs) (y:ys) -- consumir una cantidad indefinida de caracteres hasta encontrar el siguiente
        findPattern (x:xs) (y:ys) = if x == y then findPattern xs ys
                                    else False
        findPattern _ _ = False


evalBoolExp' :: (Args -> Args -> Bool) -> Args -> Args -> [String] -> TabReg ->  Bool
evalBoolExp' o (A3 n) exp2 s t = evalBoolExp' o (A4 $ fromIntegral n) exp2 s t

evalBoolExp' o exp1 (A3 n)  s  t = evalBoolExp' o exp1  (A4 $ fromIntegral n) s t


evalBoolExp' o exp1 exp2 s t = case (isFieldOrDot exp1,isFieldOrDot exp2) of
                                (True,True) -> evalBoolExp' o (obtainValue s exp1 t) (obtainValue s exp2 t) s t
                                (True,False) -> evalBoolExp' o (obtainValue s exp1 t) exp2 s t
                                (False,True) -> evalBoolExp' o exp1 (obtainValue s exp2 t) s t
                                _ -> o exp1 exp2

      where isFieldOrDot (Field _) = True
            isFieldOrDot (Dot _ _) = True
            isFieldOrDot _ = False


-- Función para obtener valor de variables de tupla
obtainValue :: [String] -> Args -> TabReg -> Args
obtainValue s (Field v) r = case lookupList r s v of
                              Right x -> x
                              _ -> error "ObtainValue error"
obtainValue _ (Dot s2 v) r = case lookupList r [s2] v of
                               Right x -> x
                               _ -> error "ObtainValue error"







--- Ejecutar consultas



insertCola (n,x) [] = [(n,x)]
insertCola (n,x) c@((n',x'):xs) = if n < n' then (n',x'): (insertCola (n,x) xs)
                                  else (n,x):c

-- Convierte un árbol de consulta en una cola para poder hacer una ejecucion secuencial
-- de la misma
conversion :: DML ->  Cola AR
conversion (Union d1 d2) = let (a1,a2) = conversion d1 ||| conversion d2
                           in [(0,Uni a1 a2)]

conversion (Diff d1 d2) = let (a1,a2) = conversion d1 ||| conversion d2
                           in [(0,Dif a1 a2)]

conversion (Intersect d1 d2) = let (a1,a2) = conversion d1 ||| conversion d2
                               in [(0,Inters a1 a2)]


conversion (Select dist args dml) = insertCola (5,Pi dist args) $ conversion dml
conversion (From args dml) = insertCola (1,Prod args) $ conversion dml
conversion (Where boolExp dml) = insertCola (2,Sigma boolExp) $ conversion dml
conversion (GroupBy args dml) = insertCola (3,Group args) $ conversion dml
conversion (Having boolExp dml) = insertCola (4,Hav boolExp) $ conversion dml
conversion (OrderBy args ord dml) = insertCola (6,Order args ord) $ conversion dml
conversion (Limit n dml) = insertCola (7,Top n) $ conversion dml
conversion (End) = []

-- Ejecuta la consulta dml en el estado global  g, imprimiendo los resultados
runQuery :: State -> DML -> IO ()
runQuery g dml = do let c = conversion dml
                    v <- runQuery' g c
                    case v of
                     Left msg -> putStrLn msg
                     Right (_,_,_,ys,[t]) -> printTable ys t


withOutAgg :: State -> [String] -> Args -> IO(Either String Reg)
withOutAgg g s (Field v)  = return $ do
                            x <- lookupList (snd' g) s v
                            ok $ singleton v x


--Subconsulta en selección
withOutAgg g s (As (Subquery dml) (Field n)) =
  do let c = conversion dml
     r <- runQuery' g c
     return $ do
     (_,_,_,l,[t]) <- r
     case isSingletonT t && length l == 1 of
           True -> do  let (k,r) = l !! 0 ||| value t
                       let v = fromJust (lookup k r)
                       ok $ singleton n v

           False -> errorPi4





--Renombre de selección
withOutAgg g s (As exp (Field v2)) =  do r <- withOutAgg g s exp
                                         return $ do
                                         r2 <- r
                                         case elems r2 of
                                          [v] -> return $ singleton v2 v
                                          _ -> errorAs



-- Operador '.'
withOutAgg g _ e@(Dot s2 v) = return $
                              do  x <- lookupList (snd' g) [s2] v
                                  ok $ singleton (show2 e)  x


-- Clausula ALL no permitida con funciones de agregado
withOutAgg _ _ (All)  = errorPi3


-- Expresiones enteras
withOutAgg g s q = return $
                   do  n <- evalIntExp (snd' g) s q
                       case isInt n of
                        True -> ok $ singleton (show2 q) (A3 (round n))
                        _ -> ok $ singleton (show2 q) (A4 n)



-- Procesamiento de selecciones para el caso con funciones de agregado
withAgg  :: [Args] -> [Tab] -> ([String],AVL (HashMap String Args))
withAgg  ls ts = let s =  map (processArgs ls) ts
                     ls' = map show2 ls
                 in (ls',toTree s)
  where processArgs [] t = empty

        processArgs ((Field s):xs) t = let (m1,m2) = processArgs xs t ||| singleton s  ((value t) ! s)
                                       in union m1 m2

        processArgs ((A2 f):xs) t = let (m1,m2) = processArgs xs t ||| singleton (show3 f) (evalAgg (A2 f) t)
                                    in union m1 m2

        processArgs ((As s1 s2 ):xs) t = let m = processArgs (s1:xs) t
                                         in updateKey (show s1) (show s2) m





evalIntExp :: TabReg -> [String] -> Args -> Either String Float
evalIntExp g s (Plus exp1 exp2) = evalIntExp' False (+) g s exp1 exp2
evalIntExp g s (Minus exp1 exp2) = evalIntExp' False (-) g s exp1 exp2
evalIntExp g s (Times exp1 exp2) = evalIntExp' False (*) g s exp1 exp2
evalIntExp g s (Div exp1 exp2) = evalIntExp' True (/) g s exp1 exp2

evalIntExp g s (Negate exp1) = do n1 <- evalIntExp g s exp1
                                  return $ negate n1

evalIntExp g s (Brack exp1) = do n1 <- evalIntExp g s exp1
                                 return n1

evalIntExp g s (Field v) = do x <- lookupList g s v
                              evalIntExp g s x

evalIntExp g s (Dot s2 v) = do x <- lookupList g [s2] v
                               evalIntExp g s x

evalIntExp _ _ (A3 n) = return $ fromIntegral n
evalIntExp _ _ (A4 n) = return n


evalIntExp' :: Bool -> (Float -> Float -> Float) -> TabReg -> [String] -> Args -> Args -> Either String Float
evalIntExp' b o g s exp1 exp2 = do case b of
                                     True -> do n2 <- evalIntExp g s exp2
                                                case n2 == 0 of
                                                   True -> divisionError
                                                   False -> do n1 <- evalIntExp g s exp1
                                                               return $ o n1 n2
                                     False -> do n1 <- evalIntExp g s exp1
                                                 n2 <- evalIntExp g s exp2
                                                 return $ o n1 n2







-- Ejecución de consultas
runQuery' :: State -> Cola AR -> IO(Either String Answer)

runQuery' g [] = retOk (g,False,[],[],[])


runQuery' g ((_,Uni a1 a2):xs) = runQuery'' g a1 a2 xs unionT
runQuery' g ((_,Inters a1 a2):xs) = runQuery'' g a1 a2 xs intersectionT
runQuery' g ((_,Dif a1 a2):xs) = runQuery'' g a1 a2 xs differenceT




-- Ejecuta proyecciones
runQuery' g ((_,Pi d ls):rs) =
 do r <- runQuery' g rs
    case r of
     Right (g',b,l1,l2,ss) -> do let s = isNull ss
                                 case b of
-- Si no hay una claúsula group by no se pueden mezclar funciones de agregado con selecciones comunes
                                  False -> do case ls == [All] of
                                                 True -> distinct g' d l1 l2 (head s)
                                                 False -> do case partition isAgg ls of -- Separar funciones de agregado de selecciones comunes
                                                              (xs,[]) -> do let (ls',s') = withAgg ls s -- Caso con funciones de agregado
                                                                            distinct g' d l1 ls' s'

                                                              ([],xs) -> case checkTypedExpList l1 (trd' g') ls of
                                                                                Right _ -> do let u = name (fst' g')
                                                                                              -- Aplicar proyecciones sobre tabla
                                                                                              s2 <- ioEitherMapT (proy u l1 g ls) (head s)
                                                                                              case s2 of
                                                                                               Right s3 -> do let ls'= map show2 ls
                                                                                                              distinct g' d l1 ls' s3
                                                                                               Left q -> return $ Left q -- Caso sin funciones de agregado
                                                                                Left q -> return $ Left q



                                                              _ -> errorPi2   -- Error al mezclar las sentencias

-- En este caso se uso la claúsula group by
                                  True -> do case partition isAgg ls of
                                              (_,[]) -> aux g' d l1 ls s
                                              (_,xs) -> case All `elem` xs of
                                                         True -> errorPi3
                                                         False -> do let xs' = map show2 xs
                                                                     case partition (belong l2) xs'  of
                                                                      (_,[]) -> aux g' d l1 xs s
                                                                      (_,s) -> errorPi s


     Left msg -> retFail msg

  where -- proy toma un registro, lo divide y aplica proyecciones
        proy u l1 g ls x = do -- Agregar valores al estado (sirve para subconsultas y para evitar ambiguedad con atributos del mismo nobmre)
                              r <- divideRegister l1 u x
                              lr <- sequence $ map (withOutAgg (updateState2 g r) l1) ls
                              return $ tryJoin union emptyHM lr

        belong ys x = x `elem` ys

        isAgg (A2 _) = True
        isAgg (As e1 _) = isAgg e1
        isAgg _ = False
        aux g d l1 l2 s = do let (l2',s') = withAgg l2 s
                             distinct g d l1 l2' s'

        -- Eliminar registros duplicados si se solicita
        distinct g b l1 l2 t = do case b of
                                    False -> return $ Right $ (g,False,l1,l2,[t])
                                    True -> do let t' = mergeT (c l2) E t
                                               return $ Right $ (g,True,l1,l2,[t'])
        checkTypedExpList _ _ [] = return ()
        checkTypedExpList l r (x:xs) = do checkTypeExp l r x; checkTypedExpList l r xs
        isNull ss = if null ss then [emptyT]
                    else ss

-- Ejecuta producto cartesiano
runQuery' g ((_,Prod ls):xs) =
  do r <- exProd g ls
     return $ do (g',xs,ys,t) <- r
                 Right $ (g',False,xs,ys,[t])

-- Ejecuta selección
runQuery' g ((_,Sigma exp):xs) =
 do r <- runQuery' g xs
    case r of
      Left msg -> retFail msg

      Right (g',b,l1,l2,[t]) -> case checkTypeBoolExp exp l1 (trd' g') of
                                  Left msg -> retFail msg
                                  _ -> do h <- ioEitherFilterT (\x-> do let u = name (fst' g')
                                                                        r <-  divideRegister l1 u x
                                                                        evalBoolExp l1 (updateState2 g' r)  exp) t
                                          case h of
                                           Left msg -> retFail msg
                                           Right t' -> retOk (g',b,l1,l2,[t'])

-- Agrupa por registros
runQuery' g ((_,Group args):xs) =
  do r <- runQuery' g xs
     return $ do (g',b,l1,l2,[s]) <- r
                 let ls = map show2 args
                 case partition (\x -> x `elem` l2) ls  of
                  (_,[]) -> do let s' = group ls s
                               return (g',True,l1,ls,s')

                  (_,p) -> errorGroup p

runQuery' g ((_,Hav exp):xs) =
  do r <- runQuery' g xs
     case r of
      Left msg -> retFail msg
      Right (g',b,l1,l2,s) -> do l' <-  ioEitherFilterL (f l1 g' exp) s
                                 return $ do
                                 l <- l'
                                 return $ (g',b,l1,l2,l)


 where f y g e t = do let e' = replaceAgg e t
                      evalBoolExp y g e'



runQuery' g ((_,Order ls ord):xs) =
  do r <- runQuery' g xs
     return $ do (g',b,l1,l2,[s]) <- r
                 let ls' = map show ls
                 case partition (\x -> x `elem` l2) ls'  of
                  (_,[]) -> case ord of
                             A -> ret g' l1 l2 s (compareAsc ls')
                             D -> ret g' l1 l2 s (compareDesc ls')
                  (_,s) -> errorOrder s


  where ret g' l1 l2 t f = do let t' = sortedT f t
                              return $ (g',False,l1,l2,[t'])
        compareAsc (x:xs) m1 m2 = let (e1,e2) = (m1 ! x) ||| (m2 ! x) in
                                case e1 `compare` e2  of
                                 LT -> Lt
                                 GT -> Gt
                                 EQ -> if null xs then Lt
                                       else compareAsc xs m1 m2
        compareDesc (x:xs) m1 m2 = compareAsc (x:xs) m2 m1

runQuery' g ((_,Top n):xs) =
   do r <- runQuery' g xs
      case r of
        Left msg -> retFail msg
        Right q@(g',b,l1,l2,s) -> if length s /= 1  then errorTop
                                  else if n < 0 then errorTop2
                                       else case splitAtL n (head s) of
                                             Left _ -> retOk q
                                             Right (t',_) -> retOk (g',b,l1,l2,[t'])





-- Segundo nivel (operaciones de conjuntos)
runQuery'' g a1 a2 xs op =
 do let (r1',r2') = runQuery' g a1 ||| runQuery' g a2
    r1 <- r1'
    r2 <- r2'
    return $ do
    (g1,_,l11,l12,s1) <- r1
    (g2,_,l21,l22,s2) <- r2
    if notUnique s1 || notUnique s2 then errorSet
    else if notEqualLen l12 l22 then errorSet2
         else if not $ equalType (trd' g1) (trd' g2) l11 l21 l12 l22  then errorSet3
              else do let ls =  unionL l11 l21 -- unir tablas relacionadas
                      let g12 =  updateState2 g1 (snd' g2) -- unir valor de variables de tupla
                      let g13 =  updateState3 g12 (trd' g2)    -- unir tipo de variables de tupla
                      let (t1,t2) = sortedT (c l12) (head s1) ||| sortedT (c l12) (replaceAllKeys l22 l12 (head s2)) -- Ordenar árboles para hacer una unión ordenada (mejor eficiencia)
                      -- ambos entornos son iguales, tomamos el primero
                      ok (g13,False,ls,l12,[op (c l12) t1 t2])

 where notUnique s = not $ length s == 1

       notEqualLen l1 l2 = not $ length l1 == length l2

       equalType _ _ _ _ [] [] = True
       equalType e1 e2 l11 l21 (x:xs) (y:ys) = if lookupList e1 l11 x == lookupList e2 l21 y then equalType e1 e2 l11 l21 xs ys
                                               else False
       equalType _ _ _ _ _ _ = False

       replaceAllKeys  [] _ t = t
       replaceAllKeys (x:xs) (y:ys) t = if x /= y then replaceAllKeys xs ys (mapT (updateKey x y) t)
                                        else replaceAllKeys xs ys t







-- Obtiene las tablas y hace el producto cartesiano cuando se requiere
exProd :: State -> [Args] -> IO (Either String (State,TabNames,FieldNames,Tab))
exProd _ [] = retFail "Sin argumentos"


-- Caso una sola tabla
exProd g ([s]) = exProd' g s


-- Caso 2 tablas
exProd g ([s1,s2]) =
 do v1 <- exProd' g s1
    v2 <- exProd' g s2
    return $ do (g1,l1'@[s'],l2',t') <- v1
                (g2,l1@[s],l2,t) <- v2
                -- Agregar nombre de tabla a los atributos (evita ambiguedad)
                let (t1,t2) = mapT (changeKey s l2) t||| mapT (changeKey s' l2') t'
                let (k2,k2') = map (f2 s) l2 ||| map (f2 s') l2'
                let (l11',k22') = (l1 ++ l1') ||| (k2 ++ k2')
                return (updateState3 g1 (trd' g2),l11',k22',prod k22' t1 t2)

-- Caso 3 o más tablas
exProd g (s:ls) =
 do r <- exProd  g ls
    v <- exProd' g s
    return $do (g1,l1',l2',t') <- r
               (g2,l1@[s],l2,t) <- v
               let t1 = mapT (changeKey s l2) t
               let (l11',l22') = (l1 ++ l1',(map (f2 s) l2) ++ l2')
               return (updateState3 g1 (trd' g2),l11',l22', prod l22' t1 t')


-- Cambiar llaves de los registros
changeKey _ [] _ = emptyHM
changeKey s (x:xs) r = union ((singleton (f2 s x)) (r ! x)) (changeKey s xs r)
f2 s x =  s ++ "." ++ x



-- Intenta obtener una tabla


-- Primer caso : ejecuta una subconsulta
exProd' :: State -> Args -> IO(Either String (State,TabNames,FieldNames,Tab))
exProd' g (As (Subquery s) (Field n)) = do let c = conversion s
                                           r <- runQuery' g c
                                           return $ do (g',_,ys,xs,t) <- r
                                                       return $ (g',ys,xs,head t)


-- Segundo caso :
exProd' g s = do let s' = readArgs s
                 let m = fst s'
                 let e = fst' g
                 let (r,n) = (url' e m) ||| name e
                 -- Obtener relación
                 v <- obtainTable r m
                 -- Obtener esquema y tipos
                 inf <- loadInfoTable ["scheme","types"] n m
                 case isJust v   of
                       False -> errorProd m
                       _ -> do let n = snd s'
                               let [TS f,TT t] = inf
                               let r = singleton n (fromList $ zip f t)
                               retOk  (updateState3 g r ,[n],f,fromJust v)


    where  readArgs (Field s) = (s,s)
           readArgs (As (Field s1) (Field s2)) = (s1,s2)


-- Realiza el producto entre registros
prod :: [String] -> Tab -> Tab -> Tab
prod _ E t = E
prod _ t E = E
prod l t1 t2 = let (tl,tr) = prod l (left t1) t2 ||| prod l (right t1) t2
                   t = mergeT (c l) tl tr
             in mergeT (c l) t (prod' (value t1) t2)
  where prod' x t = mapT (union x) t





-- Actualiza el estado del entorno
updateState ::  State -> Env -> State
updateState g e = (e,snd' g,trd' g)


-- Actualiza los valores de las variables de tupla
updateState2 :: State -> TabReg -> State
updateState2 g x = (fst' g,union x (snd' g),trd' g)

-- Actualiza los tipos de las variables de tupla
updateState3 :: State -> TabTypes -> State
updateState3 g y =  (fst' g,snd' g,union y (trd' g))



printTable :: [String] -> AVL (HashMap String Args) -> IO ()
printTable s t =
  do r <- size
     case r of
       Nothing -> error ""
       Just w -> do putStrLn $ line $ width w
                    putStrLn  $ fold s
                    putStrLn $ line $ width w
                    sequence_ $ mapT (\ x -> putStrLn $ fold $ map (f' x) s) t
 where fold s = txt 0 s 30
       f x y = x ++ "|" ++ y
       f' x v =  show2 $ x ! v
       txt _ [] _ = ""
       txt 0 (x:xs) n  = "|" ++ x ++  txt (n - length x) xs n
       txt c v n = " " ++ txt (c-1) v n
       line 0 = ""
       line n = '-': (line (n-1))






-- Remplaza funciones de agregado por los valores correspondientes en una expresión boleana
replaceAgg ::BoolExp -> AVL (HashMap String Args) -> BoolExp
replaceAgg (And e1 e2) t = And (replaceAgg e1 t) (replaceAgg  e2 t)
replaceAgg (Or e1 e2) t = Or (replaceAgg  e1 t) (replaceAgg  e2 t)
replaceAgg (Less e1 e2) t =  Less   (evalAgg  e1 t) (evalAgg  e2 t)
replaceAgg (Great e1 e2) t = Great  (evalAgg  e1 t) (evalAgg  e2 t)
replaceAgg (Equal e1 e2) t = Equal  (evalAgg  e1 t) (evalAgg  e2 t)



evalAgg :: Args -> Tab -> Args
evalAgg (A2 f) t = A4 $ evalAgg' f t
evalAgg e t = e

evalAgg' (Count d (A1 s)) t = evalAgg'' s d t 0 count
  where count = \ x y z -> 1 + y + z

evalAgg' (Min d s) t =  evalAgg'' s d t posInf min3
  where min3 = \ x l r -> min (toFloat $ x ! s) (min l r)

evalAgg' (Max d s) t =  evalAgg'' s d t negInf max3
  where max3 = \ x l r -> max (toFloat $ x ! s) (max l r)

evalAgg' (Sum d s) t = evalAgg'' s d t 0 sum
  where sum = \ x y z -> (toFloat $ x ! s) + y + z

evalAgg' (Avg d s) t = let (n1,n2) = evalAgg' (Sum d s) t ||| evalAgg' (Count d (A1 s)) t
                        in n1 / n2



evalAgg'' s b  t e f = if b then let t' = mergeT (c [s]) E t
                                in foldT f e t'
                       else foldT f e t



toFloat :: Args -> Float
toFloat (A3 n) = fromIntegral n
toFloat (A4 n) = n

posInf :: Float
posInf = 1/0

negInf :: Float
negInf = - 1/0

-- Elimina elementos duplicados si se solicita previamente a aplicar la función de agregado


-- Toma una lista de atributos y un árbol y devuelve una
-- descomposición del árbol en base a la lista
group :: [String] -> AVL (HashMap String Args) -> [AVL (HashMap String Args)]
group _ E = []
group xs t = let a = value t
                 v = map (\x -> (x,a ! x)) xs
                 (t1,t2) = particionT (equality v) t
             in t2 : group xs t1

      where equality [] _ = True
            equality ((x,y):xs) s = if s ! x /= y then False
                                    else equality xs s




-- Actualiza el valor de la llave k2 a k1 en m
updateKey  ::  (Eq k, Hashable k) => k -> k -> HashMap k v -> HashMap k v
updateKey k1 k2 m = let m' = insertH k2 (m ! k1) m
                    in deleteH k1 m'


-- Actualiza el valor de las llaves k1 a k2 en cada elemento de t
{-updateKeyT :: [String] -> String -> Tab -> Tab
updateKeyT k1 k2 t = mapT (fun k1 k2) t
 where fun [] [] _ = emptyHM
       fun (x:xs) (y:ys) r = fun xs ys (updateKey y x r)-}





-- Crear rutas de acceso alternativas hacias los datos
divideRegister :: [String] -> String -> Reg -> IO(TabReg)
divideRegister [] _ _ = return emptyHM
-- Caso 1 tabla (todos los atributos son de s)
divideRegister [s] _ r = return $ singleton s r

-- Caso + de 1 tabla (si hay 2 atributos iguales no se sabe a que tabla pertenecen)
divideRegister s u r = divideRegister1 s u r

-- segundo nivel
divideRegister1 [] _ _ = return emptyHM
divideRegister1 (x:xs) u r = do s' <- loadInfoTable ["scheme"] u x
                                let [TS s]  = s'
                                let m = divideRegister2 x s r
                                rs <- divideRegister1 xs u r
                                return $ union (singleton x m) rs

-- tercer nivel (buscar todos los atributos asociados a la tabla x )
divideRegister2 x [] r = emptyHM
divideRegister2 x (y:ys) r = case lookup y r of
                              Nothing -> case lookup (x ++ "." ++ y) r of
                                          Nothing -> emptyHM
                                          Just v -> union (singleton y v) (divideRegister2 x ys r)



tryJoin :: (e -> b -> b) -> b -> [Either a e] -> Either a b
tryJoin u e [] = return e
tryJoin u e ((Left x):xs) = Left x
tryJoin u e ((Right x):xs) = do xs' <- tryJoin u e xs
                                return (u x xs')


ioEitherFilterL :: (e -> IO (Either String Bool)) -> [e] -> IO(Either String [e])
ioEitherFilterL f [] = retOk []
ioEitherFilterL f (x:xs) = do r' <- ioEitherFilterL f xs
                              b' <- f x
                              return $ do
                              r <- r'
                              b <- b'
                              if b then ok (x:r)
                              else ok r
