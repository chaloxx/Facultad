module Error where
import Data.HashMap.Strict hiding (foldr,map)
import AST
import Prelude hiding (lookup,fail)
import Data.Typeable
import Avl ((|||),isMember)
import Data.Either

printError :: String -> IO ()
printError msg = error msg


errorMsg = "Nombre de usuario inválido"
errorMsg2 = "No existe el usuario"


combineEither :: Monoid a => (b -> c -> e) -> Either a b -> Either a c -> Either a e
combineEither op a b = case a of
                        Right x -> case b of
                                     Right y -> return $ op x y
                                     Left y -> Left y
                        Left x -> Left x



errorField1 x = Left (x ++ " no es un atributo válido\n")
errorField2 x t1 t2 = x ++ " tiene tipo " ++ (show t1) ++ " ,se esperaba tipo " ++ (show t2) ++ "\n"



typeOfArgs :: Args -> Type
typeOfArgs (A1 s) = String
typeOfArgs (A3 n) = Int
typeOfArgs (A4 f) = Float




errorPi s = retFail  ("Atributos " ++ show s ++
                   " inválidos en SELECT" )

errorPi2 = retFail ("No pueden mezclarse funciones de agregado y selecciones " ++
                 "sin haber usado la claúsula GROUP BY")

errorPi3 = retFail $ "Inválido uso de * en SELECT"

errorPi4 = fail $ "Error en subconsulta..."

errorPi5 s = fail $ "La tabla " ++ s ++ " no existe"

errorSigma s = fail $ "Atributo " ++ s ++ " inválido condición WHERE"

errorSigma2 = fail $ "El orden de los atributos es inválido.."

errorSigma3 = fail $ "Demasiados atributos en condición IN"

errorGroup s =  fail $ "Atributos " ++ show s ++ " inválidos en GROUP BY"

errorOrder s = fail $ "Atributos " ++ show s ++ " inválidos en ORDER BY"

errorProd s = return $ fail $ "La tabla " ++ s ++ " no existe"

errorAs = fail $ "Error en sentencia AS"

errorEvalBool s = fail $ "Atributo " ++ s " inválido"


errorFind s = fail $ "No se pudo encontrar el atributo " ++ s

typeError e = fail $ "Error de tipo en la expresion " ++ e ++ "\n"

divisionError = fail $ "División por cero"

errorKey x = fail $ "La clave de " ++ (fold x) ++ " ya existe"

errorSource = "Error en la extensión del archivo"

errorOpen p err =  "Warning: Couldn't open " ++ p ++ ": " ++ err

errorSelUser = "Primero debe seleccionar un usuario..."

errorSelBase = "Primero debe seleccionar una base de datos..."

welcome n = "Bienvenido " ++ n ++ "!"

logError n = do put $ "No existe el usuario " ++ n  ++ " o la contraseña es incorrecta "
                return (Env "" "")

errorComClose = Failed $ "Error de clausura de comentario"

errorComOpen =  Failed $ "Error de apertura de comentario"

errorForKey = fail $ "El valor referenciado no existe"

errorCreateTable = "La clave o la clave foránea no son parte del esquema"

errorCheckReference = put $ "Error chequeando las referencias"

succesCreateReference n s = put $ "Referencia creada entre " ++ n ++ " y " ++ s ++ " con éxito.."

errorCreateReference = put  "La clave foránea no coincide con la clave de la tabla referenciada"

errorDropTable s = "La tabla " ++ s ++ " no existe.."

succesDropTable s = "La tabla " ++ s ++ " fue eliminada con éxito"

succesCreateTable s = put $ "La tabla " ++ s ++ " fue creada con éxito"

errorRestricted x n =  let k = keys x
                           l = map (\s -> x ! s) k
                       in retFail $ "No se puede modificar o eliminar (" ++ fold l  ++ ") pues es un elemento referenciado por la tabla " ++ n

errorTop = retFail "La claúsula LIMIT solo se puede aplicar a una tabla"
errorTop2 = retFail "La claúsula LIMIT solo se puede usar con números no negativos"


errorSet = fail "Error en unión"
errorSet2 = fail "Relaciones con distinto número de atributos"
errorSet3 = fail "Atributos con tipos incompatibles"

errorLike = fail "Los argumentos de Like deben ser strings"

fail = Left
retFail :: String ->  IO(Either String b)
retFail = return.fail

put = putStrLn


ok = Right
retOk :: a -> IO(Either String a)
retOk = return.ok

exitToInsert :: [Args] -> Either String String
exitToInsert s = return $  (fold s) ++ " se inserto correctamente"


fold :: [Args] -> String
fold s = tail $ fold' $ map show2 s
 where fold' = foldl (\ x y -> x ++ "," ++ y) ""

msg = "Error buscando el objeto"

-- Realiza una busqueda en g a partir de una lista y v
lookupList ::Show b => HashMap String (HashMap String b) -> [String] -> String -> Either String b
lookupList _ [] v = errorFind v
lookupList g (y:ys) v = case lookup y g of
                          Nothing -> error (show g)
                          Just r -> case lookup v r of
                                     Nothing -> lookupList g ys v
                                     Just x' -> return x'

-- Unir 2 listas, sin elementos repetidos en ambas
unionL :: Eq e => [e] -> [e] -> [e]
unionL [] l = l
unionL (x:xs) l = if x `elem` l then unionL xs l
                  else unionL xs (x:l)




tablePath = "DataBase/system/Catalogo/Tables"
userPath = "DataBase/system/Catalogo/Users"
