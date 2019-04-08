module Tables where 
import AST (Args (..),Type(..),TableDescript(..),RefOption(..))
import Data.Typeable
import Data.HashMap.Strict hiding (keys) 
import Avl
import COrdering
keys = ["owner","tableName"]

upd0 = E
upd1 = m keys upd0 (Z E (fromList [("types",TT [Int]),("fkey",TFK []),("refBy",TR [("Trabajador",Cascades,Restricted)]),("key",TK ["cod"]),("tableName",TN "Oficina"),("scheme",TS ["cod"]),("owner",TO "hernan")]) E)

upd2 = m keys upd1 (Z E (fromList [("types",TT [Int,String,Int]),("fkey",TFK [("Oficina",["cod"])]),("refBy",TR []),("key",TK ["dni"]),("tableName",TN "Trabajador"),("scheme",TS ["dni","nombre","cod"]),("owner",TO "hernan")]) E)
upd3 = m keys upd2 (Z E (fromList [("types",TT [Int,String]),("fkey",TFK []),("refBy",TR []),("key",TK ["dni"]),("tableName",TN "Persona"),("scheme",TS ["dni","name"]),("owner",TO "hernan")]) E)