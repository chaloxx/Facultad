module Users where 
import AST (Args (..),Type(..))
import Data.Typeable
import Data.HashMap.Strict hiding (keys) 
import Avl
import COrdering
types = fromList [("userName",String),("pass",String)]
keys = ["userName"]
fields = ["userName","pass"]

upd0 = E
upd1 = m keys upd0 (Z E (fromList [("pass","prueba"),("userName","hernan")]) E)
upd2 = m keys upd1 (Z E (fromList [("pass","mari"),("userName","mari")]) E)