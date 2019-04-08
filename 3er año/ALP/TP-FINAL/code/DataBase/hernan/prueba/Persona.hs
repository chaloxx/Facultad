module Persona where 
import AST (Args (..),Type(..),TableDescript(..))
import Data.Typeable
import Data.HashMap.Strict hiding (keys) 
import Avl (AVL(..),m)
import COrdering
keys = ["dni"]

upd0 = E
upd1 = m keys upd0 (Z E (fromList [("dni",A3 1),("name",A1 "Diego")]) E)
upd2 = m keys upd1 (Z E (fromList [("dni",A3 2),("name",A1 "Marcelo")]) E)
upd3 = m keys upd2 (Z E (fromList [("dni",A3 3),("name",A1 "Gabriel")]) E)