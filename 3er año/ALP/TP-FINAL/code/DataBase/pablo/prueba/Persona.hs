module Persona where 
import AST (Args (..),Type(..))
import Data.Typeable
import Data.HashMap.Strict hiding (keys) 
import Avl
import COrdering
types = fromList [("nombre",String),("dni",Int)]
keys = ["dni"]
fields = ["nombre","dni"]

upd0 = E
upd1 = m keys upd0 (Z (Z E (fromList [("dni",A3 2),("nombre",A1 "Pedro")]) E) (fromList [("dni",A3 1),("nombre",A1 "Diego")]) (Z E (fromList [("dni",A3 3),("nombre",A1 "Pablo")]) E))