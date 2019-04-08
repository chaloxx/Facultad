module Trabajador where 
import AST (Args (..),Type(..),TableDescript(..))
import Data.Typeable
import Data.HashMap.Strict hiding (keys) 
import Avl (AVL(..),m)
import COrdering
keys = ["dni"]

upd0 = E
upd1 = m keys upd0 (Z E (fromList [("dni",A3 1),("cod",A3 1),("nombre",A1 "Juan")]) E)
upd2 = m keys upd1 (Z E (fromList [("dni",A3 2),("cod",A3 2),("nombre",A1 "Juan")]) E)
upd3 = m keys upd2 (Z E (fromList [("dni",A3 3),("cod",A3 2),("nombre",A1 "Diego")]) E)
upd4 = m keys upd3 (Z E (fromList [("dni",A3 4),("cod",A3 3),("nombre",A1 "Marcelo")]) E)