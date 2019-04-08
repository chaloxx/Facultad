module Oficina where 
import AST (Args (..),Type(..),TableDescript(..))
import Data.Typeable
import Data.HashMap.Strict hiding (keys) 
import Avl (AVL(..),m)
import COrdering
keys = ["cod"]

upd0 = E
upd1 = m keys upd0 (Z (Z E (fromList [("cod",A3 2)]) E) (fromList [("cod",A3 1)]) (Z E (fromList [("cod",A3 3)]) E))