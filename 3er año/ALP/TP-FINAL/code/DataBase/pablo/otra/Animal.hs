module Animal where 
import AST (Args (..))
import Data.Typeable
import Avl
data Animal = Animal {raza::String,altura::Int} deriving Show
ref = Animal "" 0 
t = [typeOf(raza ref),typeOf(altura ref)]
n = 5
f = split [1,2,3,4]

upd0 = Empty
upd1 = insert upd0 (Animal "Gato" 21)



recovery ::   -> AVL String
recovery =    

