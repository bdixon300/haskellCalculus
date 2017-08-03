module Calculus where 
import Data.Maybe 

data UnOp = Neg | Sin | Cos | Log 
            deriving (Eq, Ord, Show) 
  
data BinOp = Add | Mul | Div 
             deriving (Eq, Ord, Show) 
                  
data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp 
          deriving (Eq, Ord, Show) 

type Env = [(String, Double)]

diff :: Exp -> String -> Exp
diff = "TODO: IMPLEMENT DIFF"

integrate :: Exp -> String -> Exp
integrate = "TODO: IMPLEMENT INTEGRATE"



