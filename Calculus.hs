module Calculus where 
import Data.Maybe 
{-Custom defined types representing equations-}
data UnOp = Neg | Sin | Cos | Tan | Sec | Cosec | Cot | Ln | Log Double 
            deriving (Eq, Ord, Show) 
  
data BinOp = Add | Mul | Div 
             deriving (Eq, Ord, Show) 
                  
data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp 
          deriving (Eq, Ord, Show) 

{--diff function differentiates: No implementation for inverse trig atm--}

diff :: Exp -> String -> Exp
diff (Val _) _  = Val 0
diff (Id x) y
    | x == y    = Val 1
    | otherwise = error "Invalid input, this function does not support implicit differentiation"
diff (UnApp Neg expr) ito = UnApp Neg (diff expr ito)
diff (UnApp Sin expr) ito = BinApp Mul (diff expr ito) (UnApp Cos expr)
diff (UnApp Cos expr) ito = UnApp Neg (BinApp Mul (diff expr ito) (UnApp Sin expr))
diff (UnApp Tan expr) ito = BinApp Mul (diff expr ito) (BinApp Mul (UnApp Sec expr) (UnApp Sec expr))
diff (UnApp Ln  expr) ito = BinApp Div (Val 1) (expr)
diff (UnApp (Log base) expr) ito = BinApp Div (Val 1) ((BinApp Mul (expr) (UnApp Ln (Val base))))
diff (BinApp Add expr1 expr2) ito = BinApp Add (diff expr1 ito) (diff expr2 ito)
diff (BinApp Mul expr1 expr2) ito = BinApp Add (BinApp Mul (expr1) (diff expr2 ito)) (BinApp Mul (diff expr1 ito) (expr2))
diff (BinApp Div expr1 expr2) ito = BinApp Div (BinApp Add (BinApp Mul (diff expr1 ito) (expr2)) (UnApp Neg (BinApp Mul (expr1) (diff expr2 ito)))) (BinApp Mul (expr2) (expr2)) 




{--integrate function integrates--}
integrate :: Exp -> String -> Exp
integrate = error "TODO: IMPLEMENT INTEGRATE"



