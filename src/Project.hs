module Project where

data Expr
    = N Int
    | B Bool
    | Unit
    | Var String
    | App Expr Expr
    | Abs String Expr
    | Seq Expr Expr
    | Let String Expr Expr
    | BO BinOper Expr Expr
    | Rec [(Label, Expr)]
    | Proj Expr Label
    | Ref Expr
    | Deref Expr
    | Assign String Expr
    | IfTE Expr Expr Expr
    | IfT Expr Expr
    | While Expr Expr
    | For String Expr Expr Expr
    deriving (Eq, Show)

data Label
    = LabelS String
    | LabelN Int
    deriving (Eq, Show)

data BinOper = Add | Sub | Mul | Div
    deriving (Eq, Show)

main :: IO ()
main = putStrLn "Hello, Haskell!"
