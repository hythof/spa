module AST where

data AST = Root
    | RawTag String
    | Tag String [(String, String)]
    | Var String
    | Stmt String
    deriving (Show, Eq)
