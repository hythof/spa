module AST where

data AST = Empty
    | RawTag String
    | Tag String [(String, String)]
    | Var String
    | Stmt String String
    deriving (Show, Eq)
