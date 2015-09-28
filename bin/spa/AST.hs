module AST where

data AST = Root
    | RawTag String
    | Tag String [(String, String)]
    | Var String
    | Const String
    deriving (Show, Eq)
