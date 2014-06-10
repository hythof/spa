module Build where

import Data.Tree (Tree (..))

import AST

data Node = Node {
    html :: Stirng
    js :: String
    css :: String
} deriving (Show, Eq, Ord, Read)

buildHtml (Node v ts) = build 0 v ts

build (Empty) = ""
build (RawTag name) = name
build (Var name) = "<span id=\"var_" ++ name ++ "\">"
build (Stmt name) = "<!-- " ++ name ++ " -->"
build (Tag name attrs) = "<" ++ name ++ kv(attrs) ++ ">"
    where
        kv [] = []
        kv [(k, v)] = " " ++ k ++ "=" ++ v
        kv (x:xs) = (kv [x]) ++ (kv xs)
