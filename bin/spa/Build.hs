module Build (
    buildHtml,
    buildJs
) where

import Data.Tree (Tree (..))
import Data.Char (isAlphaNum)
import qualified Data.Foldable as F
import AST

-- exports
buildHtml :: Tree AST -> String
buildHtml ast = strip $ htmlNode 0 ast

buildJs :: Tree AST -> String
buildJs ast = strip $ js ast

-- make html
restNode :: Int -> [Tree AST] -> String
restNode n xs = foldr (++) "" (map (htmlNode (n + 1)) xs)

htmlNode :: Int -> Tree AST -> String
htmlNode n (Node ast@(Empty) xs) = restNode n xs
htmlNode n (Node ast@(RawTag text) xs) = "\n" ++ (indent n) ++ (html ast) ++ (restNode (n - 1) xs)
htmlNode n (Node ast@(Var name) xs) = "\n" ++ (indent n) ++ (html ast) ++ (restNode n xs)
htmlNode n (Node ast@(Stmt stmt) _) = "\n" ++ (indent n) ++ (html ast) -- skip inner html, that maked js
htmlNode n (Node ast@(Tag name attrs) xs) = "\n" ++ (indent n) ++ (html ast) ++ (if (singleTag name) then tag1 else tag2)
  where
    tag1 = restNode n xs
    tag2 = if (length xs) == 0 then tag3 else tag4
    tag3 = "</" ++ name ++ ">\n"
    tag4 = (restNode n xs) ++ "\n" ++ (indent n) ++ "</" ++ name ++ ">"

html (Empty) = ""
html (RawTag text) = text
html (Var name) = "<span data-spa=" ++ (naming name) ++ "></span>"
html (Stmt stmt) = "<span data-spa=" ++ (naming stmt) ++ "></span>"
html (Tag name attrs) = "<" ++ name ++ (kv attrs) ++ ">"
  where
    kv [] = ""
    kv [(k, v)] = " " ++ k ++ "=" ++ (quote_attr v)
    kv (x:xs) = (kv [x]) ++ (kv xs)
    quote_attr :: String -> String
    quote_attr s = if (is_need_quote s) then "\"" ++ (safe_quote s) ++ "\"" else s


-- make js
js :: Tree AST -> String
js tree = strip $ fold tree
  where
    fold (Node (Stmt stmt) xs) = "\n" ++ "renders." ++ (naming stmt) ++ " = function(){var html=\"\";" ++ (addList xs) ++ "return html;};"
    fold (Node _ xs) = foldr (++) "" $ map fold xs
    addList xs = foldr (++) "" $ map add xs
    add (Node ast@(Empty) xs) = "" ++ addList xs
    add (Node ast@(RawTag text) xs) = "\nhtml += " ++ (safe_quote text) ++ ";" ++ addList xs
    add (Node ast@(Var name) xs) = "\nhtml += " ++ name ++ ";" ++ addList xs
    add (Node ast@(Stmt stmt) xs) = "\n" ++ (toJs stmt) ++ "\n" ++ (addList xs) ++ "\n}"
    add (Node ast@(Tag name attrs) xs) = "\n" ++ (safe_quote $ html ast) ++ if singleTag name then "" else "html += \"</" ++ name ++ "/>\";"
    toJs js = js

-- utility
naming :: String -> String
naming [] = []
naming (x:xs) = (if isAlphaNum x then x else '_') : naming xs

namingStatement :: String -> Tree AST -> String
namingStatement name tree = naming (name ++ F.foldr (\a b -> b ++ (show a)) "" tree)

have :: Eq a => a -> [a] -> Bool
have n [] = False
have n [x] = n == x
have n (x:xs) = n == x || have n xs

indent :: Int -> String
indent n = (take (n * 2) $ repeat ' ')

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip [] = []
lstrip (x:xs)
  | x == ' '  = lstrip xs
  | x == '\n' = lstrip xs
  | otherwise = x : xs

rstrip :: String -> String
rstrip [] = []
rstrip [x]
  | x == ' '  = ""
  | x == '\n' = ""
  | otherwise = [x]
rstrip (x:xs) = x : rstrip xs

singleTag :: String -> Bool
singleTag tag = have tag [
        "meta"
      , "br"
      , "hr"
      , "input"
    ]

safe_quote :: String -> String 
safe_quote [] = ""
safe_quote (x:xs) = (if x == '"' then '"' else x) : (safe_quote xs)
is_quoted :: String -> Bool
is_quoted [] = False
is_quoted [x] = False
is_quoted (x:xs) = ('"'  == x && '"'  == (xs !! ((length xs) - 1)))
            || ('\'' == x && '\'' == (xs !! ((length xs) - 1)))
is_need_quote :: String -> Bool
is_need_quote s = (not $ is_quoted s) && has_need_quote s
has_need_quote :: String -> Bool
has_need_quote s = (have '"' s)
           || (have '\'' s)
           || (have ' ' s)
           || (have '>' s)
           || (have '<' s)
           || (have '=' s)
