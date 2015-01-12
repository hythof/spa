module Build (
    buildHtml,
    buildJs
) where

import Data.Tree (Tree (..))
import Data.Char (isAlphaNum)
import qualified Data.Foldable as F

import AST

-- export functions
buildHtml :: Tree AST -> String
buildHtml (Node Root xs) = foldr (\a b -> a ++ "\n" ++ b) "" $ map (\x -> strip $ htmlNode 0 x) xs

buildJs :: Tree AST -> String
buildJs (Node Root xs) = strip $ js xs

-- make html
restNode :: Int -> [Tree AST] -> String
restNode n xs = foldr (++) "" (map (htmlNode (n + 1)) xs)

htmlNode :: Int -> Tree AST -> String
htmlNode n (Node ast@(RawTag text) xs) = "\n" ++ (indent n) ++ (html ast) ++ (restNode (n - 1) xs)
htmlNode n (Node ast@(Tag name attrs) xs) = "\n" ++ (indent n) ++ (html ast) ++ (if (singleTag name) then tag1 else tag2)
  where
    tag1 = restNode n xs
    tag2 = if (length xs) == 0 then tag3 else tag4
    tag3 = "</" ++ name ++ ">\n"
    tag4 = (restNode n xs) ++ "\n" ++ (indent n) ++ "</" ++ name ++ ">"
htmlNode n (Node ast@(Var name) xs) = "\n" ++ (indent n) ++ (html ast) ++ (restNode n xs)
htmlNode n (Node ast@(Stmt stmt) _) = "\n" ++ (indent n) ++ (html ast) -- skip inner html, that maked js

-- make js
js :: [Tree AST] -> String
js xs = foldr (\a b -> a ++ "\n" ++ b) "" $ map render xs
  where
    render :: Tree AST -> String
    render ast@(Node (Stmt stmt) []) = ""
    render ast@(Node (Stmt stmt) _) = "\n" ++ "spa._render." ++ (naming stmt) ++ " = function(){var html=\"\";" ++ (add 0 ast) ++ "\nreturn html;};"
    render ast@(Node _ xs) = foldr (++) "" $ map render xs

    addList :: Int -> [Tree AST] -> String
    addList n xs = foldr (++) "" $ map (add n) xs

    add :: Int -> Tree AST -> String
    add n (Node ast@(RawTag text) xs) = "\nhtml += " ++ (dq text) ++ ";" ++ addList n xs
    add n (Node ast@(Tag name attrs) xs)
      | singleTag name = context
      | otherwise      = context ++ "\nhtml += \"</" ++ name ++ ">\";"
      where
        context = "\nhtml += " ++ (dq $ html ast) ++ ";" ++ addList n xs 
    add n (Node ast@(Var name) xs) = "\nhtml += " ++ name ++ ";" ++ addList n xs
    add n (Node ast@(Stmt stmt) xs) = "\n" ++ (renderStmt n stmt) ++ (addList nextN xs) ++ "\n}"
      where
        nextN = if startsWith "for" stmt then n + 1 else n

    dq :: String -> String
    dq s = "\"" ++ (safe_quote s) ++ "\""

    renderStmt n ('i':'f':js) = "if(" ++ (strip js) ++ "){"
    renderStmt n ('e':'l':'s':'e':_) = "*** ELSE NO SUPPORT ***" -- TODO support else
    renderStmt n ('f':'o':'r':js) = init ++ "for(" ++ loop ++ ")" ++ each
      where
        init
          | conv = "var " ++ len_name ++ "=" ++ array_name ++ ".length;"
          | otherwise = ""
        loop
          | conv = "var " ++ i_name ++ "=0;" ++ i_name ++ "<" ++ len_name ++ ";++" ++ i_name
          | otherwise = strip js
        each
          | conv = "{var " ++ var_name ++ "=" ++ array_name ++ "[" ++ i_name ++ "];"
          | otherwise = "{"
        conv = elem '=' js 
        i_name = ["ijklmn" !! n]
        len_name = "len_" ++ i_name
        var_name = _var_name [] js
        _var_name s [] = s
        _var_name s (' ':xs) = _var_name s xs
        _var_name s ('=':xs) = s
        _var_name s (x:xs) = s ++ [x] ++ _var_name s xs
        array_name = _array_name js
        _array_name [] = []
        _array_name (' ':xs) = _array_name xs
        _array_name ('=':xs) = strip xs
        _array_name (x:xs) = _array_name xs

-- utility
naming :: String -> String
naming [] = []
naming (x:xs) = (if isAlphaNum x then x else '_') : naming xs

namingStatement :: String -> Tree AST -> String
namingStatement name tree = naming (name ++ F.foldr (\a b -> b ++ (show a)) "" tree)

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

startsWith :: String -> String -> Bool
startsWith [] all = True
startsWith (x:xs) (y:ys) = if x == y then startsWith xs ys else False

singleTag :: String -> Bool
singleTag tag = elem tag [
        "meta"
      , "br"
      , "hr"
      , "input"
    ]

safe_quote :: String -> String 
safe_quote [] = ""
safe_quote ('"':xs) = "\\\"" ++ (safe_quote xs)
safe_quote (x:xs) = x : safe_quote xs

is_quoted :: String -> Bool
is_quoted [] = False
is_quoted [x] = False
is_quoted (x:xs) = ('"'  == x && '"'  == (xs !! ((length xs) - 1)))
            || ('\'' == x && '\'' == (xs !! ((length xs) - 1)))

is_need_quote :: String -> Bool
is_need_quote s = (not $ is_quoted s) && has_need_quote s

has_need_quote :: String -> Bool
has_need_quote s = (elem '"' s)
           || (elem '\'' s)
           || (elem ' ' s)
           || (elem '>' s)
           || (elem '<' s)
           || (elem '=' s)
