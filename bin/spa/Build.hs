module Build (
    buildHtml,
    buildJs
) where

import Data.Tree (Tree (..))
import Data.Char (isAlphaNum)
import AST

buildHtml :: Tree AST -> String
buildHtml ast = strip $ html 0 ast -- drop first \n
  where
    rest :: Int -> [Tree AST] -> String
    rest n xs = foldr (++) "" (map (html (n + 1)) xs)

    html :: Int -> Tree AST -> String
    html n (Node (Empty) xs) = rest n xs
    html n (Node (RawTag text) xs) = "\n" ++ (indent n) ++ text ++ (rest (n - 1) xs)
    html n (Node (Var name) xs) = "\n" ++ (indent n) ++ "<span id=spa_" ++ (naming name) ++ "></span>" ++ (rest n xs)
    html n (Node (Stmt stmt) _) = "\n" ++ (indent n) ++ "<span id=spa_" ++ (naming stmt) ++ "></span>" -- skip inner html, that maked js
    html n (Node (Tag name attrs) xs) = "\n" ++ (indent n) ++ "<" ++ name ++ (kv attrs) ++ ">" ++ (if (single name) then tag1 else tag2)
      where
        tag1 = rest n xs
        tag2 = if (length xs) == 0 then tag3 else tag4
        tag3 = "</" ++ name ++ ">\n"
        tag4 = (rest n xs) ++ "\n" ++ (indent n) ++ "</" ++ name ++ ">"
        kv [] = ""
        kv [(k, v)] = " " ++ k ++ "=" ++ (quote_attr v)
        kv (x:xs) = (kv [x]) ++ (kv xs)
        single :: String -> Bool
        single tag = have tag [
                "meta"
              , "br"
              , "hr"
              , "input"
            ]
        quote_attr :: String -> String
        quote_attr s = if (is_need_quote s) then "\"" ++ (safe_quote s) ++ "\"" else s
          where
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

buildJs :: Tree AST -> String
buildJs ast = strip $ js 0 ast
  where
    js :: Int -> Tree AST -> String
    js n (Node (Var name) xs) = "\n" ++ "ids." ++ (naming name) ++ " = {};" ++ (rest (n - 1) xs)
    js n (Node (Stmt stmt) xs) = "\n" ++ "ids." ++ (naming stmt) ++ " = {};" ++ (render (naming stmt) xs)
    js n (Node _ xs) = rest n xs
    rest n xs = foldr (++) "" (map (js (n + 1)) xs)
    render name xs = "\nrenders." ++ name ++ " = function(){var html=\"\";" ++ inners(xs) ++ "return html;};"
      where
        inners xs = foldr (++) "" (map inner xs)
        inner (Node (Empty) xs) = ""
        inner (Node (RawTag text) xs) = add $ text
        inner (Node (Var name) xs) = "html += " ++ name ++ "\n" ++ inners xs
        inner (Node (Stmt stmt) xs) = "\n" ++ stmt ++ " {\n" ++ inners(xs) ++ "\n}"
        inner (Node (Tag name attrs) xs) = add $ name
    --html n (Node (Empty) xs) = join n xs
    --html n (Node (RawTag text) xs) = "\n" ++ (indent n) ++ text ++ (join (n - 1) xs)
    --html n (Node (Var name) xs) = "\n" ++ (indent n) ++ "<span id=spa_" ++ (naming name) ++ "></span>" ++ (join n xs)
    --html n (Node (Stmt stmt) _) = "\n" ++ (indent n) ++ "<span id=spa_" ++ (naming stmt) ++ "></span>" -- skip inner html, that maked js
    --html n (Node (Tag name attrs) xs) = "\n" ++ (indent n) ++ "<" ++ name ++ (kv attrs) ++ ">" ++ (if (single name) then tag1 else tag2)
        add x = "html += " ++ dquote x ++ "\n"
        dquote x = x

-- utility
naming :: String -> String
naming [] = []
naming (x:xs) = (if isAlphaNum x then x else '_') : naming xs

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

