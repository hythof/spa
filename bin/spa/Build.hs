module Build where

import Data.Tree (Tree (..))
import AST

buildHtml :: Tree AST -> String
buildHtml = html 0
  where
    join :: Int -> [Tree AST] -> String
    join n xs = foldr (++) "" (map (html (n + 1)) xs)

    html :: Int -> Tree AST -> String
    html n (Node (Empty) xs) = join n xs
    html n (Node (RawTag text) xs) = "\n" ++ (indent n) ++ text ++ (join (n - 1) xs)
    html n (Node (Var name) xs) = "\n" ++ (indent n) ++ "<span id=" ++ name ++ "></span>" ++ (join n xs)
    html n (Node (Stmt name exp) xs) = "\n" ++ (indent n) ++ "<!-- " ++ name ++ " " ++ exp ++ " -->" ++ (join n xs)
    html n (Node (Tag name attrs) xs) = "\n" ++ (indent n) ++ "<" ++ name ++ (kv attrs) ++ ">" ++ (if (single name) then tag1 else tag2)
      where
        tag1 = join n xs
        tag2 = if (length xs) == 0 then tag3 else tag4
        tag3 = "</" ++ name ++ ">\n"
        tag4 = (join n xs) ++ "\n" ++ (indent n) ++ "</" ++ name ++ ">\n"
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
            is_need_quote :: String -> Bool
            is_need_quote s = (have '"' s)
                           || (have '\'' s)
                           || (have ' ' s)
                           || (have '>' s)
                           || (have '<' s)
                           || (have '=' s)

buildTs :: Tree AST -> String
buildTs = ts
  where
    ts :: Tree AST -> String
    ts (Node (Var name) xs) = "\n" ++ "var " ++ name ++ " = document.getElementById(\"" ++ name ++ "\");" ++ (join xs)
    ts (Node (Stmt name exp) xs) = "\n" ++ "var " ++ name ++ " = document.getElementById(\"" ++ name ++ "\");" ++ (join xs)
    ts (Node _ xs) = foldr (++) "" (map ts xs)
    join xs = foldr (++) "" (map ts xs)

have :: Eq a => a -> [a] -> Bool
have n [] = False
have n [x] = n == x
have n (x:xs) = n == x || have n xs

indent :: Int -> String
indent n = (take (n * 2) $ repeat ' ')
