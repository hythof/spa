module Main where

import Data.Tree (Tree (..))
import AST
import Parse (parseText)
import Build (buildHtml)
import Test.HUnit

ast :: AST -> String -> Test
ast expect input = tree (Node expect []) input

tree :: Tree AST -> String -> Test
tree expect input = 
    case (parseText input) of
        Right x -> x ~?= expect
        Left x -> error $ (show x)  ++ " expect=" ++ (show expect) ++ " input=" ++ input

testRawTag = test [
        ast (RawTag "<div>") "<div>" 
      , ast (RawTag "<div class=foo>") "<div class=foo>" 
    ]

testVar = test [
        ast (Var "name") "$name"
    ]

testTag = test [
        ast (Tag "div" []) "div"
      , ast (Tag "div" [("id", "a")]) "div#a"
      , ast (Tag "div" [("class", "a")]) "div.a"
      , ast (Tag "div" [("class", "a b")]) "div.a.b"
      , ast (Tag "div" [("onclick", "alert(1)")]) "div onclick=alert(1)"
      , ast (Tag "div" [("id", "a"), ("class", "b")]) "div#a.b"
      , ast (Tag "div" [("id", "a"), ("class", "b"), ("onclick", "alert(1)")]) "div#a.b onclick=alert(1)"
    ]

testStmt = test [
        tree (Node (Stmt "if" "exp") []) "%if exp"
      , tree (Node (Stmt "if" "a==a") []) "% if a==a"
      , tree (Node (Stmt "for" "item = items") []) "%for item = items"
      , tree (Node (Stmt "for" "item = items") [
            Node (Tag "div" []) [
                Node (Var "item") []
            ]
        ]) "% for item = items\n  div $item"
    ]

testLines = test [
        tree (
            Node {rootLabel = RawTag "<!doctype html>", subForest = [
                Node {rootLabel = Tag "html" [], subForest = [
                    Node {rootLabel = Tag "head" [], subForest = [
                        Node {rootLabel = Tag "meta" [("charset","utf-8")], subForest = []},
                            Node {rootLabel = Tag "title" [], subForest = [
                                Node {rootLabel = Var "title", subForest = []}]}]},
                    Node {rootLabel = Tag "body" [], subForest = [
                        Node {rootLabel = Tag "h1" [], subForest = [
                            Node {rootLabel = Var "title", subForest = []}]},
                        Node {rootLabel = Stmt "for" "todo in todos", subForest = [
                            Node {rootLabel = Tag "ul" [], subForest = [
                                Node {rootLabel = Stmt "if" "todo.isTimeup", subForest = [
                                    Node {rootLabel = Tag "li" [("class","timeup")], subForest = [
                                        Node {rootLabel = Var "todo", subForest = []}]}]},
                                Node {rootLabel = Stmt "else" "if todo.leftDay < 2", subForest = [
                                    Node {rootLabel = Tag "li" [("class","alert")], subForest = [
                                        Node {rootLabel = Var "tody", subForest = []}]}]},
                                Node {rootLabel = Stmt "else\n" "li $tody", subForest = []}]}]},
                        Node {rootLabel = Tag "footer" [("class","copyright center note")], subForest = [
                            Node {rootLabel = Var "copyright", subForest = []}]}
                        ]}]}]}
        ) input
    ]
  where
    input = unlines [
              "<!doctype html>"
            , "html"
            , "    head"
            , "        meta charset=utf-8"
            , "        title $title"
            , "    body"
            , "        h1 $title"
            , "        % for todo in todos"
            , "          ul"
            , "              % if todo.isTimeup"
            , "                  li.timeup $todo"
            , "              % else if todo.leftDay < 2"
            , "                  li.alert $tody"
            , "              % else"
            , "                  li $tody"
            , "        footer.copyright.center.note $copyright"
        ]

main = do
    runTestTT testRawTag
    runTestTT testVar
    runTestTT testTag
    runTestTT testStmt
    runTestTT testLines
