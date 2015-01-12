module Main where

import Data.Tree (Tree (..))
import Test.HUnit

import AST
import Parse (parseText)
import Build (buildHtml, buildJs)

main = do
    runTestTT testRawTag
    runTestTT testVar
    runTestTT testTag
    runTestTT testStmt
    runTestTT testLines

-- tests
testRawTag = test [
        html "<div>" "<div>" 
      , html "<div>\n</div>" "<div></div>" 
      , html "<div class=foo>" "<div class=foo>" 
      , html "<div class=foo>\n</div>" "<div class=foo></div>" 
    ]

testTag = test [
        html "<div></div>" "div"
      , html "<div id=a></div>" "div#a"
      , html "<div class=a></div>" "div.a"
      , html "<div class=\"a b\"></div>" "div.a.b"
      , html "<div data-length=a></div>" "div data-length=a"
      , html "<div data-length=\"a b\"></div>" "div data-length=\"a b\""
      , html "<div id=a class=b data-length=c></div>" "div#a.b data-length=c"
    ]

testVar = test [
        html "<span data-spa=name></span>" "$name"
      , js "" "$name"
    ]

testStmt = test [
        html "<span data-spa=if_exp></span>" "%if exp"
      , html "<span data-spa=for_a___b></span>" "%for a = b"
      , js "" "%if exp"
      , js expect1 "%if exp\n  $exp"
      -- TOOD support else
      --, js expect2 "%if exp\n  $exp\n%else\n  $other"
      --, js expect3 "%if exp\n  $exp\n%else if other\n  $other"
      , js expect4 "%for todo = todos\n  $todo.title"
      , js expect5 "%for todo = todos\n  %for tag = todo.tags\n    $tag"
      , js expect6 "%for todo = todos\n  %if todo.yet\n    $todo.title"
    ]
  where 
    expect1 = unlines [
            "spa._render.if_exp = function(){var html=\"\";"
          , "if(exp){"
          , "html += exp;"
          , "}"
          , "return html;};"
        ]
    --expect2 = unlines [
    --        "spa._render.if_exp = function(){var html=\"\";"
    --      , "if(exp){"
    --      , "html += exp;"
    --      , "} else {"
    --      , "html += other;"
    --      , "}"
    --      , "return html;};"
    --    ]
    --expect3 = unlines [
    --        "spa._render.if_exp = function(){var html=\"\";"
    --      , "if(exp){"
    --      , "html += exp;"
    --      , "} else if(other) {"
    --      , "html += other;"
    --      , "}"
    --      , "return html;};"
    --    ]
    expect4 = unlines [
            "spa._render.for_todo___todos = function(){var html=\"\";"
          , "var len_i=todos.length;for(var i=0;i<len_i;++i){var todo=todos[i];"
          , "html += todo.title;"
          , "}"
          , "return html;};"
        ]
    expect5 = unlines [
            "spa._render.for_todo___todos = function(){var html=\"\";"
          , "var len_i=todos.length;for(var i=0;i<len_i;++i){var todo=todos[i];"
          , "var len_j=todo.tags.length;for(var j=0;j<len_j;++j){var tag=todo.tags[j];"
          , "html += tag;"
          , "}"
          , "}"
          , "return html;};"
        ]
    expect6 = unlines [
            "spa._render.for_todo___todos = function(){var html=\"\";"
          , "var len_i=todos.length;for(var i=0;i<len_i;++i){var todo=todos[i];"
          , "if(todo.yet){"
          , "html += todo.title;"
          , "}"
          , "}"
          , "return html;};"
        ]

testLines = test [
        html expectHtml input
      , js expectJs input
    ]
  where
    expectJs = unlines [
            "spa._render.if_todos = function(){var html=\"\";"
          , "if(todos){"
          , "html += \"<ul>\";"
          , "var len_i=todos.length;for(var i=0;i<len_i;++i){var todo=todos[i];"
          , "html += \"<li class=todo>\";"
          , "html += todo.title;"
          , "html += \"</li>\";"
          , "}"
          , "html += \"</ul>\";"
          , "}"
          , "return html;};"
        ]
    expectHtml = unlines [
            "<!doctype html>"
          , "<html>"
          , "  <head>"
          , "    <meta charset=utf-8>"
          , "    <title>"
          , "      <span data-spa=title></span>"
          , "    </title>"
          , "  </head>"
          , "  <body>"
          , "    <h1>"
          , "      <span data-spa=title></span>"
          , "    </h1>"
          , "    <span data-spa=if_todos></span>"
          , "    <footer class=\"copyright center note\">"
          , "      <span data-spa=copyright></span>"
          , "    </footer>"
          , "  </body>"
          , "</html>"
        ]
    input = unlines [
              "<!doctype html>"
            , "html"
            , "  head"
            , "    meta charset=utf-8"
            , "    title $title"
            , "  body"
            , "    h1 $title"
            , "    % if todos"
            , "      ul"
            , "        % for todo = todos"
            , "          li.todo $todo.title"
            , "    footer.copyright.center.note $copyright"
        ]

-- utility
ok :: (Tree AST -> String) -> String -> String -> Test
ok f expect input = 
    case (parseText input) of
        Right x -> (strip $ f x) ~?= strip expect
        Left x -> error $ (show x)  ++ " expect=" ++ expect ++ " input=" ++ input

html :: String -> String -> Test
html = ok buildHtml

js :: String -> String -> Test
js = ok buildJs

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
