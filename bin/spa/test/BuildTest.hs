module Main where

import Data.Tree (Tree (..))
import AST
import Parse (parseText)
import Build (buildHtml, buildJs)
import Test.HUnit

ok :: (Tree AST -> String) -> String -> String -> Test
ok f expect input = 
    case (parseText input) of
        Right x -> (f x) ~?= expect
        Left x -> error $ (show x)  ++ " expect=" ++ expect ++ " input=" ++ input

html :: String -> String -> Test
html = ok buildHtml

js :: String -> String -> Test
js = ok buildJs

testRawTag = test [
        html "<div>" "<div>" 
      , html "<div>\n</div>" "<div></div>" 
      , html "<div class=foo>" "<div class=foo>" 
      , html "<div class=foo>\n</div>" "<div class=foo></div>" 
    ]

testVar = test [
        html "<span id=spa_name></span>" "$name"
      , js "ids.name = {};" "$name"
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

testStmt = test [
        html "<span id=spa_if_exp></span>" "%if exp"
      , html "<span id=spa_for_a___b></span>" "%for a = b"
      , js "ids.if_exp = {};\nrenders.if_exp = function(){var html=\"\";return html;};" "%if exp"
    ]

testLines = test [
        html (take ((length expectHtml) - 1) expectHtml) input
      , js (take ((length expectJs) - 1) expectJs) input
    ]
  where
    expectJs = unlines [
        ]
    expectHtml = unlines [
            "<!doctype html>"
          , "<html>"
          , "  <head>"
          , "    <meta charset=utf-8>"
          , "    <title>"
          , "      <span id=spa_title></span>"
          , "    </title>"
          , "  </head>"
          , "  <body>"
          , "    <h1>"
          , "      <span id=spa_title></span>"
          , "    </h1>"
          , "    <span id=spa_for_todo___todos></span>"
          , "    <footer class=\"copyright center note\">"
          , "      <span id=spa_copyright></span>"
          , "    </footer>"
          , "  </body>"
          , "</html>"
        ]
    input = unlines [
              "<!doctype html>"
            , "html"
            , "    head"
            , "        meta charset=utf-8"
            , "        title $title"
            , "    body"
            , "        h1 $title"
            , "        % for todo = todos"
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
