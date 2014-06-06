module IndentedTree where

import Control.Applicative ( (*>), (<|>) )
import Data.Char (isSpace)
import Data.Tree (Tree (..))
import Text.Parsec (many, many1, letter, try, option, spaces, char, oneOf, noneOf, sepBy, runParserT)
import Text.Parsec.Indent (withBlock, runIndent, IndentParser(..))

data AST = Empty
    | RawTag String
    | Tag String [(String, String)]
    | Var String
    | Macro String
    | Control String
    | Group String
    deriving (Show, Eq)


type Parser a = IndentParser String () a

parseSPA input = runIndent "" $ runParserT aTree () "" input

buildHTML (Node v ts) = html v ++ "\n" ++ draw ts
    where
        draw [] = []
        draw [t] = buildHTML t
        draw (t:ts) = buildHTML t ++ draw ts

html (Empty) = ""
html (RawTag name) = name
html (Var name) = "<span id=\"var_" ++ name ++ "\">"
html (Macro name) = "<span id=\"macro_" ++ name ++ "\">"
html (Group name) = "<!-- " ++ name ++ " -->"
html (Tag name attrs) = "<" ++ name ++ kv(attrs) ++ ">"
    where
        kv [] = []
        kv [(k, v)] = " " ++ k ++ "=" ++ v
        kv (x:xs) = (kv [x]) ++ (kv xs)

aTree :: Parser (Tree AST)
aTree = spaces *> withBlock Node parseAST aTree

parseAST :: Parser AST
parseAST = parseRawTag
       <|> parseVar
       <|> parseMacro
       <|> parseGroup
       <|> parseTag

parseRawTag :: Parser AST
parseRawTag = do
    char '<'
    rawTag <- many1 $ noneOf ">"
    char '>'
    return $ RawTag $ "<" ++ rawTag ++ ">"

parseTag :: Parser AST
parseTag = do
    name <- many1 $ noneOf " \n"
    attrs <- many $ try attr
    spaces
    return $ Tag name attrs
  where
    attr = do
        char ' '
        k <- many1 $ noneOf "= \n"
        char '='
        v <- many1 $ noneOf " \n"
        return (k, v)

parseVar = do
    char '$'
    name <- many1 $ noneOf " \n"
    spaces
    return $ Var name

parseMacro = do
    char '@'
    name <- many1 $ noneOf " \n"
    spaces
    return $ Macro name

parseGroup = do
    char '%'
    name <- many $ noneOf "\n"
    spaces
    return $ Group name

-- 入力例
example = unlines [
    "<!doctype html>",
    "html",
    "    meta charset=utf-8",
    "    meta name=viewport content=\"width-device-width,initial-scal=1\"",
    "    meta name=keywords content=$keywords",
    "    title $title",
    "    link href=css/base.css",
    "    body",
    "        h1 $title",
    "        form#test",
    "            input type=text name=mail $message",
    "        form#signin method=post action=signin",
    "            @mail input type=text name=mail",
    "            @pass input type=password name=password",
    "        % for todo in todos",
    "        ul",
    "            % if todo.isTimeup",
    "                li.timeup $todo",
    "            % else if todo.leftDay < 2",
    "                li.alert $tody",
    "            % else",
    "                li $tody"
    ]
