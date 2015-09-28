module Parse(
    parseText
) where

import Control.Applicative ( (*>), (<|>), (<$>) )
import Data.Tree (Tree (..))
import Data.Char (toUpper)
import Text.Parsec (many, many1, letter, try, option, spaces, char, oneOf, noneOf, sepBy, runParserT, lookAhead)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Indent (withBlock, runIndent, IndentParser(..))
import Debug.Trace (trace)

import AST

type Parser a = IndentParser String () a

identifier = many1 $ noneOf " \n"

parseText :: String -> Either ParseError (Tree AST)
parseText input = runIndent "" $ runParserT (parseRoot) () "" input

parseRoot :: Parser (Tree AST)
parseRoot = Node Root <$> many parseTree

parseTree :: Parser (Tree AST)
parseTree = spaces *> withBlock Node parseAST parseTree

parseAST :: Parser AST
parseAST = parseRawTag   -- <html>
       <|> parseVar      -- $foo
       <|> parseConst    -- @bar
       <|> parseJs       -- :func-arg1-arg2
       <|> parseTag      -- div, span

parseRawTag :: Parser AST
parseRawTag = do
    char '<'
    rawTag <- many1 $ noneOf ">"
    char '>'
    return $ RawTag $ "<" ++ rawTag ++ ">"

parseVar :: Parser AST
parseVar = do
    char '$'
    name <- identifier
    spaces
    return $ Var name

parseConst :: Parser AST
parseConst = do
    char '@'
    name <- identifier
    spaces
    return $ Const name

--parseStmt :: Parser AST
--parseStmt = do
--    char '%'
--    spaces
--    stmt <- many1 $ noneOf "\n"
--    spaces
--    return $ Stmt stmt

parseJs :: Parser AST
parseJs = do
    char ':'
    spaces
    (func:args) <- sepBy (many1 $ noneOf " -") (char '-')
    spaces
    let arg = join "," $ map arg_convert args
    return $ Tag "a" [
            ("href", "#"),
            ("onclick", "return " ++ func ++ "(" ++ arg ++ ")")
        ]
  where
    arg_convert ('$':prefix:name) = "this.dataset.spaValue" ++ [toUpper prefix] ++ name
    arg_convert x = "'" ++ x ++ "'"

parseTag :: Parser AST
parseTag = do
    name <- tagName <|> lookAhead (oneOf ".#" >> return "div")
    css_ids <- many $ css_id
    css_classes <- many $ css_classes
    attrs <- many $ try attr
    spaces
    return $ Tag name (css_ids ++ css_classes ++ attrs)
  where
    tagName = many1 $ noneOf " .#\n"
    css_id = do
        char '#'
        id_ <- many1 $ noneOf " .#\n"
        return ("id", id_)
    css_classes = do
        classes <- many1 $ css_class
        return ("class", join " " classes)
      where
        css_class = do
            char '.'
            class_name <- many1 $ noneOf " .#\n"
            return class_name
    attr = do
        char ' '
        k <- many1 $ noneOf "= \n"
        char '='
        v <- quoted_value <|> identifier
        return $ conv k v
      where
        conv "if" v = ("data-spa-if", v)
        conv "for" v = ("data-spa-for", v)
        conv k v = (k, v)
    quoted_value = do
        char '"'
        k <- many1 $ noneOf "\""
        char '"'
        return k

join :: String -> [String] -> String
join sep [] = ""
join sep [x] = x
join sep (x:xs) = x ++ sep ++ (join sep xs)
