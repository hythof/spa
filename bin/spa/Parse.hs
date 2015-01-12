module Parse(
    parseText
) where

import Control.Applicative ( (*>), (<|>), (<$>) )
import Data.Tree (Tree (..))
import Text.Parsec (many, many1, letter, try, option, spaces, char, oneOf, noneOf, sepBy, runParserT)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Indent (withBlock, runIndent, IndentParser(..))

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
parseAST = parseRawTag -- <html>
       <|> parseVar    -- $foo
       <|> parseStmt   -- %if, %for
       <|> parseTag    -- div, span

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

parseStmt :: Parser AST
parseStmt = do
    char '%'
    spaces
    stmt <- many1 $ noneOf "\n"
    spaces
    return $ Stmt stmt

parseTag :: Parser AST
parseTag = do
    name <- tagName
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
        return ("class", join classes)
      where
        join [] = ""
        join [x] = x
        join (x:xs) = x ++ " " ++ (join xs)
        css_class = do
            char '.'
            class_name <- many1 $ noneOf " .#\n"
            return class_name
    attr = do
        char ' '
        k <- many1 $ noneOf "= \n"
        char '='
        v <- quoted_value <|> identifier
        return (k, v)
    quoted_value = do
        char '"'
        k <- many1 $ noneOf "\""
        char '"'
        return k
