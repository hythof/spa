module Parse where

import Control.Applicative ( (*>), (<|>), (<$>) )
import Data.Tree (Tree (..))
import Text.Parsec (many, many1, letter, try, option, spaces, char, oneOf, noneOf, sepBy, runParserT)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Indent (withBlock, runIndent, IndentParser(..))
import AST

type Parser a = IndentParser String () a

identifier = many1 $ noneOf " \n"

parseText :: String -> Either ParseError (Tree AST)
parseText input = runIndent "" $ runParserT parseTree () "" input

parseTree :: Parser (Tree AST)
parseTree = spaces *> withBlock Node parseAST parseTree

parseAST :: Parser AST
parseAST = parseRawTag
       <|> parseVar
       <|> parseStmt
       <|> parseTag

parseRawTag :: Parser AST
parseRawTag = do
    char '<'
    rawTag <- many1 $ noneOf ">"
    char '>'
    return $ RawTag $ "<" ++ rawTag ++ ">"

parseTag :: Parser AST
parseTag = do
    name <- identifier
    attrs <- many $ try attr
    spaces
    return $ Tag name attrs
  where
    attr = do
        char ' '
        k <- many1 $ noneOf "= \n"
        char '='
        v <- identifier
        return (k, v)

parseVar :: Parser AST
parseVar = do
    char '$'
    name <- identifier
    spaces
    return $ Var name

parseStmt :: Parser AST
parseStmt = do
    char '%'
    name <- many1 $ noneOf "\n"
    spaces
    return $ Stmt name
