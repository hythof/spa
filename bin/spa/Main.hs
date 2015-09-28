module Main( main ) where

import System.Environment (getArgs)
import Build
import Parse

main = do
    args <- getArgs
    let dictPath = args !! 0
    let templatePath = args !! 1

    dict <- readFile dictPath
    template <- readFile templatePath
    case parseText template of
      Right tree -> do
        let map = to_map dict
--        let map2 = ("script", buildJs map tree) : map
        putStr $ buildHtml map tree
      Left err -> print err

to_map :: String -> [(String, String)]
to_map text = convert [] $ lines text
  where
    convert :: [(String, String)] -> [String] -> [(String, String)]
    convert xs [] = xs
    convert [] (y:ys) = if isTitle y then convert [(y, [])] ys else convert [] ys
    convert xs@((k, v):rest) (y:ys) = if isTitle y then convert key ys else convert val ys
      where
        key = (y, []) : (k, v) : rest
        val = (k, v ++ (trim y)) : rest
    isTitle (' ':_) = False
    isTitle _ = True
    trim (' ':xs) = trim xs
    trim xs = xs
