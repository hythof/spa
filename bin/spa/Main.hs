module Main( main ) where

import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import Data.IORef (newIORef, readIORef, writeIORef)

import Build
import Parse

main = do
    args <- getArgs
    let output = args !! 0
    let input = args !! 1
    ref <- newIORef ""
    forever $ do
        compile ref output input
        threadDelay (1 * 1000 * 1000) -- micro sec

compile ref output input = do
    text <- readFile input
    content <- readIORef ref
    unless (text == content) $ do
        writeIORef ref text
        case parseText text of
          Right tree -> do
            writeFile (output ++ "/index.html") $ buildHtml tree
            putStrLn "compile"
          Left err -> print err
