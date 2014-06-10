module Main where

import Parse (parseText)
import Build (buildHtml)

main = do
    case (parseText input) of
        Right x -> putStr $ buildHtml x
        Left x -> print x 
    where
        input = unlines [
                "<!doctype html>"
                , "html"
                , "    meta charset=utf-8"
                , "    meta name=viewport content=\"width-device-widthinitial-scal=1\""
                , "    title $title"
                , "    link href=css/base.css"
                , "    body"
                , "        h1 $title"
                , "        form#test"
                , "            input type=text name=mail $message"
                , "        form#signin method=post action=signin"
                , "            @mail input type=text name=mail"
                , "            @pass input type=password name=password"
                , "        % for todo in todos"
                , "        ul"
                , "            % if todo.isTimeup"
                , "                li.timeup $todo"
                , "            % else if todo.leftDay < 2"
                , "                li.alert $tody"
                , "            % else"
                , "                li $tody"
            ]
