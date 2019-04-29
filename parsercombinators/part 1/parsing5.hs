module Main where

import Text.Printf

data Result a = Success a | Failure String
    deriving Show

data Parser a = Parser (String -> Result (a, String)) 

getParser :: Parser a -> String -> Result (a, String) 
getParser (Parser x) = x 

run :: Parser a -> String -> Result (a, String)
run parser input = getParser parser $ input

pchar :: Char -> Parser Char
pchar charToMatch = do
    let innerfn str = do
        if null str then do
            Failure "No more input"
        else do
            let first = head str
            if first == charToMatch then do
                let remaining = tail str
                Success (charToMatch, remaining)
            else do
                let msg = printf "Expecting '%c', Got '%c'" charToMatch first   
                Failure msg
    Parser innerfn

main :: IO ()
main = do 
    let parseA = pchar 'A'
    let inputABC = "ABC"
    print (run parseA inputABC)
    let inputZBC = "ZBC"
    print (run parseA inputZBC)

    

