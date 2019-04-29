module Main where

import Text.Printf

data Result a = Success a | Failure String
    deriving Show

pchar :: Char -> (String -> Result (Char, String))
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
    innerfn

main :: IO ()
main = do 
    let parseA = pchar 'A' 
    let a = parseA("ABC")
    print a
    let z = parseA("ZBC")
    print z
    

