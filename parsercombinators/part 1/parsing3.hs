module Main where

import Text.Printf

data Result a = Success a | Failure String
    deriving Show

pchar :: Char -> String -> Result (Char, String)
pchar charToMatch str = if null str then do
        Failure "No more input"
    else do
        let first = head str
        if first == charToMatch then do
            let remaining = tail str
            Success (charToMatch, remaining)
        else do
            let msg = printf "Expecting '%c', Got '%c'" charToMatch first   
            Failure msg

main :: IO()
main = do 
    let a = pchar 'A' "ABC"
    print a
    let z = pchar 'Z' "ABC"
    print z

