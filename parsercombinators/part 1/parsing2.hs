module Main where

import Text.Printf

pchar :: (Char, [Char]) -> ([Char], [Char])
pchar (charToMatch, str) = if null str then do
        let msg = "No more input"
        (msg, "")
    else do
        let first = head str
        if first == charToMatch then do
            let remaining = tail str
            let msg = printf "Found %c" charToMatch
            (msg, remaining)
        else do
            let msg = printf "Expecting '%c', Got '%c'" charToMatch first   
            (msg, str)

main :: IO()
main = do 
    let a = pchar('A', "ABC")
    print a
    let z = pchar('A', "ZBC")
    print z
