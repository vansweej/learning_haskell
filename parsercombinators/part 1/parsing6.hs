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

andThen :: Parser a1 -> Parser a2 -> Parser (a1, a2)
andThen parser1 parser2 = do
    let innerFn input = do
        let result1 = run parser1 input
        case result1 of Failure err -> Failure err
                        Success (value1, remaining1) -> do
                            let result2 = run parser2 remaining1
                            case result2 of Failure err -> Failure err
                                            Success (value2, remaining2) -> do
                                                let newValue = (value1, value2)
                                                Success(newValue, remaining2)
    Parser innerFn

(|>>) :: Parser a1 -> Parser a2 -> Parser (a1, a2)
p1 |>> p2 = p1 `andThen` p2   -- andThen in infox notations

main :: IO ()
main = do 
    let parseA = pchar 'A'
    let parseB = pchar 'B'
    let parseAThenB = parseA |>> parseB
    print (run parseAThenB "ABC")
    print (run parseAThenB "ZBC")
    print (run parseAThenB "AZC")
    

