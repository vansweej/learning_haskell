module Main where

a_parser :: [Char] -> (Bool, [Char])
a_parser str = if null str then (False, "")
    else if head str == 'A' then do
        let remaining = tail str
        (True, remaining)
    else
        (False, "")

main :: IO()
main = do 
    let result1 = a_parser "ABC"
    print result1
    let result2 = a_parser "ZBC"
    print result2
