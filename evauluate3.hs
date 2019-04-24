module Main where 

data Expr = Val Int | Div Expr Expr   

safediv :: Int -> Int -> Maybe Int
safediv n m = if m==0 then
                Nothing
              else
                Just (n `quot` m)

eval :: Expr -> Maybe Int
eval (Val n) = return n
eval (Div x y) = eval x >>= (\n ->
                 eval y >>= (\m ->
                 safediv n m))
                

main :: IO()
main = do 
    let a = Div (Val 6) (Val 2)
    let x = eval ( a )
    putStr "Result: "
    print (x)
    let b = Div (Val 6) (Val 0)
    let y = eval ( b )
    putStr "Result: "
    print (y)