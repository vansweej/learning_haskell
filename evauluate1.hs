module Main where 

data Expr = Val Int | Div Expr Expr   

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `quot` eval y

main :: IO()
main = do 
    let a = Div (Val 6) (Val 2)
    let x = eval ( a )
    putStr "Result: "
    print (x)