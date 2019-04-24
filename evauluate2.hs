module Main where 

data Expr = Val Int | Div Expr Expr   

safediv :: Int -> Int -> Maybe Int
safediv n m = if m==0 then
                Nothing
              else
                Just (n `quot` m)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
                    Nothing -> Nothing
                    Just n -> case eval y of
                                Nothing -> Nothing
                                Just m -> safediv n m
                

main :: IO()
main = do 
    let a = Div (Val 6) (Val 2)
    let x = eval ( a )
    putStr "Result: "
    print (x)