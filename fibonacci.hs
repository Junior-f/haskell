aux :: Int -> Int -> Bool
aux n d
   | d == 1 = True
   | (mod n d) /= 0 = aux n(d-1)
   | otherwise = False



primo :: Int -> Bool
primo n 
   |n == 1 = False
   |n == 2 = True
   |(mod n 2) == 0 &&  (n /= 2) = False
   |otherwise = aux n(n-1)




fibo :: Int -> Int
fibo n
    | n == 0 = 0
    | n == 1 = 1
    |otherwise = fibo(n -1) + fibo (n-2)



auxFibo :: Int-> Int-> Int -> Int 
auxFibo x cont y 
    | x == cont = y - 1
    | primo(y) == False =  x auxFibo (y + 1)
    | otherwise = auxFibo x (cont + 1) (y + 1) 


nFibo :: Int-> Int
nFibo n
    | n  == 1 = 2
    |otherwise = auxFibo n 0 1