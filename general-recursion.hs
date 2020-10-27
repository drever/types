import Data.Function (fix)

main = do
    putStrLn "isEven 0"
    print $ isEven 0
    putStrLn "isEven 1"
    print $ isEven 1
    putStrLn $ "isEven 3"
    print $ isEven 3
    putStrLn "isEven 4"
    print $ isEven 4

    putStrLn "---------------"
    putStrLn "Equal (0, 0)"
    print $ equal (0, 0)
    putStrLn "Equal (1, 0)"
    print $ equal (1, 0)
    putStrLn "Equal (3, 1000)"
    print $ equal (3, 1000)
    putStrLn "Equal (1000, 1000)"
    print $ equal (1000, 1000)

    putStrLn "----------------"
    putStrLn "Plus 0 0"
    print $ plus 0 0
    putStrLn "Plus 0 1"
    print $ plus 0 1
    putStrLn "Plus 1 0"
    print $ plus 1 0
    putStrLn "Plus 42 42"
    print $ plus 42 42

    putStrLn "----------------"
    putStrLn "Mult 0 1"
    print $ mult 0 1
    putStrLn "Mult 3 3"
    print $ mult 3 3
    putStrLn "Mult 2 8"
    print $ mult 2 8
    putStrLn "Mult 8 2"
    print $ mult 8 2
    putStrLn "Mult 7 7"
    print $ mult 7 7

    putStrLn "-----------"
    putStrLn "Fac 0"
    print $ fac 0
    putStrLn "Fac 1"
    print $ fac 1
    putStrLn "Fac 2"
    print $ fac 2
    putStrLn "Fac 3"
    print $ fac 3
    putStrLn "Fac 4"
    print $ fac 4

ff_isEven :: (Int -> Bool) -> Int -> Bool
ff_isEven k n = case n of
     0 -> True
     1 -> False
     otherwise -> k (n - 2)

isEven :: Int -> Bool
isEven = fix ff_isEven

ff_equal :: ((Int, Int) -> Bool) -> (Int, Int) -> Bool
ff_equal k (a, b) = case a of
    0 -> b == 0
    otherwise -> k (a - 1, b - 1)

equal :: (Int, Int) -> Bool
equal = fix ff_equal

ff_plus :: (Int -> Int -> Int) -> Int -> Int -> Int
ff_plus k a b = case a of
    0 -> b
    otherwise -> k (a - 1) (b + 1)

plus :: Int -> Int -> Int
plus = fix ff_plus

ff_mult :: (Int -> Int -> Int) -> Int -> Int -> Int
ff_mult k a b = case a of
   0 -> 0
   1 -> b
   otherwise -> k (a - 1) b `plus` b

mult :: Int -> Int -> Int
mult = fix ff_mult

ff_fac :: (Int -> Int) -> Int -> Int
ff_fac k n = case n of
    0 -> 1
    otherwise -> k (n - 1) `mult` n

fac :: Int -> Int
fac = fix ff_fac

