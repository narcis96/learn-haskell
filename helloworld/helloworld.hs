import System.Environment
import Test.QuickCheck

mySum ::Int -> Int -> Int
mySum a b = a+b

square ::Int -> Int
square x = x * x

double ::Int -> Int
double x = x + x 

isTriple ::Int -> Int -> Int -> Bool
isTriple a b c = (square(a) + square(b) == square(c))

param1 ::Int -> Int -> Int
param1 x y = square x - square y

param2 ::Int -> Int -> Int
param2 x y = double (x*y)

param3 ::Int -> Int -> Int
param3 x y = mySum (square x) (square y)

prop_triple ::Int -> Int -> Bool
prop_triple x y = isTriple (param1 x y) (param2 x y) (param3 x y)

fib ::Int -> Int
fib 0 = 1
fib 1 = 0
fib n = fib(n-1) + fib(n-2)

--listSum :: Int -> [Int]
--listSum a = head (a) + listSum (tail(a)) 

main = do
	--args <- getArgs
	let args = getArgs
	putStrLn "Hello, World!"
	first <- getLine
	second <- getLine
	let x = read first :: Int
	putStrLn (first ++ " + " ++ second ++ " = " ++ show (mySum x (read second::Int)))
	quickCheck prop_triple	
--	putStrLn (double ("boss"))
--	putStrLn (head args)
--let var = read (head args) :: String
	--putStrLn (head args :: Strin)
--putStrLn(args)
	--do x <- [1, 2, 3]
	--	print x
	return ()

