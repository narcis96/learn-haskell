import System.Environment

mySum ::Int -> Int -> Int
mySum a b = a+b

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
	let y = read second :: Int
	putStrLn (first ++ " + " ++ second ++ " = " ++ show(mySum x y))
	putStrLn (head args)
--let var = read (head args) :: String
	--putStrLn (head args :: Strin)
--putStrLn(args)
	--do x <- [1, 2, 3]
	--	print x
	return ()

