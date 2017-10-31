
mySum ::Int -> Int -> Int
mySum a b = a+b

--listSum :: Int -> [Int]
--listSum a = head (a) + listSum (tail(a)) 

main = do
	putStrLn "Hello, World!"
	first <- getLine
	second <- getLine
	let x = read first :: Int
	let y = read second :: Int
	print (mySum x y)
	let list = [1, 2, 3, 4]
	return ()

