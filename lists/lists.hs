
mySum :: [Int] -> Int
mySum [] = 0
mySum a = head a + mySum (tail a)

average x y = (x + y)/2

myMax [] = 0
myMax (x:tail) = max x (myMax tail)

multiplyBy ::[Int] -> Int -> [Int]
multiplyBy a b = [x*b| x<- a]


main = do
	let myList = [x^2| x <- [1..10], x `mod` 2 == 0]
	print myList
	print (multiplyBy myList 3)
	print (mySum myList)
