
main = do
	let myList = [x^2| x <- [1..10], x `mod` 2 == 0]
	print myList
