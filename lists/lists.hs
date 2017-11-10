import Test.QuickCheck
import Data.Char

type Matrix = [[Int]]

mySum :: [Int] -> Int
mySum [] = 0
mySum a = head a + mySum (tail a)

average x y = (x + y)/2

myMax [] = 0
myMax (x:tail) = max x (myMax tail)

multiplyBy ::[Int] -> Int -> [Int]
multiplyBy a b = [x*b| x<- a]

sumEvenPositions :: [Int] -> Int 
sumEvenPositions a = sum [a !! x | x <- [0, 2 .. (length a)-1]]

---3---

halveEvens :: [Int] -> [Int]
halveEvens a = [div x 2| x <-a, even x]

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (listHead:listTail)
        | even listHead	= [div listHead 2] ++ (halveEvensRec listTail)
        | otherwise     = halveEvensRec listTail
	
prop_halveEvens  :: [Int] -> Bool
prop_halveEvens  a = halveEvens a == halveEvensRec a


countGreater :: Int -> [Int] -> Int
countGreater x a = length [y | y <-a , y > x]

countGreaterRec :: Int -> [Int] -> Int
countGreaterRec x [] = 0
countGreaterRec x a | y > x = 1 + (countGreaterRec x (tail a))
                    | otherwise = countGreaterRec x (tail a)
                    where y = head a

prop_count :: Int -> [Int] -> Bool
prop_count x a = (countGreater x a) == (countGreaterRec x a)


multDigits :: String -> Int
multDigits s = foldr (*) 1 [digitToInt x | x <- s, isAlpha x]

capitalise :: String -> String
capitalise s = toUpper(head s):[] ++ [toLower(x) | x <- tail s]

---4---

rotate :: Int -> String -> String
rotate k str = let (a, b) = splitAt k str
		in b ++ a
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

test_input_rotate :: Int -> String -> Bool
test_input_rotate k str = (0 < k) && (k < length str)

test_rotate :: Int -> String -> Property
test_rotate k str = (test_input_rotate k str) ==> (prop_rotate k str)

makeKey :: Int -> [(Char, Char)]
makeKey k = let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
                alphaRev =  rotate k alpha
           in [(alpha!!i,alphaRev!!i) | i <- [0 .. (length alpha)-1]]

lookUp :: Char-> [(Char,Char)] -> Char
lookUp ch li = let (fi, se) = unzip li
		   pos = [i | i <- [0 .. (length fi)-1], fi!!i == ch]
		   k = pos!!0
               in se!!k

reverseKey :: [(Char, Char)] -> [(Char,Char)]
reverseKey li = let (fi,se) = unzip li 
                in [(se!!i, fi!!i)| i <- [0 .. (length fi)-1]]

contains :: String -> String -> Bool
contains [] [] = True
contains _  [] = True
contains [] _ = False
contains (ch1:text) (ch2:pattern) = (ch1 == ch2 && (contains text pattern)) || (contains text (ch2:pattern) )

splitEachK :: Int -> String -> [String]
splitEachK _ [] = []
splitEachK k str | len <= k = [str ++ replicate (k-len) 'X']
		| otherwise = let (fi,se) = splitAt k str in [fi] ++ splitEachK k se
		where len = length str

countFreqs :: String -> [(Char, Int)]
countFreqs [] = []
countFreqs str = [(ch, fr)| ch <- ['a'..'z'] ++ ['A' .. 'Z'], let fr = length [ch2 | ch2<-str, ch2 == ch], fr > 0]

---5 ---

reverseEven :: [String] -> [String]
reverseEven li = map reverse (filter (\str -> even (length str)) li) 

--concatFold :: [Int] -> [Int]
concatFold a = foldr (\li elem -> li ++ elem) [] a

myAll pred li = length (filter (==True) (map pred li) ) == length li
myAll2 pred li = foldr (&&) True (map pred li)

uniform (x:xs) = myAll2 (==x) xs

main = do
	let myList = [x^2| x <- [1..10], x `mod` 2 == 0]
	print myList
	print (multiplyBy myList 3)
	print (mySum myList)
	print (sumEvenPositions myList)
	let myList2 = halveEvens myList
	print myList2
	quickCheck prop_halveEvens
	quickCheck prop_count 
	putStrLn (capitalise "boSS")
	print (lookUp 'N' (makeKey 3))
	quickCheck test_rotate
	print (contains "Example" "amp")
	print (splitEachK 5 "abcdefghijklmnopqrstuwwxyz")
	print (countFreqs "boss De bboSS")
	print (foldr (\x y -> if x > y then x else y) 0 [1,2,3])
	print (concatFold ["boss", " de", " boss"])
	print (uniform [1,1,1,1])
