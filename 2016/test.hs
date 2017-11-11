--import Test.QuickCheck
--import Data.Char

type Cifra = Int
type Numar = [Cifra]

-- 1a (0,5p). Scrieti o functie care date fiind un Numar n si o lungime l,
-- adauga l cifre de 0 la stanga lui n.
-- E.g., lungimePlus [2, 1, 4] 3 = [0, 0, 0, 2, 1, 4]
lungimePlus :: Numar -> Int -> Numar
lungimePlus x k = (replicate k 0) ++ x

-- 1b (1p). Scrieti o functie care ia ca argument o pereche de numere
-- si calculeaza perechea de numere cu numar egal de cifre 
-- obtinuta prin adaugarea de zerouri la stanga numerelor date ca argument.
-- E.g., normalizeazaLungime ([1,2], [3,4,5,6]) = ([0,0,1,2], [3,4,5,6])
-- E.g., normalizeazaLungime ([1,2], []) = ([1,2], [0,0])
normalizeazaLungime :: (Numar, Numar) -> (Numar, Numar)
normalizeazaLungime (a,b) = ((lungimePlus a (len-lenA)), (lungimePlus b (len-lenB)))
                            where lenA = length a
                                  lenB = length b
                                  len = max lenA lenB
-- 2a (0,75p). Scrieti o functie care ia doua Numere *de aceeasi lungime* ca argumente
-- si verifica daca primul Numar este mai mic sau egal cu al doilea.
-- Puteti folosi doar recursie si functii din categoria A
-- E.g., [1,2,3] `lteN` [1,2,1] = False
-- E.g., [0,2,3] `lteN` [1,2,1] = True
lteN :: Numar -> Numar -> Bool
lteN [] [] = True
lteN (a:xa) (b:xb) | a < b = True
                   | a > b = False
                   | otherwise = lteN xa xb

-- 2b (0,25p).  Scrieti o functie care ia doua Numere ca argumente
-- si verifica daca primul Numar este mai mic sau egal cu al doilea
lte :: Numar -> Numar -> Bool
lte a b = lteN x y where (x,y) = normalizeazaLungime (a,b)

-- 3a (1p). Scrieti o functie care primeste ca argument o lista de
-- numere naturale intre 0 si 81, reprezentand rezultatele brute
-- ale unei operatii asupra unui numar, si calculeaza o pereche
-- formata dintr-o Cifra c si o lista de cifre cs, unde cs are aceeasi 
-- lungime ca lista initiala, fiind obtinuta prin propagarea 
-- depasirilor de cifre de la dreapta la stanga, iar c este cifra 
-- care reprezinta depasirea in plus.
-- E.g. propagaFold' [1, 1] = (0, [1, 1])    -- obtinut din 10 + 1
-- E.g. propagaFold' [1, 10] = (0, [2, 0])   -- obtinut din 19 + 1
-- E.g. propagaFold' [10, 1] = (1, [0, 1])   -- obtinut din 30 + 71
-- E.g. propagaFold' [81, 81] = (8, [9, 1])  -- obtinut din 9*99
-- Folositi doar functii din categoriile A, B, si C
-- Fara recursie sau descrieri de liste.

propagaFold' :: [Int] -> (Cifra, [Cifra])
propagaFold' nr = (x,xs) where len = length nr
                               (x:xs) = map (`mod` 10) (zipWith (+) (scanr(\digit res-> div (digit+res) 10) 0 nr) ([0] ++ nr))

-- 3b (0,5p).  Scrieti o functie care primeste ca argument o lista
-- de numere naturale ca la (3a) si calculeaza numarul corespunzator ei
-- obtinut prin propagarea depasirilor.
-- E.g. propagaFold [1, 1] =  [0, 1, 1]   -- obtinut din 10 + 1
-- E.g. propagaFold [1, 10] = [0, 2, 0]   -- obtinut din 19 + 1
-- E.g. propagaFold [10, 1] = [1, 0, 1]   -- obtinut din 30 + 71
-- E.g. propagaFold [81, 81] = [8, 9, 1]  -- obtinut din 9*99
propagaFold :: [Int] -> Numar
propagaFold nr = [x] ++ xs where (x,xs) = propagaFold' nr


-- 4a (0,75p). Scrieti o functie care primeste ca argument doua liste de cifre
-- *de lungime egala* cs1 si cs2, si calculeaza lista de intregi cs
-- cu proprietatea ca pentru toti i, cs !! i == cs1 !! i + cs2 !! i
-- 
-- E.g., [7,2,3] `plusLista` [4,5,7] = [11,7,10]
-- Folositi doar recursie si functii din categoria A
plusLista :: [Cifra] -> [Cifra] -> [Int]
plusLista [] [] = []
plusLista (a:xa) (b:xb) = (a+b) : (plusLista xa xb)

-- 4b (0,25p). Scrieti o functie care primeste ca argument doua Numere
-- si calculeaza suma lor
-- E.g., [7,2,3] `plus` [4,5,7] = [1,1,8,0]
-- E.g., [7,3] `plus` [4,5,7] = [5,3,0]
plus :: Numar -> Numar -> Numar
plus a b | he > 0 = [he] ++ ta
         | otherwise = ta
        where (x, y) = normalizeazaLungime(a, b)
              (he:ta) = propagaFold (plusLista x y)

-- 5a (0,75p). Scrieti o functie care primeste ca argument doua liste de cifre
-- *de lungime egala* cs1 si cs2, si calculeaza lista de intregi cs
-- cu proprietatea ca pentru toti i, cs !! i == cs1 !! i + cs2 !! i
-- E.g., [7,2,3] `minusLista` [4,5,7] = [3,-3,-4]
-- Folositi doar descrieri de liste si functii din categoriile A si B
-- Fara recursie
minusLista :: [Cifra] -> [Cifra] -> [Int]
minusLista a b = [fst x - snd x | x <- zip a b]

-- 5b (0,25p). Scrieti o functie care primeste ca argument doua Numere
-- si calculeaza diferenta lor, daca primul este mai mare sau egal decat al doilea.
-- In caz contrar, esueaza cu mesajul "Numere negative neimplementate"
-- E.g., [7,2,3] `minus` [4,5,7] = [2,6,6]
-- E.g., [7,3] `minus` [4,5,7]       *** Exception: Numere negative neimplementate
minus :: Numar -> Numar -> Numar
minus a b | lte a b = error "Numere negative neimplementate" 
          | otherwise = dropWhile (==0) (map (`mod` 10) (zipWith (-) (map (+10) diff) (drop 1 (scanr (\digit result-> if digit-result < 0 then 1 else 0) 0 diff))))
          where (x,y)  = normalizeazaLungime (a, b)
                diff = minusLista x y

-- 6a (0,75p). Scrieti o functie care primeste ca argument o Cifra c si un Numar n
-- si calculeaza Numarul obtinut prin inmultirea lui n cu c.
-- E.g., 4 `mulC` [1,0,4] = [4,1,6]
-- E.g., 9 `mulC` [9,9] = [8,9,1]
-- Folositi doar functii din categoriile A, B, si C, si functiile definite mai sus.
-- Fara recursie sau descrieri de liste.
mulC :: Cifra -> Numar -> Numar
mulC cif nr = dropWhile (==0) (propagaFold (map (*cif) nr))

-- 6b (0,25). Scrieti o functie care primeste ca argument un Numar n 
-- si calculeaza Numarul obtinut prin inmultirea lui n cu 10.
-- E.g., mul10 [3,4,5] = [3,4,5,0]
-- E.g., mul10 [3,5] = [3,5,0]
mul10 :: Numar -> Numar
mul10 a = a ++ [0]

-- 7 (2p). Scrieti o functie care primeste ca argument doua Numere
-- si calculeaza Numarul obtinut prin imultirea celor doua numere.
-- E.g., [1,2] `mul` [5,3] = [6,3,6]
-- E.g., [9,9,9] `mul` [9,9,9] = [9,9,8,0,0,1]
-- (32 de simboluri)
mul :: Numar -> Numar -> Numar
--mul a b = foldr (\li res -> (plus res li)) (scanr () [0] b)
mul a b = foldl1 (plus) [(mulC (bRev!!i) a) ++ (replicate i 0) | i <- [0 ..(length(b) - 1)], let bRev = reverse b]

main = do
    print (lungimePlus [2, 1, 4] 3)

    print (normalizeazaLungime ([1,2], [3,4,5,6]))
    print (normalizeazaLungime ([1,2], []))    
    print (normalizeazaLungime ([1,2], [3,4]))
    
    print (lteN [1,2,3] [1,2,1])
    print (lteN [0,2,3] [1,2,1])

    print (propagaFold' [1, 1]) -- (0, [1, 1])
    print (propagaFold' [1, 10]) -- (0, [2, 0])
    print (propagaFold' [10, 1]) -- (1, [0, 1])
    print (propagaFold' [81, 81]) -- (8, [9, 1])    

    print (propagaFold [81, 81]) -- [8 ,9, 1]
    

    print (plusLista [7,2,3] [4,5,7]) --[11,7,10]
    print (plus [7,2,3] [4,5,7]) -- [1,1,8,0]
    print (plus [7,3] [4,5,7]) -- [5,3,0]   
    print (minusLista [7,2,3]  [4,5,7]) -- [3, -3, -4]
    print (minus [7,2,3] [4,5,7]) -- [2,6,6]
    print (minus [7,2,3] [6,8,9]) -- [3, 4]
    print (minus [1,0,2,3] [6,5,7]) -- [3, 6, 6]
    print (minus [1,0,2,3] [1,0, 1, 4]) -- [9]


    print (mulC 4 [1, 0, 4]) -- [4, 1, 6]
    print (mulC 9 [9, 9]) -- [8, 9, 1]

    print (mul [1, 2] [5, 3]) -- [6,3,6]

    print (mul [9, 9, 9] [9,9,9]) -- [9,9,8,0,0,1]

    --print 1