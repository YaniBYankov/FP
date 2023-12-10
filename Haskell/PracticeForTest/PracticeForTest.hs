import Data.Char
import Data.List
import Distribution.Simple.Command (OptDescr (BoolOpt))

--   Задача 1

-- Дефинирайте функция, която обръща число:

-- на един ред
-- на един ред с "магия"

revOneLine :: Int -> Int
revOneLine n = read $ reverse $ show n

revOneLineMagic :: Integer -> Integer
revOneLineMagic = read . reverse . show

-- Задача 2

-- Дефинирайте функция, която:

-- добавя 1 към всеки елемент на списък, използвайки частично прилагане
-- добавя 1 към число, използвайки частично прилагане
-- повдига число на квадрат и добавя едно, използвайки частично прилагане и композиция на функции

addOneXs :: [Int] -> [Int]
addOneXs xs = [xs + 1 | xs <- xs]

addOneN :: Int -> Int
addOneN = (+ 1)

sqPlusOne :: Int -> Int
sqPlusOne n = (+ 1) $ n ^ 2

-- Задача 3

-- Дефинирайте функция, която връща сумата на най-малкия и най-големия делител на цяло число,
-- които са палиндроми и по-големи от 1.

sumMinMaxPalindromes :: Int -> Int
sumMinMaxPalindromes n = (minimum $ div n) + (maximum $ div n)
  where
    div n = [divs | divs <- [2 .. n], mod n divs == 0 && divs == revOneLine divs]

--     Задача 4

-- Дефинирайте функция, която проверява дали цифрите на дадено цяло число са подредени в нарастващ ред.

isAscending :: Int -> Bool
isAscending n = show n == (sort . show) n

-- Задача 5

-- Дефинирайте функция, която приема списък от цели числа и връща списък от списъци,
-- всеки от който съдържа последователни числа.

pack :: [Int] -> [[Int]]
pack [] = []
pack (x : xs) = map reverse $ helper [x] xs
  where
    helper :: [Int] -> [Int] -> [[Int]]
    helper res [] = [res]
    helper (r : rs) (y : ys)
      | y == r + 1 = helper (y : r : rs) ys
      | otherwise = (r : rs) : helper [y] ys

--       Задача 6

-- Дефинирайте функция, която приема дума и връща списък от наредени двойки от вида
--  (<буква>, <брой срещания на буквата>). Не правете разлика между малки и големи букви.

countOccurrences :: String -> [(Char, Int)]
countOccurrences str = map (\xs -> (head xs, length xs)) (group $ sort $ map toLower str)

-- Задача 7

-- Дефинирайте функция, която приема два списъка xs и ys и проверява дали съществува число n,
--  такова че yi = n + xi за всяко i.

isImage :: [Int] -> [Int] -> Bool
isImage xs ys = (length $ nub $ zipWith (\x y -> y - x) xs ys) <= 1

-- Задача 8

-- Дефинирайте функция, която връща сумата на първите n прости числа, съдържащи цифрата d.
intSqrt :: Int -> Int
isPrime :: Int -> Bool
constains :: Int -> Int -> Bool
sumSpecialPrimes :: Int -> Int -> Int
intSqrt = floor . sqrt . fromIntegral

isPrime n = n /= 1 && length [x | x <- [2 .. intSqrt n], mod n x == 0] == 0

constains digit number = elem digit $ map digitToInt $ show number

sumSpecialPrimes n d = sum $ take n [x | x <- [2 ..], isPrime x, constains d x]

--     Задача 9

-- Дефинирайте функция, която приема цяло число и връща неговия растящ ляв суфикс.
-- Растящ ляв суфикс на едно число и числото, което сформира строго растяща редица от цифри,
--  прочетено от ляво надясно.

ascendingleftSuffix :: Int -> Int
ascendingleftSuffix n = read $ last $ filter (\x -> x == sort x) $ inits $ nub $ reverse $ show n

-- Задача 10

-- Дефинирайте функция, която сумира уникалните числа в списък от списъци.

sumUnique :: [[Int]] -> Int
sumUnique xss = sum $ concat $ filter (\xs -> length xs == 1) $ group $ sort $ concat xss

-- Задача 11

-- Дефинирайте функция, която връща броя на различните символи/цифри (игнорираме малки и големи букви),
-- които се срещат повече от веднъж в символен низ.

duplicateCount :: [Char] -> Int
duplicateCount xs = length $ filter (\x -> length x > 1) $ group $ sort $ (map toLower xs)

-- Задача 12

-- Дефинирайте функция, която приема символен низ и премахва последователни дублиращи се символа.
-- Два символа са дублиращи, ако:

-- представят един и същи символ
-- са един до друг
-- първият е главна буква, вторият е малка буква или обратното
areEqual :: Char -> Char -> Bool
areEqual x y = (x == toLower y || x == toUpper y) && x /= y

findFirstDuplicateIndex :: [Char] -> Int
findFirstDuplicateIndex xs = helper 0 xs
  where
    helper :: Int -> String -> Int
    helper _ [] = -1
    helper _ [x] = -1
    helper curr (x : y : xs) = if areEqual x y then curr else helper (curr + 1) (y : xs)

removeAt :: String -> Int -> String -- removing 2 elements that are duplicates
removeAt [] _ = []
removeAt xs i = (take i xs) ++ (drop (i + 2) xs)

reduceStr :: String -> String
reduceStr xs = helper xs
  where
    helper :: String -> String
    helper res = if findFirstDuplicateIndex res == -1 then res else helper (removeAt res (findFirstDuplicateIndex res))

main :: IO ()
main = do
  print $ revOneLine 123 == 321
  print $ revOneLineMagic 123 == 321

  print $ addOneXs [1, 2, 3, 4, 5] == [2, 3, 4, 5, 6]
  print $ addOneN 5 == 6
  print $ sqPlusOne 5 == 26

  print $ sumMinMaxPalindromes 132465 == 8
  print $ sumMinMaxPalindromes 654546 == 8
  print $ sumMinMaxPalindromes 100001 == 100012
  print $ sumMinMaxPalindromes 21612 == 21614
  print $ sumMinMaxPalindromes 26362 == 26364

  print $ isAscending 123 == True
  print $ isAscending 122 == True
  print $ isAscending 0 == True
  print $ isAscending 10 == False
  print $ isAscending 12340 == False
  print $ isAscending 12349 == True

  print $ pack [1, 2, 3, 7, 8, 9] == [[1, 2, 3], [7, 8, 9]]
  print $ pack [1, 7, 8, 9] == [[1], [7, 8, 9]]
  print $ pack [1, 9] == [[1], [9]]

  print $ countOccurrences "Test" == [('e', 1), ('s', 1), ('t', 2)]
  print $ countOccurrences "ThisIsAReallyLongWordContaingAlmostEveryCharacter"
  -- == [('a', 6), ('c', 3), ('d', 1), ('e', 4), ('g', 2), ('h', 2), ('i', 3), ('l', 4), ('m', 1), ('n', 3), ('o', 4), ('r', 5), ('s', 3), ('t', 4), ('v', 1), ('w', 1), ('y', 2)]

  print $ isImage [] [] == True
  print $ isImage [1, 2, 3] [2, 3, 4] == True
  print $ isImage [1, 2, 3] [4, 6, 9] == False
  print $ isImage [1, 2, 3] [2, 5, 4] == False

  print $ sumSpecialPrimes 5 2 == 392
  print $ sumSpecialPrimes 5 3 == 107
  print $ sumSpecialPrimes 10 3 == 462

  print $ ascendingleftSuffix 37563
  print $ ascendingleftSuffix 32763
  print $ ascendingleftSuffix 32567
  print $ ascendingleftSuffix 32666

  print $ sumUnique [[1, 2, 3, 2], [-4, -4], [5]] == 9 -- (= 1 + 3 + 5)
  print $ sumUnique [[2, 2, 2], [3, 3, 3], [4, 4, 4]] == 0
  print $ sumUnique [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == 45

  print $ duplicateCount "" == 0 -- no characters repeats more than once
  print $ duplicateCount "abcde" == 0
  print $ duplicateCount "aabbcde" == 2 -- 'a' and 'b'
  print $ duplicateCount "aabBcde" == 2 -- 'a' occurs twice and 'b' twice (`b` and `B`)
  print $ duplicateCount "indivisibility" == 1 -- 'i' occurs six times
  print $ duplicateCount "Indivisibility" == 1
  print $ duplicateCount "aA11" == 2 -- 'a' and '1'
  print $ duplicateCount "ABBA" == 2 -- 'A' and 'B' each occur twice
  print $ duplicateCount "Indivisibilities" == 2 -- 'i' occurs seven times and 's' occurs twice
  print $ duplicateCount ['a' .. 'z'] == 0
  print $ duplicateCount (['a' .. 'z'] ++ ['A' .. 'Z']) == 26

  print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
