-- Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns False
splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith _ [] = [[]]
splitWith _ (x:xs) | null xs = [[x]]
splitWith p xs =
  let (pre, suf) = break p (dropWhile p xs)
  in pre : (splitWith p suf)


-- Write your own definition of concat using foldr.
myConcat :: [[a]] -> [a]
myConcat xs = foldr (\x y -> foldr (:) y x) [] xs
--myConcat = foldr (++) []


-- Write your own definition of the standard takeWhile function, first using explicit recursion, then foldr
myTakeWhile  :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile _ (x:xs) | null xs = [x]
myTakeWhile p (x:xs) | not (p x) = []
myTakeWhile p (x:xs) = x : myTakeWhile p xs


foldrTakeWhile :: (a -> Bool) -> [a] -> [a]
foldrTakeWhile _ [] = []
foldrTakeWhile _ (x:xs) | null xs = [x]
foldrTakeWhile p (x:xs) | not (p x) = []
foldrTakeWhile p xs = foldr func [] xs
  where func x y | p x = x : y
                 | otherwise = []

