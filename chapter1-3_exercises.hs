data List a = Cons a (List a)
            | Nil
            deriving(Show)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving(Show)

-- Creates a Cons list from a regular list
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil


-- Finds the second to last item in a list
lastButOne (x:xs) = if length xs == 1
                    then x
                    else lastButOne xs



-- list operatoins
myLength :: [x] -> Int
myLength (x:xs) | null xs = 1 
myLength (x:xs) = 1 + myLength xs
myLength [] = 0

mySum (x:xs) | null xs = x
mySum (x:xs) = x + mySum xs

myMean (x:xs) | null xs = fromIntegral(x)
myMean (x:xs) = fromIntegral (x + mySum xs) / fromIntegral(myLength (x:xs))
myMean [] = 0.0



-- Create a palindrome from a list, eg [1,2,3] => [1,2,3,3,2,1]
myReverseList (x:xs) | null xs = [x]
myReverseList (x:xs) = myReverseList xs ++ [x]

myPalindrome (x:xs) = (x:xs) ++ myReverseList(x:xs)

-- Check if a list is a palindrome
isPalindrome :: Eq t => [t] -> Bool
isPalindrome [] = True
isPalindrome x = x == myReverseList x


-- In the worst case scenario this would need to be called length list number of times, as it's a very simplistic brute force approach
--sortListOfList :: [[a]] -> [[b]]
partialSort (x:y:xs) = if myLength x < myLength y then x:(partialSort (y:xs)) else y:(partialSort (x:xs))
partialSort (x:y) = [x]
partialSort _ = []


-- delimits a list of strings
intersperse :: a -> [[a]] -> [a]
intersperse _ (x:xs) | null xs = x
intersperse a (x:xs) = x ++ [a] ++ intersperse a xs
intersperse _ [] = []


-- find the height or 'depth' of our tree structure declared previously
findTreeHeight Empty = 0
findTreeHeight (Node _ left right) = 1 + max (findTreeHeight left) (findTreeHeight right)

--for simple testing
simpleTree = Node "parent" (Node "left child" Empty Empty) (Node "right child" Empty Empty)
simpleGp = Node "grandparent" simpleTree (Node "right parent" Empty Empty)


data Direction = RightTurn
                 | LeftTurn
                 | Straight




