module Tree(
  Tree(..),
  buildTree
  ) where

import Tokens
import Extractor

--build tree by pivoting around operators, build subtrees inside of ( ) first then work outwards
data Tree a = Branch a (Tree a) (Tree a)
  | Leaf a
  deriving(Show)


--takes a list of tokens
buildTree :: [Token] -> Tree(Token)
buildTree x = buildTreeInternal 0 [] x

--Operator level, left, right
buildTreeInternal :: Int -> [Token] -> [Token] -> Tree(Token)
buildTreeInternal opLevel left (x:[]) = Leaf x
buildTreeInternal opLevel left (x:y:xs) | (getOperatorLevel x) == 3 = buildTreeInternal opLevel newToken xs
                                where newToken = left ++ [Stok $ extractToken x ++ extractToken y]
buildTreeInternal opLevel [] (x:xs) = buildTreeInternal opLevel [x] xs
buildTreeInternal opLevel left (x:xs)
  | (getOperatorLevel x) == opLevel = Branch x (buildTreeInternal 0 [] left) (buildTreeInternal 0 [] xs)
