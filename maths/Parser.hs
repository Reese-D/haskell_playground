import Data.Char

--define our data type with 3 constructors
data Token = Ctok Char
  | Stok String
  | Dtok Double
  deriving(Show)
-- --determine if two characters should be split or not
-- same_type :: Char -> Char -> Bool
-- same_type a b
--   | a == '(' || b == ')' = False
--   | a == ')' || b == ')' = False
--   | isAlpha a && isAlpha b = True
--   | isDigit a && isDigit b = True
-- same_type _ _ = False

--check list for sin/cos/tan otherwise default to using same_type to split
  


parse :: String -> [Token]
parse []  = []
parse x   = parseInternal x []

takeInts :: String ->  String -> (String, String)
takeInts head (x:xs) | isDigit x || x == '.'  = takeInts (head++[x]) xs
takeInts head xs                              = (head, xs)

parseToTokens :: String -> [Token]
parseToTokens x = parseInternal x []

--Parses a string into individual tokens
parseInternal :: String -> [Token] -> [Token]
parseInternal  ('S':'i':'n':xs) tokens  = parseInternal xs $ Stok "Sin" : tokens
parseInternal  ('C':'o':'n':xs) tokens  = parseInternal xs $ Stok "Cos" : tokens
parseInternal  ('T':'a':'n':xs) tokens  = parseInternal xs $ Stok "Tan" : tokens
parseInternal (x:xs) tokens
  | isDigit x                            = parseInternal second $ (Dtok (read first :: Double)) : tokens
    where (first, second)                = takeInts [x] xs
parseInternal (x:xs) tokens             = parseInternal xs $ Ctok x : tokens
parseInternal [] tokens                 = reverse tokens

--build tree by pivoting around operators, build subtrees inside of ( ) first then work outwards

--returns the underlying string value of a token
extractToken :: Token -> [Char]
extractToken (Stok a) = a
extractToken (Ctok a) = [a]
extractToken (Dtok a) = show a

getOperatorLevel :: Token -> Int 
getOperatorLevel token
  | extracted  == ['+']                                             = 0
  | extracted == ['*'] || extracted == ['/']                        = 1
  | extracted == ['^']                                              = 2
  | extracted == "Sin" || extracted == "Cos" || extracted == "Tan"  = 3
  | otherwise                                                       = -1
  where extracted = extractToken token
  
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
  

--TODO
--write tokenizer/character, once this is done test for proof of concept
--Move each logical section of code into its own class
--Write automated tests to verify functionality

tokenizer :: [Char] -> [Tokens]
tokenizerInternal :: [Char] -> [Tokens] -> [Tokens]

--left token to match, right token to match, list of tokens
matchCharacters :: Token -> Token -> [Tokens] -> [Tokens]
matchCharacters leftToken rightToken list = matchCharactersInternal leftToken rightToken 0 list [] []


--left token to match, right token to match, how many matching tokens we've seen, tokens before first match, tokens after first match, tokens not seen yet
matchCharactersInternal :: Token -> Token -> Int -> [Tokens] -> [Tokens] -> [Tokens] -> [Tokens]
matchCharactersInternal leftToken rightToken 0 left [] (x:xs) --first token not found yet
  | extractToken x == extractToken leftToken = matchCharactersInternal leftToken rightToken 1 left [] xs
  | otherwise = matchCharactersInternal leftToken rightToken 0 [left ++ x] [] xs
matchCharactersInternal leftToken rightToken matchCounter left middle (x:xs) 
  | extractToken x == extractToken leftToken = matchCharactersInternal leftToken rightToken (matchCounter + 1) (left ++ [x]) xs
  | extractToken x == extractToken rightToken && matchCounter > 0 = matchCharactersInteral leftToken rightToken (matchCounter - 1) (left ++ [x]) xs
  | extractToken x == extractToken rightToken = matchCharactersInteral leftToken rightToken (matchCounter - 1) (left ++ [x]) xs -- what do we do with our match?...
