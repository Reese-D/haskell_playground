import Data.Char
import Tokens
import Extractor
import Tree
import Expander

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



  
  

--TODO
--write tokenizer/character, once this is done test for proof of concept
--Move each logical section of code into its own class
--Write automated tests to verify functionality


-- --left token to match, right token to match, list of tokens
-- matchCharacters :: Token -> Token -> [Token] -> [Token]
-- matchCharacters leftToken rightToken list = matchCharactersInternal leftToken rightToken 0 list [] []


-- --left token to match, right token to match, how many matching tokens we've seen, tokens before first match, tokens after first match, tokens not seen yet
-- matchCharactersInternal :: Token -> Token -> Int -> [Token] -> [Token] -> [Token] -> [Token]
-- matchCharactersInternal leftToken rightToken 0 left [] (x:xs) --first token not found yet
--   | extractToken x == extractToken leftToken = matchCharactersInternal leftToken rightToken 1 left [] xs
--   | otherwise = matchCharactersInternal leftToken rightToken 0 [left ++ x] [] xs
--     matchCharactersInternal leftToken rightToken matchCounter left middle (x:xs) 
--   | extractToken x == extractToken leftToken = matchCharactersInternal leftToken rightToken (matchCounter + 1) (left ++ [x]) xs -- found another left token, increment counter
--   | extractToken x == extractToken rightToken && matchCounter > 0 = matchCharactersInternal leftToken rightToken (matchCounter - 1) (left ++ [x]) xs --we found a match, but we encountered more left tokens previously so just decrement counter
--   | extractToken x == extractToken rightToken = matchCharactersInternal leftToken rightToken (matchCounter - 1) (left ++ [x]) xs -- found a right match, finish
