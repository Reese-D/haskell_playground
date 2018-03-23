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
parse [] = []
parse x = parse_internal x []


takeInts :: String ->  String -> (String, String)
takeInts head (x:xs) | isDigit x || x == '.' = takeInts (head++[x]) xs
takeInts head xs = (head, xs)

--flesh this out
parse_internal :: String -> [Token] -> [Token]
parse_internal  ('S':'i':'n':xs) tokens = parse_internal xs $ Stok "Sin" : tokens
parse_internal  ('C':'o':'n':xs) tokens = parse_internal xs $ Stok "Cos" : tokens
parse_internal  ('T':'a':'n':xs) tokens = parse_internal xs $ Stok "Tan" : tokens
parse_internal (x:xs) tokens
  | isDigit x = parse_internal second $ (Dtok (read first :: Double)) : tokens
    where (first, second) = takeInts [x] xs
parse_internal (x:xs) tokens = parse_internal xs $ Ctok x : tokens
parse_internal [] tokens = reverse tokens

--build tree by pivoting around operators, build subtrees inside of ( ) first then work outwards
