module Tokens(
  Token(..)
             ) where
import Data.Char



--define our data type with 3 constructors
data Token = Ctok Char
  | Stok String
  | Dtok Double
  deriving(Show)
