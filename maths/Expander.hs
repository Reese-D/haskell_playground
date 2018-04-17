module Expander(
  expand
  ) where

import Tokens
import Extractor



expand :: [Token] -> [Token]
expand x = expanderInternal [] x

expanderInternal :: [Token] -> [Token] -> [Token]
expanderInternal left (x:y:xs) | getOperatorLevel x == -1 && getOperatorLevel y == -1 = expanderInternal (left ++ [x] ++ [(Ctok '*')] ++ [y]) xs
