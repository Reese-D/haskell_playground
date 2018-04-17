module Extractor(
  extractToken,
  getOperatorLevel,
  ) where
 
import Tokens

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
