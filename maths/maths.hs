mult :: Int -> Int -> Int
mult a b = a * b

sub :: Int -> Int -> Int
sub a b = a - b


data Val = Char | String | Int -- Either a variable, (Cos, Sin, Tan), or a number
data WorkUnit = WorkUnit WorkUnit
               | Val
