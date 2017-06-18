module Prettify where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = Concat x y

hcat ::[Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (d:ds) =
          case d of
            Empty -> transform ds
            Char c -> c : transform ds
            Text s -> s ++ transform ds
            Line -> '\n' : transform ds
            a `Concat` b -> transform (a:b:ds)
            _ `Union` b -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) =
          case d of
            Empty        -> best col ds
            Char c       -> c :  best (col + 1) ds
            Text s       -> s ++ best (col + length s) ds
            Line         -> '\n' : best 0 ds
            a `Concat` b -> best col (a:b:ds)
            a `Union` b  -> nicest col (best col (a:ds))
                            (best col (b:ds))
        best _ _ = ""

        nicest col a b | (width - least) `fits` a = a
                       | otherwise                = b
          where least = min width col                           

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs


--Our current pretty printer is spartan, so that it will fit within our space constraints, but there are a number of useful improvements we can make. No comments

--1.Write a function, fill, with the following type signature. No comments
--fill :: Int -> Doc -> Doc

--user comments in the online version of the book
--said it's supposed to be Int -> Doc -> String
--This is the implementation of that version, i'm not sure if the type signature is correct or not.

fill :: Int -> Doc -> String
fill col x = fillHelper 0 col [x]
  where fillHelper count col (d:ds) =
          case d of
            Line       -> (replicate (col - count - 1) ' ') ++ "\n" ++  (fillHelper 0 col ds)
            Empty      -> fillHelper (count+1) col ds
            Text s     -> s ++ fillHelper (count+1) col ds
            Char c     -> c : fillHelper (count+1) col ds
            Concat a b -> fillHelper count col (a:b:ds)
            Union _ b  -> fillHelper count col (b:ds)

        fillHelper count col _ = ""


