module Ch05 where

import Numeric (showHex)
import Data.Bits
import Data.Char
import Data.Monoid
import Data.List (intercalate)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _ = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull

string :: String -> Doc
string = enclose '"' '"' . mconcat . map oneChar

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary)  = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name
                          <> text ": "
                          <> renderJValue val

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)

-- file: ch05/PrettyJSON.hs
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)


fsep :: [Doc] -> Doc
fsep = foldr (</>) Empty

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group Line

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

compact :: Doc -> String
compact x = transform [x]
  where
    transform [] = ""
    transform (d:ds) = case d of
      Empty        -> transform ds
      Char c       -> c : transform ds
      Text s       -> s ++ transform ds
      Line         -> '\n' : transform ds
      a `Concat` b -> transform (a:b:ds)
      _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where
    best col (d:ds) = case d of
      Empty        -> best col ds
      Char c       -> c :  best (col + 1) ds
      Text s       -> s ++ best (col + length s) ds
      Line         -> '\n' : best 0 ds
      a `Concat` b -> best col (a:b:ds)
      a `Union` b  -> nicest col (best col (a:ds))
                                         (best col (b:ds))
    best _ _ = ""

    nicest col a b
      | (width - (min width col)) `fits` a = a
      | otherwise                = b

instance Semigroup Doc where
  (<>) = Concat

instance Monoid Doc where
  mempty = Empty

instance Show JValue where
  show = compact . renderJValue
