module PrettyJSON (renderJValue, enclose) where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Numeric (showHex)
import Prettify
  ( Doc,
    char,
    compact,
    double,
    fsep,
    hcat,
    pretty,
    punctuate,
    text,
    (<>),
  )
import SimpleJSON (JValue (..))

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber n) = double n
renderJValue (JString s) = string s
renderJValue (JArray a) = series '[' ']' renderJValue a
renderJValue (JObject o) = series '{' '}' field o
  where
    field (name, val) =
      string name
        Prettify.<> text ": "
        Prettify.<> renderJValue val

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left Prettify.<> x Prettify.<> char right

hexEscape :: Char -> Doc
hexEscape c
  | d < 0x10000 = smallHex d
  | otherwise = astral (d - 0x10000)
  where
    d = ord c

smallHex :: Int -> Doc
smallHex x =
  text "\\u"
    Prettify.<> text (replicate (4 - length h) '0')
    Prettify.<> text h
  where
    h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) Prettify.<> smallHex (b + 0xdc00)
  where
    a = (n `shiftR` 10) .&. 0x3ff
    b = n .&. 0x3ff

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
  Just r -> text r
  Nothing
    | mustEscape c -> hexEscape c
    | otherwise -> char c
  where
    mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where
    ch a b = (a, ['\\', b])

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item =
  enclose open close
    . fsep
    . punctuate (char ',')
    . map item

putCompactJSONStr :: Doc -> IO ()
putCompactJSONStr = putStrLn . compact

putJSONStr :: Doc -> IO ()
putJSONStr = putStrLn . pretty 70