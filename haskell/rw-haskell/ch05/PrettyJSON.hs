module PrettyJSON
  (
    renderJValue
  ) where

import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)

import SimpleJSON
import Prettify


string :: String -> Doc
string str = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                         . fsep . punctuate (char ',') . map item


oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\',b])

renderJValue :: JValue -> Doc
renderJValue (JString s) = string s
renderJValue (JNumber n) = double n
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"

smallHex :: Int -> Doc
smallHex x =    text "\\u"
             <> text (replicate (4 - length h) '0')
             <> text h
  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | c < 0x10000 = smallHex d
            | otherwise  = astral (d - 0x10000)
  where d = ord c

