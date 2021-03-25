module OCR (convert) where

import Data.String.Utils (join)

convert :: String -> String
convert = join "," . readMultiLines . lines

readMultiLines :: [String] -> [String]
readMultiLines [] = []
readMultiLines (l0:l1:l2:l3:ls) = readOneLine [l0, l1, l2, l3] : readMultiLines ls
readMultiLines _ = ["?"]

readOneLine :: [String] -> String
readOneLine ["", "", "", ""] = ""
readOneLine [(l00:l01:l02:l0s), (l10:l11:l12:l1s), (l20:l21:l22:l2s), (l30:l31:l32:l3s)] = 
  readOneNum [[l00, l01, l02], [l10, l11, l12], [l20, l21, l22], [l30, l31, l32]] : readOneLine [l0s, l1s, l2s, l3s]
readOneLine _ = "?"

readOneNum :: [String] -> Char
readOneNum ["   ", "  |", "  |", "   "] = '1'
readOneNum [" _ ", " _|", "|_ ", "   "] = '2'
readOneNum [" _ ", " _|", " _|", "   "] = '3'
readOneNum ["   ", "|_|", "  |", "   "] = '4'
readOneNum [" _ ", "|_ ", " _|", "   "] = '5'
readOneNum [" _ ", "|_ ", "|_|", "   "] = '6'
readOneNum [" _ ", "  |", "  |", "   "] = '7'
readOneNum [" _ ", "|_|", "|_|", "   "] = '8'
readOneNum [" _ ", "|_|", " _|", "   "] = '9'
readOneNum [" _ ", "| |", "|_|", "   "] = '0'
readOneNum _ = '?'


-- "    _  _ "
-- "  | _| _|"
-- "  ||_  _|"
-- "         "
-- "    _  _ "
-- "|_||_ |_ "
-- "  | _||_|"
-- "         "
-- " _  _  _ "
-- "  ||_||_|"
-- "  ||_| _|"
-- "         "
