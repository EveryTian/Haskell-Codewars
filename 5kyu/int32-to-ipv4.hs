-- https://www.codewars.com/kata/int32-to-ipv4

module IPv4 where
import Data.Int (Int32)
import Data.Word (Word32)
import Data.Bits (shiftR, (.&.))

type IPString = String

int32ToIP :: Int32 -> IPString
int32ToIP int32 = foldl1 (\ x y -> x ++ "." ++ y) $ map (\ x -> show (shiftR word32 x .&. 0xFF)) [24, 16, 8, 0]
    where word32 = fromIntegral int32 :: Word32
