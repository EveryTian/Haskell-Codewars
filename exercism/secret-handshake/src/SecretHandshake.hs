module SecretHandshake (handshake) where

import Data.Bits (bit, (.&.))

handshake :: Int -> [String]
handshake n = let s = ["wink", "double blink", "close your eyes", "jump"]
              in (if bit 4 .&. n == 0 then id else reverse)
                 [s !! i | i <- [0..3], bit i .&. n /= 0]
