module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard = filter . (not .)

keep :: (a -> Bool) -> [a] -> [a]
keep = filter
