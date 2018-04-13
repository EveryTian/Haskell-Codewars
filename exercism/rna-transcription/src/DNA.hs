module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA dna = let maybeRNA = map f dna
            in if 'X' `elem` maybeRNA then Nothing else Just maybeRNA
            where f :: Char -> Char
                  f 'A' = 'U'
                  f 'T' = 'A'
                  f 'C' = 'G'
                  f 'G' = 'C'
                  f _ = 'X'
