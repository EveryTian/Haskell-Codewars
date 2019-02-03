-- https://www.codewars.com/kata/dna-to-rna-conversion

module DnaToRna where 

dnaToRna :: String -> String 
dnaToRna = map (\x -> case x of 'T' -> 'U'
                                y -> y)
