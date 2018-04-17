module ProteinTranslation(proteins) where

aminoAcid :: String -> Maybe String
aminoAcid "AUG" = Just "Methionine"
aminoAcid "UUU" = Just "Phenylalanine"
aminoAcid "UUC" = Just "Phenylalanine"
aminoAcid "UUA" = Just "Leucine"
aminoAcid "UUG" = Just "Leucine"
aminoAcid "UCU" = Just "Serine"
aminoAcid "UCC" = Just "Serine"
aminoAcid "UCA" = Just "Serine"
aminoAcid "UCG" = Just "Serine"
aminoAcid "UAU" = Just "Tyrosine"
aminoAcid "UAC" = Just "Tyrosine"
aminoAcid "UGU" = Just "Cysteine"
aminoAcid "UGC" = Just "Cysteine"
aminoAcid "UGG" = Just "Tryptophan"
aminoAcid "UAA" = Just ""
aminoAcid "UAG" = Just ""
aminoAcid "UGA" = Just ""
aminoAcid _ = Nothing

proteins :: String -> Maybe [String]
proteins "" = Just []
proteins (x0:x1:x2:xs) = case aminoAcid [x0, x1, x2] of
    Nothing -> Nothing
    Just "" -> Just []
    Just s -> case proteins xs of
                  Nothing -> Nothing
                  Just x -> Just (s:x)
proteins _ = Nothing
