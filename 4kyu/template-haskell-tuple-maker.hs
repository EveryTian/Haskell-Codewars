-- https://www.codewars.com/kata/template-haskell-tuple-maker

{-# LANGUAGE TemplateHaskell #-}
module Kata.TupleMaker (tuple) where

import Language.Haskell.TH
import Control.Applicative
import Control.Monad

-- | Creates a lambda that takes `n` arguments and
-- | returns an n-tuple of those arguments.
tuple :: Int -> Q Exp
tuple n = do
  args <- replicateM n (newName "arg")
  lamE (map varP args) (tupE $ map varE args)
