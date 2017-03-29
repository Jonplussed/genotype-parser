{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Genotype.Comparison
  ( ReferenceComparison (..)
  , compareToRef
  , printCompResult
  ) where

import Data.Text (Text)

import Genotype.Types

data ReferenceComparison
  = BothMatch
  | FirstMatch
  | LastMatch
  | NoMatch
  | CannotCompare
  deriving (Enum, Eq, Show)

compareToRef :: BasePair -> (Datum, Datum) -> ReferenceComparison
compareToRef ref datums =
  case getBasePairs datums of
    Just (bp1, bp2)
      | bp1 == ref && bp2 == ref -> BothMatch
      | bp1 == ref -> FirstMatch
      | bp2 == ref -> LastMatch
      | otherwise  -> NoMatch
    _ -> CannotCompare

getBasePair :: Datum -> Maybe BasePair
getBasePair  = \case
  Certain bp -> Just bp
  Estimated bp -> Just bp
  _ -> Nothing

getBasePairs :: (Datum, Datum) -> Maybe (BasePair, BasePair)
getBasePairs (d1,d2) = do
  bp1 <- getBasePair d1
  bp2 <- getBasePair d2
  return (bp1,bp2)

printCompResult :: ReferenceComparison -> Text
printCompResult = \case
  BothMatch     -> "0"
  FirstMatch    -> "1"
  LastMatch     -> "2"
  NoMatch       -> "3"
  CannotCompare -> "-9"
