{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Genotype.Comparison
  ( ReferenceComparison (..)
  , compareToRef
  , printCompResult
  , headsTails
  , firstCertain
  ) where

import Control.Monad (sequence)
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

headsTails :: [[a]] -> Maybe ([a],[[a]])
headsTails ll = do
  hts <- sequence $ map headTail ll
  return (map fst hts, map snd hts)

headTail :: [a] -> Maybe (a,[a])
headTail (x:xs) = Just (x,xs)
headTail _ = Nothing

firstCertain :: [(Datum, Datum)] -> BasePair
firstCertain (d:ds) =
  case d of
    ((Certain bp), _) -> bp
    (_, (Certain bp)) -> bp
    _ -> firstCertain ds
firstCertain [] = error "can't find reference BasePair"
