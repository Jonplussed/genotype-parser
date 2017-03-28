{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Printer.Geno where

import Control.Monad (sequence)
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Text.IO as T

import Prelude hiding (head)
import Types

print :: [Genotype] -> IO ()
print genos = go $ map geno_datums genos
  where
    go datums =
      case headsTails datums of
        Just (heads, tails) -> do
          T.putStrLn $ makeNextLine heads
          go tails
        Nothing -> return ()

headsTails :: [[a]] -> Maybe ([a],[[a]])
headsTails ll = do
  hts <- sequence $ map headTail ll
  return (map fst hts, map snd hts)

headTail :: [a] -> Maybe (a,[a])
headTail (x:xs) = Just (x,xs)
headTail _ = Nothing

makeNextLine :: [(Datum, Datum)] -> Text
makeNextLine datums = foldMap printer datums
  where
    printer = printCompResult . compareToRef baseRef
    baseRef = firstCertain datums

firstCertain :: [(Datum, Datum)] -> BasePair
firstCertain (d:ds) =
  case d of
    ((Certain bp), _) -> bp
    (_, (Certain bp)) -> bp
    _ -> firstCertain ds
firstCertain [] = error "can't find reference BasePair"

compareToRef :: BasePair -> (Datum, Datum) -> ReferenceComparison
compareToRef ref datums =
  case getBasePairs datums of
    Just (bp1, bp2)
      | bp1 == ref && bp2 == ref -> BothMatch
      | bp1 == ref -> FirstMatch
      | bp2 == ref -> LastMatch
      | otherwise  -> NoMatch
    _ -> CannotCompare

getBasePairs :: (Datum, Datum) -> Maybe (BasePair, BasePair)
getBasePairs (d1,d2) = do
  bp1 <- getBasePair d1
  bp2 <- getBasePair d2
  return (bp1,bp2)

getBasePair :: Datum -> Maybe BasePair
getBasePair  = \case
  Certain bp -> Just bp
  Estimated bp -> Just bp
  _ -> Nothing

printCompResult :: ReferenceComparison -> Text
printCompResult = \case
  BothMatch     -> "0"
  FirstMatch    -> "1"
  LastMatch     -> "2"
  NoMatch       -> "3"
  CannotCompare -> "-9"
