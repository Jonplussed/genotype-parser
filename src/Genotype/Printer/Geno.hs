{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Genotype.Printer.Geno where

import Control.Monad (sequence)
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Text.IO as T

import Genotype.Comparison
import Genotype.Types
import Prelude hiding (head)

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
