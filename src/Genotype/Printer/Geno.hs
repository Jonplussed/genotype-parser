module Genotype.Printer.Geno
  ( print
  ) where

import Data.Text (Text)

import qualified Data.Text.IO as T

import Genotype.Comparison
import Genotype.Types
import Prelude hiding (print)

print :: [Genotype] -> IO ()
print = go . map geno_datums
  where
    go datums =
      case headsTails datums of
        Just (heads, tails) -> do
          T.putStrLn $ makeNextLine heads
          go tails
        Nothing -> return ()

makeNextLine :: [(Datum, Datum)] -> Text
makeNextLine datums = foldMap printer datums
  where
    printer = printCompResult . compareToRef baseRef
    baseRef = firstCertain datums
