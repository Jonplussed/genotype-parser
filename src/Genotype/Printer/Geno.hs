module Genotype.Printer.Geno
  ( print
  ) where

import Control.Monad (forM_)
import System.IO (putChar)

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
          printNextLine heads
          putChar '\n'
          go tails
        Nothing -> return ()

printNextLine :: [(Datum, Datum)] -> IO ()
printNextLine datums =
    forM_ datums $ T.putStr . printCompResult . compareToRef baseRef
  where
    baseRef = firstCertain datums
