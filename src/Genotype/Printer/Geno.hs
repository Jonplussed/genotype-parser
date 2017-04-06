module Genotype.Printer.Geno
  ( print
  ) where

import Control.Monad (forM_)
import Data.Text.IO (hPutStr)
import System.IO (Handle, hPutChar)

import Genotype.Comparison
import Genotype.Types
import Prelude hiding (print)

print :: PhaseKnowledge -> Handle -> [Genotype] -> IO ()
print phase sink = go . map geno_datums
  where
    go datums =
      case headsTails datums of
        Just (heads, tails) -> do
          printNextLine phase sink heads
          hPutChar sink '\n'
          go tails
        Nothing -> return ()

printNextLine :: PhaseKnowledge -> Handle -> [(Datum, Datum)] -> IO ()
printNextLine phase sink datums =
    forM_ datums $ hPutStr sink . printCompResult phase . compareToRef baseRef
  where
    baseRef = firstCertain datums
