module Genotype.Printer.Geno
  ( print
  ) where

import Control.Monad (forM_)
import Data.Text.IO (hPutStr)
import System.IO (Handle, hPutChar)

import Genotype.Comparison
import Genotype.Types
import Prelude hiding (print)

print :: MatchType -> Handle -> [Genotype] -> IO ()
print mtype sink = go . map geno_datums
  where
    go datums =
      case headsTails datums of
        Just (heads, tails) -> do
          printNextLine mtype sink heads
          hPutChar sink '\n'
          go tails
        Nothing -> return ()

printNextLine :: MatchType -> Handle -> [(Datum, Datum)] -> IO ()
printNextLine mtype sink datums =
    forM_ datums $ hPutStr sink . printCompResult mtype . compareToRef baseRef
  where
    baseRef = firstCertain datums
