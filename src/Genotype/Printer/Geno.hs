module Genotype.Printer.Geno
  ( print
  ) where

import Control.Monad (forM_)
import Data.Text.IO (hPutStr)
import System.IO (Handle, hPutChar)

import Genotype.Comparison
import Genotype.Types
import Prelude hiding (print)

print :: Handle -> [Genotype] -> IO ()
print sink = go . map geno_datums
  where
    go datums =
      case headsTails datums of
        Just (heads, tails) -> do
          printNextLine sink heads
          hPutChar sink '\n'
          go tails
        Nothing -> return ()

printNextLine :: Handle -> [(Datum, Datum)] -> IO ()
printNextLine sink datums =
    forM_ datums $ hPutStr sink . printCompResult . compareToRef baseRef
  where
    baseRef = firstCertain datums
