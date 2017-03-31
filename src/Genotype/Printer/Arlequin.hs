module Genotype.Printer.Arlequin
  ( print
  ) where

import Control.Monad (forM_)
import Data.Text.IO (hPutStr)
import System.IO (Handle, hPutChar)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Genotype.Comparison
import Genotype.Types
import Prelude hiding (print)

print :: Handle -> [Genotype] -> IO ()
print sink genos =
  forM_ genos $ \geno -> do
    let (Name letters numbers _) = geno_name geno
    hPutStr sink letters
    hPutStr sink numbers
    hPutChar sink '\t'
    hPutStr sink . T.pack . show $ geno_subpopLabel geno
    hPutChar sink '\t'
    printNextLine sink baseRefs $ geno_datums geno
    hPutChar sink '\n'
  where
    baseRefs = buildReferenceVector $ map geno_datums genos

printNextLine :: Handle -> V.Vector BasePair -> [(Datum, Datum)] -> IO ()
printNextLine sink refs = go 0
  where
    go _ [] = return ()
    go index (d:ds) = do
      hPutStr sink . printCompResult $ compareToRef (refs V.! index) d
      go (succ index) ds

-- | Prevent an O(n^2) operation from a `V.snoc` call per data column
buildReferenceVector
  :: [[(Datum, Datum)]] -> V.Vector BasePair
buildReferenceVector datums =
  V.create $ do
    vect <- VM.new $ refCount datums
    go vect 0 datums
    return vect
  where
    go vect index remDatums =
      case headsTails remDatums of
        Just (heads, tails) -> do
          VM.write vect index $ firstCertain heads
          go vect (succ index) tails
        Nothing -> return ()

refCount :: [[a]] -> Int
refCount (x:_) = length x
refCount [] = 0
