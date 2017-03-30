module Genotype.Printer.Arlequin where

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.IO (putChar)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Genotype.Comparison
import Genotype.Types
import Prelude hiding (head, print)

print :: [Genotype] -> IO ()
print genos =
  forM_ genos $ \geno -> do
    let (Name name _) = geno_name geno
    T.putStr $ name
    putChar '\t'
    T.putStr . T.pack . show $ geno_subpopLabel geno
    putChar '\t'
    printNextLine baseRefs $ geno_datums geno
    putChar '\n'
  where
    baseRefs = buildReferenceVector $ map geno_datums genos

printNextLine :: V.Vector BasePair -> [(Datum, Datum)] -> IO ()
printNextLine refs = go 0
  where
    go _ [] = return ()
    go index (d:ds) = do
      T.putStr . printCompResult $ compareToRef (refs V.! index) d
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
