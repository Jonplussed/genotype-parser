module Genotype.Processor
  ( Processor
  , preprocess
  ) where

import Data.Foldable (foldl')
import Genotype.Types (Genotype)

type Processor = [Genotype] -> IO [Genotype]

preprocess :: [Genotype] -> [Processor] -> IO [Genotype]
preprocess gs = foldl' (>>=) (return gs)
