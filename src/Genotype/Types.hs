module Genotype.Types where

import Data.Text (Text)

data BasePair = C | T | A | G deriving (Eq, Show)

data Name = Name Text Text (Maybe Char) deriving (Eq, Show)

data Datum
  = Missing
  | Estimated BasePair
  | Certain BasePair
  deriving (Eq, Show)

data Genotype = Genotype
  { geno_name :: Name
  , geno_subpopLabel :: Int
  , geno_datums :: [(Datum, Datum)]
  } deriving (Eq, Show)
