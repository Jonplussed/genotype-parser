{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Parser.FastPhase as FastPhase
import qualified Printer.Geno as Geno

import qualified Data.Text.IO as T

main :: IO ()
main = do
  file <- T.readFile "doc/boulderensis_fastphase_genotypes.out"
  case FastPhase.runParser file of
    Left err -> fail err
    Right genos -> Geno.print genos
