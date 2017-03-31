{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<**>), (<|>))
import Data.Char (toLower)
import Data.Semigroup ((<>))
import System.IO (IOMode (WriteMode), stdout, openFile)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Genotype.Parser.FastPhase as FastPhase
import qualified Genotype.Printer.Arlequin as Arlequin
import qualified Genotype.Printer.Geno as Geno
import qualified Options.Applicative as O

data InputFormat
  = FastPhase
  deriving (Eq, Show)

parseInputFormat :: O.Parser InputFormat
parseInputFormat =
  O.option (O.maybeReader formats)
    (  O.long "input-format"
    <> O.metavar "NAME"
    <> O.help "Select from: fastphase"
    )
  where
    formats str = case map toLower str of
      "fastphase" -> Just FastPhase
      _ -> Nothing

data InputSource
  = STDIN
  | InputFile T.Text
  deriving (Eq, Show)

parseInputSourceFile :: O.Parser InputSource
parseInputSourceFile =
  InputFile . T.pack <$> O.strOption
    (  O.long "input-file"
    <> O.metavar "FILENAME"
    <> O.help "Input file"
    )

parseInputSource :: O.Parser InputSource
parseInputSource = parseInputSourceFile <|> pure STDIN

data OutputFormat
  = Arlequin
  | Geno
  deriving (Eq, Show)

parseOutputFormat :: O.Parser OutputFormat
parseOutputFormat =
  O.option (O.maybeReader formats)
    (  O.long "output-format"
    <> O.metavar "NAME"
    <> O.help "Select from: arlequin, geno"
    )
  where
    formats str = case map toLower str of
      "arlequin" -> Just Arlequin
      "geno" -> Just Geno
      _ -> Nothing

data OutputSink
  = STDOUT
  | OutputFile T.Text
  deriving (Eq, Show)

parseOutputSinkFile :: O.Parser OutputSink
parseOutputSinkFile =
  OutputFile . T.pack <$> O.strOption
    (  O.long "output-file"
    <> O.metavar "FILENAME"
    <> O.help "Output file"
    )

parseOutputSink :: O.Parser OutputSink
parseOutputSink = parseOutputSinkFile <|> pure STDOUT

data Options = Options
  { opt_inputFormat :: InputFormat
  , opt_inputSource :: InputSource
  , opt_outputFormat :: OutputFormat
  , opt_outputSource :: OutputSink
  } deriving (Eq, Show)

parseOptions :: O.Parser Options
parseOptions =
  Options
    <$> parseInputFormat
    <*> parseInputSource
    <*> parseOutputFormat
    <*> parseOutputSink

parseOptionsWithInfo :: O.ParserInfo Options
parseOptionsWithInfo =
  O.info
    (parseOptions <**> O.helper)
    (O.header "genotype-parser - parser and printer of genotype data formats")

-- TODO: ensure output file handle closes after use
main :: IO ()
main = do
  opts <- O.execParser parseOptionsWithInfo
  input <- case opt_inputSource opts of
    InputFile file -> T.readFile $ T.unpack file
    STDIN -> T.getContents
  parsed <- either fail return $ case opt_inputFormat opts of
    FastPhase -> FastPhase.runParser input
  sink <- case opt_outputSource opts of
    OutputFile file -> openFile (T.unpack file) WriteMode
    STDOUT -> return stdout
  case opt_outputFormat opts of
    Arlequin -> Arlequin.print sink parsed
    Geno -> Geno.print sink parsed
