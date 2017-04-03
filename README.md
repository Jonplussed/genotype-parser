# Genotype Parser

A command-line tool for parsing and printing various file types of genomic
data. This stemmed from the needs of a friend who was spending far too much
time converting between the various genomic data file-types required by their
corresponding programs.

Supported input formats:

- FastPhase

Supported output formats:

- Arlequin
- Geno

To support additional formats, either raise a Github issue or issue a pull
request with the desired parser or printer. All parsers resolve to a datatype
of `[Genotype]`, while all printers stream to the selected `Handle`.

### Installation

First you'll need Stack to install the required Haskell dependencies. How to
install by OS differs, but if this is a Debian or Ubuntu machine, then:

```
sudo apt-get update
sudo apt-get install haskell-stack
```

Next, clone this directory onto your local machine, either via:

```
git clone git@github.com:Jonplussed/genotype-parser.git
```

...or via the Github page download link, `cd` into the directory,
and:

```
stack update
stack setup
stack build
stack install
```

Now, genotype-parser is ready to use!

### Usage

Example:

```
genotype-parser \
  --input-format=fastphase \
  --input-file="my_work/fastphase_genotypes.out" \
  --output-format=geno \
  --output-file=m"my_work/my_results" \
  --keep-columns="columns_to_keep.csv"
```

For descriptions of the command-line flags, use:

```
genotype-parser --help
```
