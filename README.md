# Genotype Parser

A command-line tool for parsing and printing various file types of genomic
data. This stemmed from the needs of a friend who was spending far too much
time converting between the various genomic data file-types required by their
corresponding programs.

### Usage

Example:

```
stack exec -- geno-parser \
  --input-format=fastphase \
  --input-file="my_work/fastphase_genotypes.out" \
  --output-format=geno \
  --output-file=m"my_work/my_results" \
  --keep-columns="columns_to_keep.csv"
```

For descriptions of the command-line flags, use:

```
stack exec -- geno-parser --help
```
