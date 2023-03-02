# sativum

The goal of this package is to provide user-friendly wrapper functions for student activities in an introductory biology lab focusing on evolution and genetics.

> Now in active development. Target roll-out is **13 April 2023**.


## Installation

```R
install.packages("devtools")
devtools::install_github("aphanotus/sativum")
library(sativum)
```

## Tasks for Package Completion: `sativum`

### Data sets

- [x] Mendel's data
- [ ] Morgan's data
- [x] East's data
- [x] `anole.txt` sequences 

Others?

### Functions for the phylogenetics lab

See [Sarah's lab activity](https://docs.google.com/document/d/1q7_6T65jznl8OIv6nZ8lzWuxdYgz1WA2/edit?usp=sharing&ouid=109618221670730570824&rtpof=true&sd=true)

- [x] `search.for.ncbi.ids`
- [x] `fetch.sequences` (also `fetch.sequence`)
- [X] `simplify.sequence.names`
- [x] `write.fasta `
- [x] `combine.sequences`
- [ ] `align.sequences`
- [ ] `show.alignment(anole.alignment, format = "text")` or `"pdf"`
- [ ] `infer.phylogeny`
- [ ] a function to get branch lengths between specific tips on a tree (all pairwise?)
- [ ] color code tips / branches / clades
- [ ] rotate nodes

### Other functions

- [ ] Functions to simulate a cross of X genes, Y alleles, with H dom matrix, for N offspring, E epistatic matrix
- [ ] Functions for pop gen simulations
- [ ] Cool stuff from BI279

