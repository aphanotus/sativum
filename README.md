# sativum

The goal of this package is to provide user-friendly wrapper functions for student activities in an introductory biology lab focusing on evolution and genetics.

> Now in active development. Target roll-out is **Spring 2024**.

## Installation

```R
install.packages("devtools")
devtools::install_github("aphanotus/sativum")
library(sativum)
```

## Tasks for Package Completion

### Data sets

- [x] Mendel 1866
- [ ] Morgan's data
- [x] East 1916
- [x] `genome.size`: Genome sizes of various organisms
- [x] `human.chrs`: Gene content of human chromosomes
- [x] `finches`: data on beak depth in Darwin's medium ground finch (*Geospiza fortis*) from [Boag & Grant 1981](https://doi.org/10.1126/science.214.4516.82)
- [x] `fish.COII`: Unaligned amino acid sequences from mitochondrial cytochrome c oxidase II (COII) for 5 species of vertebrates. 
- [x] `anole.ND2`: Unaligned nucleotide sequences from 15 species of anoles, including the complete CDS of mitochondrial NADH dehydrogenase subunit 2 (ND2), the complete tRNA-Trp sequence, and partial sequence from tRNA-Ala. 
- [x] `anole.tree`: Phylogenetic tree based on the ND2-Ala sequences 
- [x] `dewlaps`: Dewlap phenotype data from 47 species of anoles. (Table 4 from [Nicholson et al. 2007. *Evolution*](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1803026/))
- [x] `anole.natural.history`: Data for 16 species of anoles and the related lizard \emph{Leiocephalus barahonensis}, listing their endemic island and ecomorph (from [Nicholson et al. 2007](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1803026/)). For use in BI164.
- [x] cross data from Fawcett et al. 2018
- [x] Data on the size and sternite curvature of 542 milkweed bugs, *Oncopeltus fasciatus*, with information on sex and RNAi treatment targetting putative sex-determination genes ([Just et al. 2023](https://doi.org/10.1098/rspb.2022.2083))
- [ ] historic (updated) BI164 *Brassica* data
- [ ] GenBank data accession by year; sequencing costs (available from NCBI)

### Functions for the phylogenetics lab

- [x] `search.for.ncbi.ids`
- [x] `fetch.sequences` (also `fetch.sequence`)
- [X] `simplify.sequence.names`
- [X] `simplify.names`
- [x] `write.fasta`
- [x] `read.fasta` with output to various formats
- [x] `combine.sequences`
- [x] `align.sequences`
- [x] `write.alignment` - write out an interleaved Phylip format file
- [x] `infer.phylogeny` (Consider adding [parsimony](https://cran.r-project.org/web/packages/phangorn/vignettes/Trees.html#Parsimony?)
- [x] rotate nodes using existing functions from `ape` (**Be sure to include in vignette!**)
- [ ] re-root the tree using existing functions from `ape` (**Be sure to include in vignette!**)
- [x] `draw.tree` with some sensible aesthetic parameters, includes the ability to easily color-code tree tips
- [x] `add.branch.lengths` to a tree plot (rendered mostly useless by the `plot.tree` function)
- [x] `label.clade` with a colored bar and text, based on tip names (rather than node numbers)
- [x] `phylogenetic.distance` finds the totals branch lengths separating two tips on a tree

### Other functions

- [ ] Functions to simulate a cross of X genes, Y alleles, with H dom matrix, for N offspring, E epistatic matrix (?)
- [ ] Haldane's equation for recombination frequency by genetic distance

### Vignettes

- [x] **phylogenetics-tetrapod-exercise**: BI163 lab tetrapod phylogenetics introduction
- [x] **phylogenetics-anole-exercise**: BI164 lab [anole phylogenetics activity](https://docs.google.com/document/d/1q7_6T65jznl8OIv6nZ8lzWuxdYgz1WA2/edit?usp=sharing&ouid=109618221670730570824&rtpof=true&sd=true)
- [ ] **Genome Sizes**: Exploration of the `genome.size` data with code for producing nice plots
- [ ] **Mendelian Genetics**: Exploration of Mendel's original data, chi-squared distribution testing, and tests of data from Fawcett et al. 
- [ ] **Quantitative Genetics**: Exploration of East's quantitative data and the BI164 *Brassica* data

---
