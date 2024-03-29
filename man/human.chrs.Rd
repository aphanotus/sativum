% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{human.chrs}
\alias{human.chrs}
\title{Gene content of human chromosomes}
\format{
An object of class \code{data.frame} with 25 rows and 14 columns.
}
\usage{
human.chrs
}
\description{
The dataset includes information on the size and gene content of each human chromosomes, including
    NCBI RefSeq identifiers, length in base pairs, the average G+C percentage, the number of short variants (SNPs),
    the numbers of annotated protein-coding genes, pseudogenes, transfer RNAs (tRNAs), microRNAs,
    small nuclear RNAs (snRNAs), small nucleolar RNAs (snoRNAs), long non-coding RNAs (lncRNAs),
    and centromere positions (median) and length. Data are from GRCh37.p14 release 110, except for
    GC percentage and short variant numbers, which are based on release 109.
}
\examples{
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggpubr)

chr.length <- human.chrs \%>\%
  filter(chr != "mt") \%>\%
  mutate(chr.number = c(str_pad(1:22, width = 2, pad = "0"),"X","Y")) \%>\%
  select(chr.number, length_bp, centromere_position) \%>\%
  ggplot(aes(x=chr.number, y=length_bp/1e6)) +
  theme_classic() +
  geom_col(width = 0.35, fill = "gray35") +
  geom_point(aes(y=centromere_position/1e6), size = 5 ) +
  scale_x_discrete(name = "chromosome", labels = human.chrs$chr[-25], position = "top") +
  scale_y_reverse(name = "length (Mbp)")

length.v.genes <- human.chrs \%>\%
  mutate(chr.number = str_pad(1:25, width = 2, pad = "0")) \%>\%
  ggplot(aes(x = length_bp/1e6, y = protein_coding_genes, label = chr)) +
  theme_bw() +
  theme( panel.grid.minor = element_blank() ) +
  geom_smooth(method=lm, fill = "gray85", color = "darkred") +
  geom_text() +
  scale_x_continuous(name = "chromosome length (Mbp)") +
  scale_y_continuous(name = "number of protein-coding genes")

genes.by.chr <- ggarrange(chr.length, length.v.genes, ncol = 2)

ggsave('genes.by.chr.jpg', genes.by.chr, width = 9.5, height = 4.5, scale=1)

rnas.by.chr <- human.chrs \%>\%
  mutate(chr.number = str_pad(1:25, width = 2, pad = "0")) \%>\%
  pivot_longer(cols = grep("RNA",colnames(human.chrs)), names_to = "RNA.type", values_to = "count") \%>\%
  ggplot(aes(x = rev(chr.number), y = count, group = RNA.type)) +
  theme_bw() +
  theme( strip.background = element_blank() ) +
  facet_grid(.~RNA.type, scales = "free") +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "chromosome", labels = rev(human.chrs$chr)) +
  scale_y_continuous(name = "count") +
  coord_flip()

ggsave('rnas.by.chr.jpg', rnas.by.chr, width = 9.5, height = 4.5, scale=1)

}
\keyword{datasets}
