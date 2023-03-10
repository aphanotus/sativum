#' @title Quantative Genetics Data from East 1916
#'
#' @description A dataset reporting the corolla lengths of \emph{Nicotiana} flowers of different
#'     breeds, after hybridization, and after several generations of artificial selection
#'     for longer or shorter corolla lengths.
#'
#' @usage East1916
#'
#' @details
#'     These data originally appeared in East (1916). This dataset is often used in genetics courses
#'     to present the effects of selection on quantitative traits.
#'
#'     \code{East1916} organizes the data in a "tidy" long format. \code{East1916.wide} presents the data in
#'     a dateframe that most closely resembles the original Table 1 from East (1916).
#'
#' @source East, E. 1916. Studies on Size Inheritance in \emph{Nicotiana}. \emph{Genetics} 1(2): 164-176
#'     \url{http://www.genetics.org/content/1/2/164/}
#'
#' @examples
#' library(tidyverse)
#'
#' line.info <- data.frame(
#'   shortdes = c('F2','F2','F3-3','F3-3'),
#'   xintercept = c(81,69.88,69.88,76.34),
#'   style = c('dotted','dotted','solid','dotted')
#' )
#'
#' East1916 %>% filter(grepl(') 2',designation)) %>%
#'   mutate(shortdes = paste0(generation,sub("No. \\(383 X 330\\) 2","",designation))) %>%
#'   filter(shortdes %in% c('F2','F3-3')) %>%
#'   ggplot(aes(x=corolla_mm, y=count, fill=shortdes)) +
#'   theme_bw() +
#'   theme(strip.text = element_blank(), legend.position="none") +
#'   facet_grid(shortdes~., ) +
#'   geom_bar(stat='identity') +
#'   scale_fill_manual(values = c("#7FD34E", "#C2DF23")) +
#'   geom_vline(data=line.info, aes(xintercept=xintercept, linetype=style), color = 'gray35', size = 1) +
#'   labs(x='corolla length (mm)')
#'
#' ggsave('East1916.parent.offspring.RS.example.vertical.pdf',
#'        width = 4, height = 5, scale = 1)
#'
"East1916"

#' @title Quantative Genetics Data from East 1916
#'
#' @description A dataset reporting the corolla lengths of \emph{Nicotiana} flowers of different
#'     breeds, after hybridization, and after several generations of artificial selection
#'     for longer or shorter corolla lengths.
#'
#' @usage East1916.wide
#'
#' @details
#'     These data originally appeared in East (1916). This dataset is often used in genetics courses
#'     to present the effects of selection on quantitative traits.
#'
#'     \code{East1916} organizes the data in a "tidy" long format. \code{East1916.wide} presents the data in
#'     a data frame that most closely resembles the original Table 1 from East (1916).
#'
#' @source East, E. 1916. Studies on Size Inheritance in \emph{Nicotiana}. \emph{Genetics} 1(2): 164-176
#'     \url{http://www.genetics.org/content/1/2/164/}
#'
"East1916.wide"

#' @title Data from Mendel 1866 on Monohybrid Crosses
#'
#' @description A dataset reporting the offspring from monohybrid crosses in the garden pea \emph{Pisum sativum} for 7 traits.
#'
#' @usage Mendel.monohybrid
#'
#' @details
#'     The table lists \code{experiment_number} used by Mendel, the \code{focal_trait} for each experiment,
#'     the \code{number_of_P1_fertilizations} and the \code{number_of_P1_plants}. For the first two
#'     crosses focusing on seed shape and color the \code{number_of_F1_plants} is listed.
#'     The \code{dominant_state} and \code{recessive_state} for each trait are listed in separate columns.
#'     The columns \code{F2_dominant} and \code{F2_recessive} listed the number of F2 offspring displaying the dominant and recessive states for each set of crosses.
#'     Mendel also allowed F2 plants with the dominant phenotype to self-fertilize.
#'     The numbers of those plants producing F3 offspring with only the dominant phenotype are given in the columns \code{F3_dominant_only}.
#'     \code{F3_mixed} lists the number of plants that produced a mix of offspring with the dominant and recessive phenotypes
#'     ("in the proportion of 3:1," as Mendel put it).
#'
#' @source Mendel, G. 1866. Versuche über Plflanzenhybriden. \emp{Verhandlungen des naturforschenden Vereines in Brünn, Bd. IV für das Jahr 1865}, Abhandlungen, 3-47.
#'     \url{http://www.netspace.org./MendelWeb/}
#'
"Mendel.monohybrid"

#' @title Data from Mendel 1866 on Experiment 1, the Monohybrid Cross Focusing on Seed Shape
#'
#' @description A dataset reporting the offspring from replicate monohybrid crosses of the garden pea \emph{Pisum sativum} focusing on seed shape.
#'
#' @usage Mendel.monohybrid.Exp1
#'
#' @details
#'     The table lists the numbers of F2 plants producing \code{round} and \code{wrinkled} seeds from the cross of F1 heterozygous parents.
#'
#' @source Mendel, G. 1866. Versuche über Plflanzenhybriden. \emp{Verhandlungen des naturforschenden Vereines in Brünn, Bd. IV für das Jahr 1865}, Abhandlungen, 3-47.
#'     \url{http://www.netspace.org./MendelWeb/}
#'
"Mendel.monohybrid.Exp1"

#' @title Data from Mendel 1866 on Experiment 2, the Monohybrid Cross Focusing on Seed Color
#'
#' @description A dataset reporting the offspring from replicate monohybrid crosses of the garden pea \emph{Pisum sativum} focusing on seed color
#'
#' @usage Mendel.monohybrid.Exp2
#'
#' @details
#'     The table lists the numbers of F2 plants producing \code{yellow} and \code{green} seeds from the cross of F1 heterozygous parents.
#'
#' @source Mendel, G. 1866. Versuche über Plflanzenhybriden. \emp{Verhandlungen des naturforschenden Vereines in Brünn, Bd. IV für das Jahr 1865}, Abhandlungen, 3-47.
#'     \url{http://www.netspace.org./MendelWeb/}
#'
"Mendel.monohybrid.Exp2"

#' @title Data from Mendel 1866 on his Dihybrid Cross
#'
#' @description A dataset reporting the offspring from the dihybrid cross in the garden pea \emph{Pisum sativum} focusing on seed shape and color.
#'
#' @usage Mendel.dihybrid
#'
#' @details
#'     Mendel crossed seed parents with round yellow seeds (the two dominant phenotype for those traits)
#'     with a pollen parent with wrinkled green seeds (the two recessive phenotypes).
#'     The F1 offspring were all round and yellow like those of the P1 seed parents.
#'     Next, 15 plants yielded offspring with all four possible combinations of seed shape and seed color \code{F2_phenotype}.
#'     The actual numbers are listed in the column \code{F2_count} and appear in the ratio of 9:3:3:1.
#'     Plants grown from the round yellow seeds were allowed to self-fertilize. These produced several \code{F3_phenotype} classes
#'     whose numbers are listed in the column \code{F3_count} and reveal a 1:2:2:4 ratio of homozygosity to heterozygosity.
#'
#' @source Mendel, G. 1866. Versuche über Plflanzenhybriden. \emp{Verhandlungen des naturforschenden Vereines in Brünn, Bd. IV für das Jahr 1865}, Abhandlungen, 3-47.
#'     \url{http://www.netspace.org./MendelWeb/}
#'
"Mendel.dihybrid"

#' @title Data from Mendel 1866 on his Trihybrid Cross
#'
#' @description A dataset reporting the offspring from the trihybrid cross in the garden pea \emph{Pisum sativum} focusing on seed shape, seed color and flower color.
#'
#' @usage Mendel.trihybrid
#'
#' @details
#'     Mendel crossed seed parents with round yellow seeds and violet flowers (the dominant phenotypes for those three traits)
#'     with a pollen parent with wrinkled green seeds and white flowers (the recessive phenotypes).
#'     The F1 offspring were all round yellow seeds which grew into plants with violet flowers, like those of the P1 seed parents.
#'     Next, 24 plants yielded offspring with all possible combinations of seed shape and seed color \code{F2_phenotype}.
#'     Mendel reported the actual numbers from this cross by already organizing plants into classes (\code{Mendels_class}) based on subsequent self-fertilization to identity heterozygotes.
#'     This table provides the phenotypes for \code{seed_shape}, \code{seed_color} and \code{flower_color} for each class as well as their \code{count}.
#'     Combining the counts by phenotype will reveal the expected F2 trihybrid ratio of 27:9:9:9:3:3:3:1.
#'
#' @source Mendel, G. 1866. Versuche über Plflanzenhybriden. \emp{Verhandlungen des naturforschenden Vereines in Brünn, Bd. IV für das Jahr 1865}, Abhandlungen, 3-47.
#'     \url{http://www.netspace.org./MendelWeb/}
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#'
#' Mendel.trihybrid %>%
#'   mutate(phenotype = paste(seed_shape, seed_color, flower_color, sep="/")) %>%
#'   group_by(phenotype) %>%
#'   summarize(count = sum(count))
#'
"Mendel.trihybrid"

#' @title Data on genome sizes
#'
#' @description A dataset including genome size (in base pairs), predicted gene number, percent G+C content and
#'     haploid chromosome number for more than 500 species.
#'
#' @usage genome.size
#'
#' @details
#'     Data are from NCBI where available with many details filled in from the primary literature.
#'     This dataset is for teaching pursues. Its accuracy and currency cannot be maintained.
#'
#' @examples
#' library(ggplot2)
#' library(ggrepel)
#' library(ggpubr)
#'
#' scatterplot.theme <- theme_bw() + theme(
#'   plot.title = element_text(size=16,face="bold",hjust=0.5),
#'   axis.text.x = element_text(size=12,face="plain"),
#'   axis.text.y = element_text(size=12,face="plain"),
#'   axis.title.x = element_text(size=14,face="plain"),
#'   axis.title.y = element_text(size=14,face="plain"),
#'   panel.border = element_rect(fill = NA, color = "black")
#' )
#'
#' taxon.colors=c("darkolivegreen3", "darkred", "brown", "#AA336A", "darkgrey", "antiquewhite4", "darkblue", "darkgoldenrod", "black")
#'
#' genome.fig1 <- ggplot(genome.size) +
#'   scatterplot.theme + theme(legend.position="none") +
#'   geom_point(aes(x=log10(genome_size), y=log10(gene_number), colour = taxon, alpha = 0.9), size=3) +
#'   geom_text_repel(aes(x=log10(genome_size), y=log10(gene_number), label = fig1_labels),
#'                   size =3.5, max.iter = 50000, max.overlaps = 100) +
#'   scale_colour_manual(values = taxon.colors) +
#'   scale_alpha(guide = 'none') +
#'   xlab(expression(paste(log[10]," genome size (bp)", sep=""))) +
#'   ylab(expression(paste(log[10]," gene number", sep="")))
#'
#' genome.fig2 <- ggplot(genome.size) +
#'   scatterplot.theme +
#'   geom_point(aes(x=log10(genome_size), y=log10(gene_number), colour = taxon, alpha = 0.9), size=3) +
#'   geom_text_repel(aes(x=log10(genome_size), y=log10(gene_number), label = fig2_labels),
#'                   size =3.5, max.iter = 50000, max.overlaps = 1000) +
#'   scale_colour_manual(values = taxon.colors) +
#'   scale_alpha(guide = 'none') +
#'   coord_cartesian(xlim = c(7.5, 11.25), ylim = c(4, 5.25)) +
#'   xlab(expression(paste(log[10]," genome size (bp)", sep=""))) +
#'   ylab(expression(paste(log[10]," gene number", sep="")))
#'
#' genome.figs <- ggarrange(genome.fig1, genome.fig2, ncol=2, nrow=1,
#'                          widths = c(1,1.5))
#'
#' genome.figs
#'
#' ggsave('genome.size.figure.jpg', genome.figs, width=9,height=6, scale=1.25)
#'
"genome.size"

#' @title Gene content of human chromosomes
#'
#' @description The dataset includes information on the size and gene content of each human chromosomes, including
#'     NCBI RefSeq identifiers, length in base pairs, the average G+C percentage, the number of short variants (SNPs),
#'     the numbers of annotated protein-coding genes, pseudogenes, transfer RNAs (tRNAs), microRNAs,
#'     small nuclear RNAs (snRNAs), small nucleolar RNAs (snoRNAs), long non-coding RNAs (lncRNAs),
#'     and centromere positions (median) and length. Data are from GRCh37.p14 release 110, except for
#'     GC percentage and short variant numbers, which are based on release 109.
#'
#' @usage human.chrs
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(stringr)
#' library(tidyr)
#' library(ggplot2)
#' library(ggpubr)
#'
#' chr.length <- human.chrs %>%
#'   filter(chr != "mt") %>%
#'   mutate(chr.number = c(str_pad(1:22, width = 2, pad = "0"),"X","Y")) %>%
#'   select(chr.number, length_bp, centromere_position) %>%
#'   ggplot(aes(x=chr.number, y=length_bp/1e6)) +
#'   theme_classic() +
#'   geom_col(width = 0.35, fill = "gray35") +
#'   geom_point(aes(y=centromere_position/1e6), size = 5 ) +
#'   scale_x_discrete(name = "chromosome", labels = human.chrs$chr[-25], position = "top") +
#'   scale_y_reverse(name = "length (Mbp)")
#'
#' length.v.genes <- human.chrs %>%
#'   mutate(chr.number = str_pad(1:25, width = 2, pad = "0")) %>%
#'   ggplot(aes(x = length_bp/1e6, y = protein_coding_genes, label = chr)) +
#'   theme_bw() +
#'   theme( panel.grid.minor = element_blank() ) +
#'   geom_smooth(method=lm, fill = "gray85", color = "darkred") +
#'   geom_text() +
#'   scale_x_continuous(name = "chromosome length (Mbp)") +
#'   scale_y_continuous(name = "number of protein-coding genes")
#'
#' genes.by.chr <- ggarrange(chr.length, length.v.genes, ncol = 2)
#'
#' ggsave('genes.by.chr.jpg', genes.by.chr, width = 9.5, height = 4.5, scale=1)
#'
#' rnas.by.chr <- human.chrs %>%
#'   mutate(chr.number = str_pad(1:25, width = 2, pad = "0")) %>%
#'   pivot_longer(cols = grep("RNA",colnames(human.chrs)), names_to = "RNA.type", values_to = "count") %>%
#'   ggplot(aes(x = rev(chr.number), y = count, group = RNA.type)) +
#'   theme_bw() +
#'   theme( strip.background = element_blank() ) +
#'   facet_grid(.~RNA.type, scales = "free") +
#'   geom_bar(stat = "identity") +
#'   scale_x_discrete(name = "chromosome", labels = rev(human.chrs$chr)) +
#'   scale_y_continuous(name = "count") +
#'   coord_flip()
#'
#' ggsave('rnas.by.chr.jpg', rnas.by.chr, width = 9.5, height = 4.5, scale=1)
#'
"human.chrs"

#' @title Anole DNA sequences
#'
#' @description Unaligned sequences from mitochondrial DNA of 17 species of anoles, including
#'     the complete CDS of NADH dehydrogenase subunit 2 (ND2), the complete tRNA-Trp sequence,
#'     partial sequence from tRNA-Ala.
#'
#' @usage anole.ND2
#'
#' @examples
#' anole.ND2
#' names(anole.ND2)
#'
#' # Make the names more minimalist
#' names(anole.ND2) <- sub("_ND2-tRNAtrp-tRNAala_","__",names(anole.ND2))
#'
#' # Par the names down to just the species
#' names(anole.ND2) <- stringr::str_split_fixed(names(anole.ND2),"__",2)[,1]
#'
#' # Alignment
#' require(msa)
#' seqs.msa <- msa(anole.ND2, method = "ClustalOmega", order = "aligned")
#' print(seqs.msa, show="complete")
"anole.ND2"

