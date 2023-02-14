#' @title Quantative Genetics Data from East 1916
#'
#' @description A dataset reporting the corolla lengths of \emph{Nicotiana} flowers of different
#'     breeds, after hybridization, and after several generations of artificial selection
#'     for longer or shorter corolla lengths.
#'
#' @usage East1916
#' East1916.wide
#'
#' @details
#'     These data originally appeared in East (1916). This daatset is often used in genetics courses
#'     to present the effects of selection on quantitative traits.
#'
#'     \code{East1916} organizes the data in a "tidy" long format. \code{East1916.wide} presents the data in
#'     a dateframe that most closely resembles the original Table 1 from East (1916).
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut]
#'
#' @references East, E. 1916. Studies on Size Inheritance in \emph{Nicotiana}. \emph{Genetics} 1(2): 164-176
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
#' @usage East1916
#' East1916.wide
#'
#' @details
#'     These data originally appeared in East (1916). This daatset is often used in genetics courses
#'     to present the effects of selection on quantitative traits.
#'
#'     \code{East1916} organizes the data in a "tidy" long format. \code{East1916.wide} presents the data in
#'     a dateframe that most closely resembles the original Table 1 from East (1916).
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut]
#'
#' @references East, E. 1916. Studies on Size Inheritance in \emph{Nicotiana}. \emph{Genetics} 1(2): 164-176
#'     \url{http://www.genetics.org/content/1/2/164/}
#'
"East1916.wide"