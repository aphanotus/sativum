#' Plot a phylogenetic tree
#'
#' This function plots a phylogenetic tree (of class \code{phylo}).
#' The function is a wrapper for \code{ape::plot.phylo} with a few stylistic changes to the default,
#' including thicker branches, slightly offset tip labels, different default font style (bold).
#' It also includes the display of branch lengths by default using \code{ape::edgelabels},
#' with stylistic changes to make it a bit cleaner.
#'
#' @param tree An object of class \code{phylo}.
#' @param font An integer specifying the type of font for the labels: 1 (plain text), 2 (bold), 3 (italic, the default), or 4 (bold italic).
#' @param tip.color A color name or vector for the tip label text.
#' @param edge.width An integer specifying the width of branches in the tree.
#' @param show.branch.lengths Logical (TRUE or FALSE) specifying whether to show branch lengths on the tree. (Default is TRUE.)
#' @param branch.length.cex An integer specifying the character expansion factor for branch length text. (Default is 0.7)
#' @param branch.length.text.color A color name for the branch length text.
#' @param show.scale.bar Logical (TRUE or FALSE) specifying whether to show a scale bar. (Default is TRUE.)
#' @param margins A vector of four integers defining the margins of the plot.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' draw.tree(anole.tree)
#'

draw.tree <- function (
    tree,
    font = 2,
    tip.color = "black",
    edge.width = 3,
    show.branch.lengths = TRUE,
    branch.length.cex = 0.7,
    branch.length.text.color = "gray50",
    show.scale.bar = TRUE,
    margins = c(0.5,0.5,0.5,0.5),
    y.lim = NULL,
    ...
)
{
  # if (!exists(quote(y.lim))) {
  if (is.null(y.lim)) {
    y.lim <- c(0.9,length(tree$tip.label)+0.35)
  }

  original.margins <- par("mai")
  par(mar = margins)

  if (with(tree, exists(quote(edge.length)))) {
    plot(tree,
         font = font,
         tip.color = tip.color,
         use.edge.length = TRUE,
         label.offset = 0.02*max(tree$edge.length),
         edge.width = edge.width,
         y.lim = y.lim,
         ...
    )
    edgelabels(
      text = signif(tree$edge.length,3),
      adj = c(0.5,-0.5),
      bg = NULL,
      frame = "none",
      cex = branch.length.cex,
      col = branch.length.text.color
    )
    if (show.scale.bar) {
      add.scale.bar(lwd = edge.width)
    }
  } else {
    plot(tree,
         font = font,
         tip.color = tip.color,
         use.edge.length = FALSE,
         label.offset = 0.05,
         edge.width = edge.width,
         ...
    )
  }

  par(mar = original.margins)
}
# End of function
