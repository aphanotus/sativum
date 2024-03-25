#' Plot a phylogenetic tree
#'
#' This function plots a phylogenetic tree (of class \code{phylo}).
#' The function is a wrapper for \code{ape::plot.phylo} with a stylistic changes
#' including thicker branches, slightly offset tip labels, different default font style (bold).
#' It also includes the display of branch lengths by default using \code{ape::edgelabels},
#' with stylistic changes to make it a bit cleaner.
#'
#' If \code{tip.groups} are defined, then \code{tip.color} can be a vector of color names
#' to identify each group. If so, this vector should be equal in length to the number of
#' unique group names. Alternatively, \code{tip.color} can be the name of a color palette
#' from the \code{viridis} package, including "magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako" or "turbo",
#' or letters A through H which stand in for those palette names.
#' If no tip color or palette information is provided, the function defaults to the "viridis" palette.
#'
#' If \code{legend.text.color = "group"} then legend text will be colored to match the groups.
#'
#' @param tree An object of class \code{phylo}.
#' @param tip.font An integer specifying the type of font for the tip labels: 1 (plain text), 2 (bold), 3 (italic, the default), or 4 (bold italic).
#' @param tip.color An optional color name or vector for the tip label text.
#' @param tip.groups An optional vector to identify groups of taxa (tips). The length of this vector must equal the number of tips.
#' @param avoid.auto.color.extremes Logical value (TRUE or FALSE) specifying whether to narrow the range of a viridis palette. Default is TRUE, because the extremes can appear very light and very dark, making perception of the color shade difficult.
#' @param edge.width An integer specifying the width of branches in the tree.
#' @param show.branch.lengths Logical value specifying whether to show branch lengths on the tree. (Default is TRUE.)
#' @param branch.length.cex An integer specifying the character expansion factor for branch length text. (Default is 0.7)
#' @param branch.length.text.color A color name for the branch length text.
#' @param show.scale.bar Logical value specifying whether to show a scale bar. (Default is TRUE.)
#' @param margins A vector of four integers defining the margins of the plot (bottom, left, top, right).
#' @param y.lim An integer vector with two values specifying the minimum and maximum vertical limits of the plot. \code{x.lim} can also be specified.
#' @param show.legend Logical value specifying whether to show a legend for group color-coding. Defaults to TRUE if \code{tip.groups} are defined.
#' @param legend.position A character string specifying whether on the plot to place the legend. Options are "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#' @param legend.title A character string specifying the title of the legend.
#' @param legend.title.font An integer specifying the type of font for the legend title.
#' @param legend.title.color An optional color name for the legend title.
#' @param legend.title.cex An integer specifying the character expansion factor for the legend title.
#' @param legend.title.adj An integer specifying the horizontal adjustment for the legend title.
#' @param legend.text An optional character vector for the group names. Defaults to the unique values of \code{tip.groups}.
#' @param legend.text.color An optional color name or vector of colors for the legend text.
#' @param legend.text.font An integer specifying the type of font for the legend text.
#' @param legend.group.boxes Logical value specifying whether to show boxes with the color of each group. (Default is TRUE.)
#' @param legend.panel.background An optional color name for the legend background.
#' @param legend.border.width An optional integer specifying the border line width around the legend. (Default is 0, for no border.)
#' @param legend.border.color An optional color name for the legend border.
#' @param legend.columns An optional integer specifying the number of columns in the legend.
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
    tip.font = 2,
    tip.color = NULL,
    tip.groups = NULL,
    avoid.auto.color.extremes = TRUE,
    edge.width = 3,
    show.branch.lengths = TRUE,
    branch.length.cex = 0.7,
    branch.length.text.color = "gray50",
    show.scale.bar = TRUE,
    margins = c(0.5,0.5,0.5,0.5),
    y.lim = NULL,
    show.legend = NULL,
    legend.position = "topright",
    legend.title = NULL,
    legend.title.font = 2,
    legend.title.color = par("col"),
    legend.title.cex = par("cex"),
    legend.title.adj = 0,
    legend.text = NULL,
    legend.text.color = par("col"),
    legend.text.font = par("font"),
    legend.group.boxes = TRUE,
    legend.panel.background = par("bg"),
    legend.border.width = 0,
    legend.border.color = par("fg"),
    legend.columns = 1,
    ...
)
{
  # Don't bother running anything if dependent package isn't installed!
  if (!require(ape, quietly = TRUE, warn.conflicts = FALSE)) { stop("Please run  `install.packages('ape')`  first.") }

  if (is.null(tip.color)) { tip.color <- par("col") }

  # User-specified taxon groups to be color-coded
  if (!is.null(tip.groups)) {
    if (!any(is(tip.groups)=="vector")) { stop("Error: `tip.groups` must be a vector equal in length to the number of tips. (See the help entry: `?draw.tree`)\n") }
    if (!(length(tip.groups)==length(tree$tip.label))) { stop("Error: length of `tip.groups` must equal the number of tips. (See the help entry: `?draw.tree`)\n") }

    # **Set the colors**
    # Use the user-provided colors if they supplied the right number, and...
    if ((length(tip.color)==length(unique(tip.groups))) &
        # ...the vector contains only R color names or...
        (all(tip.color %in% colors()) |
         # ...the vector contains hex color codes
         (all(nchar(tip.color) %in% c(7,9)) & all(grepl("^#[0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F]",tip.color))) ) ) {
      specific.colors <- tip.color
    } else { # Otherwise
      if (!require(viridis, quietly = TRUE, warn.conflicts = FALSE)) { stop("Please run  `install.packages('viridis')`  first.") }
      if (length(tip.color)==1) {
        if (tip.color %in% c(LETTERS[1:8], "magma","inferno","plasma","viridis","cividis","rocket","mako","turbo")) {
          specific.colors <- tip.color
        } else {
          specific.colors <- "D"
          if (tip.color != par("fg")){
            warning(paste0(tip.color," is not a recognized color palette. Defaulting to `viridis` palette ",specific.colors,"."))
          }
        }
      } else { specific.colors <- "D" }
      if (avoid.auto.color.extremes) {
        begin = 0.1
        end = 0.9
      } else {
        begin = 0
        end = 1
      }
      specific.colors <- viridis(
        n = length(unique(tip.groups)), option = specific.colors,
        begin = begin, end = end)
    } # End **Set the colors**

    numeric.tip.groups <- as.numeric(as.factor(tip.groups))
    tip.color <- numeric.tip.groups
    for (i in 1:length(tip.color)) {
      tip.color[i] <- specific.colors[numeric.tip.groups[i]]
    }


  } # End  if (!is.null(tip.groups))

  if (is.null(y.lim)) {
    y.lim <- c(0.9,length(tree$tip.label)+0.35)
  }

  original.margins <- par("mai")
  par(mar = margins)

  if (with(tree, exists(quote(edge.length)))) {
    plot(tree,
         font = tip.font,
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
      add.scale.bar(
        lwd = edge.width,
        cex = branch.length.cex,
        col = branch.length.text.color
      )
    }
  } else {
    plot(tree,
         font = tip.font,
         tip.color = tip.color,
         use.edge.length = FALSE,
         label.offset = 0.05,
         edge.width = edge.width,
         ...
    )
  }

  # **Legend**
  if (is.null(show.legend)) {
    show.legend <- ifelse((length(tip.groups)>1), TRUE, FALSE)
  }
  if (show.legend) {
    # Add some vetting here...

    if (is.null(legend.text)) {
      legend.text <- sort(unique(tip.groups))
    }

    if (legend.text.color == "group") {
      legend.text.color <- specific.colors
    }

    if (legend.group.boxes) {
      legend.fill <- specific.colors
      legend.group.border <- par("fg")
    } else {
      legend.fill <- NULL
      legend.group.border <- NULL
    }

    legend(
      x=legend.position,
      title = legend.title,
      title.font = legend.title.font,
      title.col = legend.title.color,
      title.cex = legend.title.cex,
      title.adj = legend.title.adj,
      legend = legend.text,
      text.font = legend.text.font,
      text.col = legend.text.color,
      fill = legend.fill,
      border = legend.group.border,
      bg = legend.panel.background,
      box.col = legend.border.color,
      box.lwd = legend.border.width,
      ncol = legend.columns
    )
  } # End  if (show.legend)
  # End  **Legend**

  par(mar = original.margins)
}
# End of function
