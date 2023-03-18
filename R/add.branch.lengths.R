#' Add branch lengths to a phylogenetic tree
#'
#' @param tree An object of class \code{phylo}.
#' @param rounding A number of significant digits for rounding of the branch lengths. Defaults to 3.
#' @param cex A number for the graphic scaling factor of the text for branch lengths. Defaults to 0.5.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' plot(anole.tree)
#' add.branch.lengths(anole.tree)

add.branch.lengths <- function (
  tree,
  rounding = 3,
  cex = 0.5,
  ...
)
{ # Begin the function
  if (!require(ape)) { stop("Please run  `install.packages('ape')`  first.") }

  # Vet the input
  if (!exists(quote(tree))) {
    stop("No argument specified to `tree`. (See the help entry: `?add.branch.lengths`)\n")
  }
  if (!(class(tree)[1] == "phylo")) {
    stop("Argument `tree` is not a recognized data type. (See the help entry: `?add.branch.lengths`)\n")
  }
  if (class(rounding)[1] != "numeric") {
    stop("Error in argument `rounding`. (See the help entry: `?add.branch.lengths`)\n")
  } else {
    rounding <- rounding[1]
  }
  if (class(cex)[1] != "numeric") {
     stop("Error in argument `cex`. (See the help entry: `?add.branch.lengths`)\n")
  } else { cex <- cex[1] }
  # End preliminary vetting of the input and arguments

  # Do the thing
  edgelabels(text = signif(tree$edge.length,rounding), adj = c(0.5,-0.25), frame = "none", bg = NULL, cex = cex, ...)

} # End of function
