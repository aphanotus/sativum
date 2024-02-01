#' Find the distance between two tips of a phylogenetic tree
#'
#' @param tree An object of class \code{phylo}.
#' @param tip1 A character exactly matching one tip label in \code{tree}.
#' @param tip2 A character exactly matching another tip label in \code{tree}.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' plot(anole.tree)
#' phylogenetic.distance(anole.tree, "A. olssoni","A. cybotes")
#' phylogenetic.distance(anole.tree, "A. cybotes", "A. occultus")
#'
#' # To find all pairwise distances
#' ape::cophenetic.phylo(anole.tree)


phylogenetic.distance <- function (tree, tip1, tip2)
{ # Begin the function
  if (!require(ape, quietly = TRUE, warn.conflicts = FALSE)) { stop("Please run  `install.packages('ape')`  first.") }

  # Vet the input
  if (!exists(quote(tree))) {
    stop("No argument specified to `tree`. (See the help entry: `?phylogenetic.distance`)\n")
  }
  if (!exists(quote(tip1))) {
    stop("No argument specified to `tip1`. (See the help entry: `?phylogenetic.distance`)\n")
  }
  if (!exists(quote(tip2))) {
    stop("No argument specified to `tip2`. (See the help entry: `?phylogenetic.distance`)\n")
  }
  if (!(class(tree)[1] == "phylo")) {
    stop("Argument `tree` is not a recognized data type. (See the help entry: `?phylogenetic.distance`)\n")
  }
  if (class(tip1)[1] != "character") {
    stop("Error in argument `tip1`. (See the help entry: `?phylogenetic.distance`)\n")
  } else {
    tip1 <- tip1[1]
  }
  if (class(tip2)[1] != "character") {
    stop("Error in argument `tip2`. (See the help entry: `?phylogenetic.distance`)\n")
  } else {
    tip2 <- tip2[1]
  }
  if (!(tip1 %in% tree$tip.label)) {
    stop("Error in argument `tip1`. (See the help entry: `?phylogenetic.distance`)\n")
  }
  if (!(tip2 %in% tree$tip.label)) {
    stop("Error in argument `tip2`. (See the help entry: `?phylogenetic.distance`)\n")
  }
  if (tip1 == tip2) {
    stop("`tip1` and `tip2` must be different!. (See the help entry: `?phylogenetic.distance`)\n")
  }
  # End preliminary vetting of the input and arguments

  # Do the thing
  x <- cophenetic.phylo(tree)
  return(x[tip1, tip2])

} # End of function
