#' Infer phylogeny for aligned DNA or amino acid sequences
#'
#' @param x an \code{MsaDNAMultipleAlignment} or \code{MsaAAMultipleAlignment} object.
#' @param method A character indicating the phylogenetic method. Available methods include \code{"NJ"} (default) or \code{"UPGMA"}
#' @param model A character indicating the substitution model to be used.
#'     For DNA alignments, available methods include "F81" (default) and "JC69".
#'     For amino acid alignments: "WAG", "JTT", "LG", "Dayhoff", "cpREV", "mtmam", "mtArt", "MtZoa", "mtREV24", "VT","RtREV", "HIVw", "HIVb", "FLU", "Blosum62" (default), "Dayhoff_DCMut" and "JTT_DCMut".
#'     For more information see \code{?phangorn::dist.ml}.
#' @param truncate.names An optional number of characters by which to truncate sequence names.
#' @param show.tree A logical argument specifying whether to display the resulting phylogeny. Defaults to \code{TRUE}.
#' @param verbose A logical argument specifying whether to display details. Defaults to \code{TRUE}.
#'
#' @details This function is intended as a user-friendly wrapper for \code{phangorn::dist.ml} and \code{NJ} or \code{upgma} functions.
#'     Additional arguments will be passed to \code{dist.ml}.
#'
#' @return Returns an \code{phylo} tree object.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' anole.alignment <- align.sequences(anole.ND2)
#' anole.tree <- infer.phylogeny(anole.alignment)
#'

infer.phylogeny <- function (
  x,
  method = c("NJ","UPGMA"),
  model = c("F81","JC69",  "WAG", "JTT", "LG", "Dayhoff", "cpREV", "mtmam", "mtArt", "MtZoa", "mtREV24", "VT","RtREV", "HIVw", "HIVb", "FLU", "Blosum62", "Dayhoff_DCMut", "JTT_DCMut"),
  truncate.names = NULL,
  show.tree = TRUE,
  verbose = TRUE,
  ...
)
{ # Begin the function
  if (!require(phangorn)) { stop("Please run  `install.packages('phangorn')`  first.") }

  # Vet the input
  if (!exists(quote(x))) {
    stop("No argument specified to `x`. (See the help entry: `?infer.phylogeny`)\n")
  }
  if (!grepl("MultipleAlignment$",class(x))) {
    stop("Argument `x` is not a recognized data type. (See the help entry: `?infer.phylogeny`)\n")
  }
  if (grepl("DNA",class(x)[1])) {
    type.x <- "DNA"
  } else {
    type.x <- "AA"
    if (any(model %in% c("F81","JC69"))) { model <- "Blosum62" }
  }

  if (!(exists(quote(method)))) { method <- "NJ" }
  if ((class(method)[1] != "character")) {
    stop("Error in argument `method`. (See the help entry: `?infer.phylogeny`)\n")
  } else { method <- toupper(method[1]) }
  if (!(method %in% c("NJ","UPGMA"))) {
    stop("Unrecognized `method`. (See the help entry: `?infer.phylogeny`)\n")
  }
  if ((class(model)[1] != "character")) {
    stop("Error in argument `model`. (See the help entry: `?infer.phylogeny`)\n")
  }
  if (!(model %in% c("F81","JC69",  "WAG", "JTT", "LG", "Dayhoff", "cpREV", "mtmam", "mtArt", "MtZoa", "mtREV24", "VT","RtREV", "HIVw", "HIVb", "FLU", "Blosum62", "Dayhoff_DCMut", "JTT_DCMut"))) {
    stop("Unrecognized `model`. (See the help entry: `?infer.phylogeny`)\n")
  }
  if ((model %in% c("F81","JC69")) & (type.x == "AA")) {
    stop("Amino acid alignment provided for a DNA substitution model. (See the help entry: `?infer.phylogeny`)\n")
  }
  if (!(model %in% c("F81","JC69")) & (type.x == "DNA")) {
    stop("DNA alignment provided for an amino acid substitution model. (See the help entry: `?infer.phylogeny`)\n")
  }
  if (!(class(truncate.names)[1] %in% c("numeric","NULL"))) {
    stop("Error in argument `truncate.names`. (See the help entry: `?infer.phylogeny`)\n")
  } else { truncate.names <- truncate.names[1] }

  # Convert the MSA MultipleAlignment object to a phangorn phyDat data type
  filename <- paste0("temp", format(Sys.time(), "%Y%m%d%H%M%s"),".fna")
  sativum::write.fasta(x, filename = filename, verbose = FALSE)
  x <- read.phyDat(file = filename, format = "fasta", type = type.x)
  system2(command = "rm", args = filename)

  if (!is.null(truncate.names)) {
    if (class(truncate.names)[1] != "numeric") {
      stop("Argument `truncate.names` must be a whole number. (See the help entry: `?infer.phylogeny`)\n")
    } else {
      names(x) <- strtrim(names(x), truncate.names[1])
    }
  }

  # Calculate the distance matrix
  dm  <- dist.ml(x, model = model, ...)

  # Build the tree
  if (method == "NJ") {
    tree <- NJ(dm)
  } else {
    tree <- upgma(dm)
  }

  if (show.tree) {
    infer.phylogeny(tree)
  }

  if (verbose) {
    message("Inferred phylogeny for ",length(x)," aligned sequences using ",method," with model ",model)
  }

  return(tree)

} # End of function
