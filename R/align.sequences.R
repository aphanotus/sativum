#' Align DNA or amino acid sequences
#'
#' @param x An object of biological sequence data. Must be one of the following:
#'     A character string containing the FASTA format sequences as downloaded from NCBI.
#'     A named vector of sequences.
#'     A data frame with columns for the sequence names and sequence text, as produced by \code{phylotools::read.fasta}.
#'     A \code{Biostrings::DNAStringSet} or \code{AAStringSet} object.
#' @param method The alignment algorithm. Options include \code{"ClustalOmega"} (default), \code{"ClustalW"} and \code{"Muscle"}.
#' @param verbose A logical argument specifying whether to display confirmation. Defaults to \code{TRUE}.
#'
#' @details This function is intended as a user-friendly wrapper for \code{msa::msa} and accepts other arguments to that function.
#'
#' @return Returns an \code{MsaDNAMultipleAlignment} or \code{MsaAAMultipleAlignment} object.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' anole.alignment <- align.sequences(anole.ND2)
#' print(anole.alignment, show="complete")
#'

align.sequences <- function (x, method = "ClustalOmega", verbose = TRUE, ...)
{ # Begin the function

  # Don't bother running anything if dependent package isn't installed!
  if (!require(msa)) {
    stop("Please run  `BiocManager::install('msa')`  first.")
  }

  # Vet the input
  if (!exists(quote(x))) {
    stop("No argument specified to `x`. (See the help entry: `?write.fasta`)\n")
  }
  if (!(class(x)[1] %in% c("character","data.frame","DNAStringSet","AAStringSet"))) {
    stop("Argument `x` is not a recognized data type. (See the help entry: `?write.fasta`)\n")
  }
  if (class(verbose)[1] != "logical") {
     stop("Error in argument `verbose`. (See the help entry: `?align.sequences`.)\n")
  } else { verbose <- verbose[1] }
  # End preliminary vetting of the input and arguments

  # Convert misc input to an XStringSet
  if (class(x)[1] == "character") {
    # Input is raw FASTA as a single string
    if (length(x)==1) {
      if (!require(stringr)) { stop("Please run  `install.packages('stringr')`  first.") }
      x <- unlist(str_split(x, ">"))[-1]
      seq.names <- str_split_fixed(x, "\\\n", 2)[,1]
      x <- str_split_fixed(x, "\\\n", 2)[,2]
      names(x) <- seq.names
      x <- gsub("\\\n","",x)
      # Is it DNA or AA sequence?
      nts <- mean((lengths(regmatches(x, gregexpr("[aA]", x))) + lengths(regmatches(x, gregexpr("[cC]", x))) + lengths(regmatches(x, gregexpr("[gG]", x))) + lengths(regmatches(x, gregexpr("[tT]", x)))) / nchar(x))
      if (nts > 0.9) {  x <- DNAStringSet(x) } else { x <- AAStringSet(x) }
    }
    else {
      # Input is a character vector
      x <- gsub("\\\n","",x)
      # Is it DNA or AA sequence?
      nts <- mean((lengths(regmatches(x, gregexpr("[aA]", x))) + lengths(regmatches(x, gregexpr("[cC]", x))) + lengths(regmatches(x, gregexpr("[gG]", x))) + lengths(regmatches(x, gregexpr("[tT]", x)))) / nchar(x))
      if (nts > 0.9) {  x <- DNAStringSet(x) } else { x <- AAStringSet(x) }
    }
  } else {
    if (class(x)[1] == "data.frame") {
      if ("seq.name" %in% colnames(x) & "seq.text" %in% colnames(x)) {
        # Input is a data.frame as produced by phytools::read.fasta
        seq.names <- x$seq.name
        x <- x$seq.text
        names(x) <- seq.names
        x <- gsub("\\\n","",x)
        # Is it DNA or AA sequence?
        nts <- mean((lengths(regmatches(x, gregexpr("[aA]", x))) + lengths(regmatches(x, gregexpr("[cC]", x))) + lengths(regmatches(x, gregexpr("[gG]", x))) + lengths(regmatches(x, gregexpr("[tT]", x)))) / nchar(x))
        if (nts > 0.9) { x <- DNAStringSet(x) } else { x <- AAStringSet(x) }
      } else {
        stop("Argument `x` is not a recognized data frame structure (See the help entry: `?write.fasta`)\n")
      }
    }
  }

  # Do the thing
  output <- msa(x, method = method, verbose = FALSE, ...)

  # Confirmation
  if (verbose) { message("Aligned ",length(x)," sequences using method ",method) }

  return(output)

} # End of function
