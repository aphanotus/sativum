#' Combine two sequence data objects
#'
#' Combine two sequence data objects of the same type. The objects must be one of the following:
#'     1. Character strings containing FASTA format sequences as downloaded from NCBI.
#'     2. Character vectors of sequences.
#'     3. Data frames with columns for the sequence names and sequence text, e.g. as produced by \code{phylotools::read.fasta}.
#'     4. \code{Biostrings::DNAStringSet} or 5. \code{AAStringSet} objects.
#'
#' @param x1,x2 Objects of biological sequence data.
#' @param verbose A logical argument specifying whether to display the summary output. Defaults to \code{TRUE}.
#'
#' @return Returns one sequence data object in the same format as the two that are provided. Sequences in `x1` will appear before those in `x2`.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' accession.ids <- c("EF591774.1", "AF055971.2", "AF055967.2", "AF055927.2", "AY296172.1", "AF294303.1", "AF294297.1", "AF055976.2", "AF055945.2", "AF294317.1", "AY296196.1", "AF055966.2", "AF055939.2", "AF055940.2", "AY296195.1", "AY263052.1", "AY296163.1")
#' anole.nd2 <- combine.sequences(accession.ids)

combine.sequences <- function ( x1, x2, verbose = TRUE, ... )
{ # Begin the function

  # Vet the input
  # if (!(exists(quote(x1) & exists(quote(x2))))) {
  #   stop("Arguments must be specified to `x1` and `x2`. (See the help entry: `?combine.sequences`.)\n")
  # }
  if (!(class(x1)[1] %in% c("character","data.frame","DNAStringSet","AAStringSet"))) {
    stop("Argument `x1` is not a recognized data type. (See the help entry: `?combine.sequences`)\n")
  }
  if (!(class(x2)[1] %in% c("character","data.frame","DNAStringSet","AAStringSet"))) {
    stop("Argument `x2` is not a recognized data type. (See the help entry: `?combine.sequences`)\n")
  }
  if (class(x1)[1] != class(x2)[1]) {
    stop("The objects provided to `x1` and `x2` are different data types. (See the help entry: `?combine.sequences`)\n")
  }
  if (class(verbose)[1] != "logical") {
     stop("Error in argument `verbose`. (See the help entry: `?combine.sequences`.)\n")
  } else { verbose <- verbose[1] }
  # End preliminary vetting of the input and arguments

  # Output for each format

  if (class(x1)[1] == "character") {
    # Input is raw FASTA as a single string
    if (length(x1)==1 & length(x2)==1) {
      output <- paste0(x1,x2)
      sequence.numbers <- c(lengths(regmatches(x1, gregexpr(">", x1))),lengths(regmatches(x2, gregexpr(">", x2))))
    }
    else {
      # Input is a character vector
      output <- c(x1,x2)
      sequence.numbers <- c(length(x1),length(x2))
    }
  } else {
    if (class(x1)[1] == "data.frame") {
      if ((dim(x1)[2]) != (dim(x2)[2])) { stop("Data frames provided to `x1` and `x2` must contain the same columns. (See the help entry: `?combine.sequences`.)\n") }
      if (any(colnames(x1) != colnames(x2))) { stop("Data frames provided to `x1` and `x2` must contain the same columns. (See the help entry: `?combine.sequences`.)\n") }
      output <- rbind(x1,x2)
      sequence.numbers <- c((dim(x1)[1]),(dim(x2)[1]))
    } else {
      if (class(x1)[1] == "DNAStringSet") {
        # Input is an DNAStringSet object
        if (!require(Biostrings, quietly = TRUE, warn.conflicts = FALSE)) { stop("Please run  `BiocManager::install('Biostrings')`  first") }
        output <- DNAStringSet(c(as.character(x1),as.character(x2)))
        sequence.numbers <- c(length(x1),length(x2))
      } else {
        if (class(x1)[1] == "AAStringSet") {
          # Input is an AAStringSet object
          if (!require(Biostrings, quietly = TRUE, warn.conflicts = FALSE)) { stop("Please run  `BiocManager::install('Biostrings')`  first") }
          output <- AAStringSet(c(as.character(x1),as.character(x2)))
          sequence.numbers <- c(length(x1),length(x2))
        }
      }
    }
  }

  # Confirmation
  if (verbose) { message("Combined ",sequence.numbers[1]," and ",sequence.numbers[2]," sequences: ",sum(sequence.numbers)) }

  return(output)

} # End of function
