#' Create a FASTA file from sequence data
#'
#' @param x An object of biological sequence data. Must be one of the following:
#'     A character string containing the FASTA format sequences as downloaded from NCBI.
#'     A named vector of sequences.
#'     A data frame with columns for the sequence names and sequence text, as produced by \code{phytools::read.fasta}.
#'     A \code{Biostrings::DNAStringSet} or \code{AAStringSet} object.
#' @param filename A character with the path and filename to create on disk.
#' @param verbose A logical argument specifying whether to display confirmation. Defaults to \code{TRUE}.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' write.fasta(anole.ND2, "anole.ND2.fna")

write.fasta <- function (
  x,
  filename = "sequence.fasta",
  verbose = TRUE
)
{ # Begin the function

  # Vet the input
  if (!exists(quote(x))) {
    stop("No argument specified to `x`. (See the help entry: `?write.fasta`)\n")
  }
  if (!(class(x)[1] %in% c("character","data.frame","DNAStringSet","AAStringSet","MsaDNAMultipleAlignment","MsaAAMultipleAlignment"))) {
    stop("Argument `x` is not a recognized data type. (See the help entry: `?write.fasta`)\n")
  }
  if (class(filename)[1] != "character") {
    stop("Error in argument `filename`. (See the help entry: `?write.fasta`)\n")
  } else {
    filename <- filename[1]
  }
  if (class(verbose)[1] != "logical") {
     stop("Errorin argument `verbose`. (See the help entry: `?write.fasta`)\n")
  } else { verbose <- verbose[1] }
  # End preliminary vetting of the input and arguments

  # Output for each format

  if (class(x)[1] == "character") {
    # Input is raw FASTA as a single string
    if (length(x)==1) {
      write(x, filename)
      sequence.number <- 1
    }
    else {
      if (!is.null(names(x))) {
        # Input is a named character vector
        # Reconstitute FASTA format (single line)
        x <- gsub(">","",x)
        s <- paste0(">", names(x), "\n", x, "\n")
        write(s, filename)
      } else {
        # Input is a unnamed character vector
        s <- paste0("> sequence", 1:length(x), "\n", x, "\n")
        write(s, filename)
      }
      sequence.number <- length(x)
    }
  } else {
    if (class(x)[1] == "data.frame") {
      if ("seq.name" %in% colnames(x) & "seq.text" %in% colnames(x)) {
        # Input is a data.frame as produced by phytools::read.fasta
        s <- paste0(">", x[,"seq.name"], "\n", x[,"seq.text"], "\n")
        write(s, filename)
        sequence.number <- dim(x)[1]
      } else {
        stop("Argument `x` is not a recognized data frame structure (See the help entry: `?write.fasta`)\n")
      }
    } else {
      if (class(x)[1] %in% c("DNAStringSet","AAStringSet")) {
        # Input is an XStringSet object
        if (!require(Biostrings)) { stop("Please run  `BiocManager::install('Biostrings')`  first") }
        writeXStringSet(x=x, filepath=filename, append=FALSE, compress=FALSE, format="fasta")
        sequence.number <- length(x)
      } else {
        if (class(x)[1] %in% c("MsaDNAMultipleAlignment","MsaAAMultipleAlignment")) {
          # Input is an MsaMultipleAnlignment object
          if (!require(Biostrings)) { stop("Please run  `BiocManager::install('Biostrings')`  first") }
          writeXStringSet(x=unmasked(x), filepath=filename, append=FALSE, compress=FALSE, format="fasta")
          sequence.number <- length(unmasked(x))
        }
      }
    }
  }

  # Confirmation
  if (verbose) { message("Wrote ",sequence.number," sequences to file: ",filename) }

} # End of function
