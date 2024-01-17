#' Read in a FASTA biological sequence file
#'
#' @param filename A character with the path and filename to FASTA file
#' @param format A character name for the output format.
#'     \code{"character"}: A named vector of sequences.
#'     \code{"data.frame"} or \code{"table"}: A data frame with columns for the sequence names and sequence text, as produced by \code{phylotools::read.fasta}.
#'     \code{"DNAStringSet"}: A \code{Biostrings::DNAStringSet} object. (Default)
#'     \code{"AAStringSet"}: A \code{Biostrings::AAStringSet} object.
#' @param verbose A logical argument specifying whether to display the summary output. Defaults to \code{TRUE}.
#'
#' @details This function is intended as a user-friendly wrapper for \code{phylotools::read.fasta},
#'     and converts the output into one of several formats in the local environment.
#'
#' @return Returns sequence data in the format specified by the \code{format} argument.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' write.fasta(anole.ND2, "anole.nd2.fna")
#'
#' seqs <- read.fasta("anole.nd2.fna")
#'

read.fasta <- function (
  filename,
  format = "DNAStringSet",
  verbose = TRUE,
  ...
)
{ # Begin the function

  # Don't bother running anything if dependent package isn't installed!
  if (!require(phylotools)) {
    stop("Please run  `install.packages('phylotools')`  first.")
  }
  if (!require(stringr)) {
    stop("Please run  `install.packages('stringr')`  first.")
  }

  # Vet the input
  if (!exists(quote(filename))) {
    stop("No argument specified to `filename`. (See the help entry: `?read.fasta`.)\n")
  }
  if (class(filename)[1] != "character") {
    stop("Error in argument `filename`. (See the help entry: `?read.fasta`.)\n")
  } else { filename <- filename[1] }
  if (class(format)[1] != "character") {
    stop("Error in argument `format`. (See the help entry: `?read.fasta`.)\n")
  } else { format <- format[1] }
  allowable.formats <- c("raw", "character", "data.frame", "table", "DNAStringSet", "AAStringSet")
  if (!(strtrim(format,1) %in% strtrim(allowable.formats,1))) {
    warning("You have entered an invalid value for the argument `format`. Defaulting to `format = 'DNAStringSet'.`\n")
    format <- "DNAStringSet"
  }
  if (class(verbose)[1] != "logical") {
     stop("Error in argument `verbose`. (See the help entry: `?read.fasta`.)\n")
  } else { verbose <- verbose[1] }
  # End preliminary vetting of the input and arguments

  # Do the thing
  seqs <- phylotools::read.fasta(file = filename)

  # Output for each format

  # Output: named character vector format
  if (strtrim(format,1)=="c") {
    if (verbose) { message("Sequences imported: ",dim(seqs)[1]) }
    s <- seqs$seq.name
    seqs <- seqs$seq.text
    names(seqs) <- s
    return(seqs)
  }

  # Output: data frame
  if (strtrim(format,1)=="d" | strtrim(format,1)=="t") {
    if (verbose) { message("Sequences imported: ",dim(seqs)[1]) }
    return(seqs)
  }

  # Output: DNAStringSet
  if (strtrim(format,1)=="D") {
    if (!require(Biostrings)) { stop("Please run  `BiocManager::install('Biostrings')`  first.") }
    s <- seqs$seq.name
    seqs <- seqs$seq.text
    names(seqs) <- s
    # Check that the sequence doesn't include any amino acid abbreviations
    aa.letters <- c("E","F","I","L","P")
    if (any(unlist(lapply(aa.letters, function(x) { grepl(x, seqs, ignore.case = TRUE)})))) {
      # If so, save it as amino acid data and throw a warning
      seqs.ss <- AAStringSet(seqs)
      warning("These data appear to be amino acid sequences and have been important as an AAStringSet.")
    } else {
      seqs.ss <- DNAStringSet(seqs)
    }
    if (verbose) { print(seqs.ss) }
    return(seqs.ss)
  }

  # Output: AAStringSet
  if (strtrim(format,1)=="A") {
    if (!require(Biostrings)) { stop("Please run  `BiocManager::install('Biostrings')`  first.") }
    s <- seqs$seq.name
    seqs <- seqs$seq.text
    names(seqs) <- s
    seqs.ss <- AAStringSet(seqs)
    if (verbose) { print(seqs.ss) }
    return(seqs.ss)
  }

} # End of function
