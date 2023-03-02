#' Download sequence data from NCBI Entrez database based on sequence IDs
#'
#' @param ids A character vector of valid NCBI UIDs or accession numbers.
#' @param database A character name of an NCBI database. Defaults to \code{"nuccore"}, or the non-redundant nucleotide database (NR).
#'     For a complete list of options run \code{rentrez::entrez_db_searchable()}
#' @param format A character name for the output format.
#'     \code{"raw"}: A character string containing the FASTA format sequences as downloaded from NCBI.
#'     \code{"character"}: A named vector of sequences.
#'     \code{"data.frame"} or \code{"table"}: A data frame with columns for the sequence names and sequence text, as produced by \code{phytools::read.fasta}.
#'     \code{"DNAStringSet"}: A \code{Biostrings::DNAStringSet} object. (Default)
#'     \code{"AAStringSet"}: A \code{Biostrings::AAStringSet} object.
#' @param simplify.names A logical argument specifying whether to simplify the sequence names to species - sequence name - accession number. Defaults to \code{TRUE}.
#'     The species names and accession numbers are pulled from NCBI. The sequence name must be provided by the user.
#' @param sequence.name An optional sequence name.
#' @param separator A character to use to separate the components of simplified sequence names. Defaults to the underscore.
#' @param verbose A logical argument specifying whether to display the summary output. Defaults to \code{TRUE}.
#'
#' @details This function is intended as a user-friendly wrapper for \code{rentrez::entrez_fetch} and accepts other arguments to that function.
#'
#' @return Returns sequence data in the format specified by the \code{format} argument.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' accession.ids <- c("EF591774.1", "AF055971.2", "AF055967.2", "AF055927.2", "AY296172.1", "AF294303.1", "AF294297.1", "AF055976.2", "AF055945.2", "AF294317.1", "AY296196.1", "AF055966.2", "AF055939.2", "AF055940.2", "AY296195.1", "AY263052.1", "AY296163.1")
#' anole.nd2 <- fetch.sequence(accession.ids)

fetch.sequence <- function (
  ids,
  database = "nuccore",
  format = "DNAStringSet",
  simplify.names = TRUE,
  sequence.name = "",
  separator = "_",
  verbose = TRUE,
  ...
)
{ # Begin the function

  # Don't bother running anything if dependent package isn't installed!
  if (!require(rentrez)) {
    stop("Please run  `install.packages('rentrez')`  first.")
  }

  # Vet the input
  if (!exists(quote(ids))) {
    stop("No argument specified to `ids`. (See the help entry: `?fetch.sequence`.)\n")
  }
  if (class(ids)[1] != "character") {
    stop("Error in argument `ids`. (See the help entry: `?fetch.sequence`.)\n")
  }
  if (class(database)[1] != "character") {
    stop("Error in argument `database`. (See the help entry: `?fetch.sequence`.)\n")
  } else { database <- database[1] }
  if (!(database %in% entrez_dbs())) {
    warning("You have entered an invalid value for the argument `database`. Defaulting to `database = 'nuccore'.`\n")
    database <- "nuccore"
  }
  if (class(format)[1] != "character") {
    stop("Error in argument `format`. (See the help entry: `?fetch.sequence`.)\n")
  } else { format <- format[1] }
  allowable.formats <- c("raw", "character", "data.frame", "table", "DNAStringSet", "AAStringSet")
  # allowable.formats <- strtrim(allowable.formats,1)
  if (!(strtrim(format,1) %in% strtrim(allowable.formats,1))) {
    warning("You have entered an invalid value for the argument `format`. Defaulting to `format = 'DNAStringSet'.`\n")
    format <- "DNAStringSet"
  }
  if (class(simplify.names)[1] != "logical") {
     stop("Errorin argument `simplify.names`. (See the help entry: `?fetch.sequence`.)\n")
  } else { simplify.names <- simplify.names[1] }
  if (class(sequence.name)[1] != "character") {
    stop("Error in argument `sequence.name`. (See the help entry: `?fetch.sequence`.)\n")
  } else { sequence.name <- sequence.name[1] }
  if (class(separator)[1] != "character") {
    stop("Error in argument `separator`. (See the help entry: `?fetch.sequence`.)\n")
  } else { separator <- separator[1] }
  if (class(verbose)[1] != "logical") {
     stop("Errorin argument `verbose`. (See the help entry: `?fetch.sequence`.)\n")
  } else { verbose <- verbose[1] }
  # End preliminary vetting of the input and arguments

  # # This trick often forces the NCBI website to respond.
  # invisible(curl::curl_fetch_memory(url="eutils.ncbi.nlm.nih.gov"))

  # Do the thing
  seqs.raw <- entrez_fetch(db=database, id=ids, rettype="fasta", ...)

  # Output for each format

  # Output: raw format
  if (strtrim(format,1)=="r") {
    if (verbose) {
      if (!require(stringr)) { stop("Please run  `install.packages('stringr')`  first.") }
      message("Sequences downloaded: ",str_count(seqs.raw, ">"))
    }
    return(seqs.raw)
  }

  # Parse raw sequence
  if (!require(stringr)) { stop("Please run  `install.packages('stringr')`  first.") }
  seqs <- str_split_fixed(seqs.raw,">",length(ids)+1)[1,-1]
  seq.names <- str_split_fixed(seqs,"\n",2)[,1]
  seqs <- str_split_fixed(seqs,"\n",2)[,2]
  seqs <- gsub("\\n","",seqs)

  # Simplify names
  if (simplify.names) {
    seq.names <- simplify.sequence.names(ids = ids, database = database, sequence.name = sequence.name, ...)
  }
  names(seqs) <- seq.names

  # Output: named character vector format
  if (strtrim(format,1)=="c") {
    if (verbose) { message("Sequences downloaded: ",length(seqs)) }
    return(seqs)
  }

  # Output: data frame
  if (strtrim(format,1)=="d" | strtrim(format,1)=="t") {
    if (verbose) { message("Sequences downloaded: ",length(seqs)) }
    seqs <- data.frame(
      seq.name = seq.names,
      seq.text = seqs
    )
    return(seqs)
  }

  # Output: DNAStringSet
  if (strtrim(format,1)=="D") {
    if (!require(Biostrings)) { stop("Please run  `BiocManager::install('Biostrings')`  first.") }
    seqs.ss <- DNAStringSet(seqs)
    if (verbose) { print(seqs.ss) }
    return(seqs.ss)
  }

  # Output: AAStringSet
  if (strtrim(format,1)=="A") {
    if (!require(Biostrings)) { stop("Please run  `BiocManager::install('Biostrings')`  first.") }
    seqs.ss <- AAStringSet(seqs)
    if (verbose) { print(seqs.ss) }
    return(seqs.ss)
  }

} # End of function


# Redundant definition to avoid confusion!
# Be sure to mirror this code
fetch.sequences <- function (
  ids,
  database = "nuccore",
  format = "DNAStringSet",
  simplify.names = TRUE,
  sequence.name = "",
  separator = "_",
  verbose = TRUE,
  ...
)
{ # Begin the function

  # Don't bother running anything if dependent package isn't installed!
  if (!require(rentrez)) {
    stop("Please run  `install.packages('rentrez')`  first.")
  }

  # Vet the input
  if (!exists(quote(ids))) {
    stop("No argument specified to `ids`. (See the help entry: `?fetch.sequence`.)\n")
  }
  if (class(ids)[1] != "character") {
    stop("Error in argument `ids`. (See the help entry: `?fetch.sequence`.)\n")
  }
  if (class(database)[1] != "character") {
    stop("Error in argument `database`. (See the help entry: `?fetch.sequence`.)\n")
  } else { database <- database[1] }
  if (!(database %in% entrez_dbs())) {
    warning("You have entered an invalid value for the argument `database`. Defaulting to `database = 'nuccore'.`\n")
    database <- "nuccore"
  }
  if (class(format)[1] != "character") {
    stop("Error in argument `format`. (See the help entry: `?fetch.sequence`.)\n")
  } else { format <- format[1] }
  allowable.formats <- c("raw", "character", "data.frame", "table", "DNAStringSet", "AAStringSet")
  # allowable.formats <- strtrim(allowable.formats,1)
  if (!(strtrim(format,1) %in% strtrim(allowable.formats,1))) {
    warning("You have entered an invalid value for the argument `format`. Defaulting to `format = 'DNAStringSet'.`\n")
    format <- "DNAStringSet"
  }
  if (class(simplify.names)[1] != "logical") {
     stop("Errorin argument `simplify.names`. (See the help entry: `?fetch.sequence`.)\n")
  } else { simplify.names <- simplify.names[1] }
  if (class(sequence.name)[1] != "character") {
    stop("Error in argument `sequence.name`. (See the help entry: `?fetch.sequence`.)\n")
  } else { sequence.name <- sequence.name[1] }
  if (class(separator)[1] != "character") {
    stop("Error in argument `separator`. (See the help entry: `?fetch.sequence`.)\n")
  } else { separator <- separator[1] }
  if (class(verbose)[1] != "logical") {
     stop("Errorin argument `verbose`. (See the help entry: `?fetch.sequence`.)\n")
  } else { verbose <- verbose[1] }
  # End preliminary vetting of the input and arguments

  # # This trick often forces the NCBI website to respond.
  # invisible(curl::curl_fetch_memory(url="eutils.ncbi.nlm.nih.gov"))

  # Do the thing
  seqs.raw <- entrez_fetch(db=database, id=ids, rettype="fasta", ...)

  # Output for each format

  # Output: raw format
  if (strtrim(format,1)=="r") {
    if (verbose) {
      if (!require(stringr)) { stop("Please run  `install.packages('stringr')`  first.") }
      message("Sequences downloaded: ",str_count(seqs.raw, ">"))
    }
    return(seqs.raw)
  }

  # Parse raw sequence
  if (!require(stringr)) { stop("Please run  `install.packages('stringr')`  first.") }
  seqs <- str_split_fixed(seqs.raw,">",length(ids)+1)[1,-1]
  seq.names <- str_split_fixed(seqs,"\n",2)[,1]
  seqs <- str_split_fixed(seqs,"\n",2)[,2]
  seqs <- gsub("\\n","",seqs)

  # Simplify names
  if (simplify.names) {
    seq.names <- simplify.sequence.names(ids = ids, database = database, sequence.name = sequence.name, ...)
  }
  names(seqs) <- seq.names

  # Output: named character vector format
  if (strtrim(format,1)=="c") {
    if (verbose) { message("Sequences downloaded: ",length(seqs)) }
    return(seqs)
  }

  # Output: data frame
  if (strtrim(format,1)=="d" | strtrim(format,1)=="t") {
    if (verbose) { message("Sequences downloaded: ",length(seqs)) }
    seqs <- data.frame(
      seq.name = seq.names,
      seq.text = seqs
    )
    return(seqs)
  }

  # Output: DNAStringSet
  if (strtrim(format,1)=="D") {
    if (!require(Biostrings)) { stop("Please run  `BiocManager::install('Biostrings')`  first.") }
    seqs.ss <- DNAStringSet(seqs)
    if (verbose) { print(seqs.ss) }
    return(seqs.ss)
  }

  # Output: AAStringSet
  if (strtrim(format,1)=="A") {
    if (!require(Biostrings)) { stop("Please run  `BiocManager::install('Biostrings')`  first.") }
    seqs.ss <- AAStringSet(seqs)
    if (verbose) { print(seqs.ss) }
    return(seqs.ss)
  }

} # End of function
