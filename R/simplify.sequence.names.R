#' Get simplified names for NCBI sequences
#'
#' @param ids A character vector of valid NCBI UIDs or accession numbers.
#' @param database A character name of an NCBI database. Defaults to \code{"nuccore"}, or the non-redundant nucleotide database (NR).
#'     For a complete list of options run \code{rentrez::entrez_db_searchable()}
#' @param sequence.name An optional sequence name.
#' @param include.species.name A logical argument specifying whether to include the species name in the sequence name. Defaults to \code{TRUE}.
#' @param include.accession A logical argument specifying whether to include the GenBank Accession number in the sequence name. Defaults to \code{TRUE}.
#' @param separator A character to use to separate the components of sequence names. Defaults to the underscore.
#'
#' @details This function is intended as a user-friendly wrapper for \code{rentrez::entrez_summary} and accepts other arguments to that function.
#'
#' @return Returns an character vector of NCBI IDs. These are the \code{esearch} object \code{ids} elements as generated by \code{rentrez::entrez_search}.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' accession.ids <- c("EF591774.1", "AF055971.2", "AF055967.2", "AF055927.2", "AY296172.1", "AF294303.1", "AF294297.1", "AF055976.2", "AF055945.2", "AF294317.1", "AY296196.1", "AF055966.2", "AF055939.2", "AF055940.2", "AY296195.1", "AY263052.1", "AY296163.1")
#' short.names <- simplify.sequence.names(ids  = accession.ids, sequence.name = "ND2")

simplify.sequence.names <- function (
  ids,
  database = "nuccore",
  sequence.name = "",
  include.species.name = TRUE,
  include.accession = TRUE,
  separator = "_",
  ...
)
{ # Begin the function

  # Don't bother running anything if dependent package isn't installed!
  if (!require(rentrez, quietly = TRUE, warn.conflicts = FALSE)) { stop("Please run  `install.packages('rentrez')`  first.") }

  # Vet the input
  if (!exists(quote(ids))) {
    stop("No argument specified to `ids`. (See the help entry: `?simplify.sequence.names`.)\n")
  }
  if (class(ids)[1] != "character") {
    stop("Error in argument `ids`. (See the help entry: `?simplify.sequence.names`.)\n")
  }
  if (class(database)[1] != "character") {
    stop("Error in argument `database`. (See the help entry: `?simplify.sequence.names`.)\n")
  } else { database <- database[1] }
  if (!(database %in% entrez_dbs())) {
    warning("You have entered an invalid value for the argument `database`. Defaulting to `database = 'nuccore'.`\n")
    database <- "nuccore"
  }
  if (class(sequence.name)[1] != "character") {
    stop("Error in argument `sequence.name`. (See the help entry: `?simplify.sequence.names`.)\n")
  } else { sequence.name <- sequence.name[1] }
  if (class(separator)[1] != "character") {
    stop("Error in argument `separator`. (See the help entry: `?simplify.sequence.names`.)\n")
  } else { separator <- separator[1] }
  if (class(include.species.name)[1] != "logical") {
    stop("Error in argument `include.species.name`. (See the help entry: `?simplify.sequence.names`.)\n")
  } else { include.species.name <- include.species.name[1] }
  if (class(include.accession)[1] != "logical") {
    stop("Error in argument `include.accession`. (See the help entry: `?simplify.sequence.names`.)\n")
  } else { include.accession <- include.accession[1] }
  if (!include.species.name & !include.accession) {
    warning("You have set both `include.species.name` and `include.accession` to FALSE. How do you expect to distinguish sequences?")
  }
  # End preliminary vetting of the input and arguments

  # Simplify names
  seq.details <- entrez_summary(db=database, id=ids, ...)

  if (is(seq.details)=="esummary_list") {
    taxids <- unname(unlist(lapply(seq.details, function(x) { x$taxid } )))
    taxonomy.details <- entrez_summary(db="taxonomy", id=taxids)
    if (include.species.name) {
      species.names <- unname(unlist(lapply(taxonomy.details, function(x) { paste0(x$genus, separator, x$species) } )))
    } else {
      species.names <- NULL
    }
    if (include.accession) {
      accession.ids <- unlist(lapply(1:length(seq.details), function(i) { seq.details[[i]]$accessionversion }))
    } else {
      accession.ids <- NULL
    }
  } else {
    if (is(seq.details)=="esummary") {
      taxids <- seq.details$taxid
      taxonomy.details <- entrez_summary(db="taxonomy", id=taxids)
      if (include.species.name) {
        species.names <- paste0(taxonomy.details$genus, separator, taxonomy.details$species)
      } else {
        species.names <- NULL
      }
      if (include.accession) {
        accession.ids <- seq.details$accessionversion
      } else {
        accession.ids <- NULL
      }
    }
  }

  # seq.names <- paste0(species.names, separator, sequence.name, separator, accession.ids)
  seq.names <- paste(species.names, sequence.name, accession.ids, sep = separator)

  seq.names <- gsub(paste0(separator,separator),separator,seq.names)
  seq.names <- gsub(paste0("^",separator),"",seq.names)
  seq.names <- gsub(paste0(separator,"$"),"",seq.names)

  return(seq.names)

} # End of function
