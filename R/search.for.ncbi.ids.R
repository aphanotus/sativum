#' Search NCBI Entrez database for sequence IDs
#'
#' @param query A character search term.
#' @param database A character name of an NCBI database. Defaults to \code{"nuccore"}, or the non-redundant nucleotide database (NR).
#'     For a complete list of options run \code{rentrez::entrez_db_searchable()}
#' @param verbose A logical argument specifying whether to display the summary output. Defaults to \code{FALSE}.
#'
#' @details This function is intended as a user-friendly wrapper for \code{rentrez::entrez_search} and accepts other arguments to that function.
#'
#' @return Returns an character vector of NCBI UIDs. These are the \code{esearch} object \code{ids} elements as generated by \code{rentrez::entrez_search}.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' new.ids <- search.for.ncbi.ids("Anolis sheplani ND2")
#'

search.for.ncbi.ids <- function (
  query,
  database = "nuccore",
  verbose = FALSE,
  ...
)
{ # Begin the function

  # Don't bother running anything if dependent package isn't installed!
  if (!require(rentrez, quietly = TRUE, warn.conflicts = FALSE)) { stop("Please run  `install.packages('rentrez')`  first.") }

  # Vet the input
  if (!exists(quote(query))) {
    stop("No argument specified to `query`. (See the help entry: `?search.for.ncbi.ids`.)\n")
  }
  if (class(query)[1] != "character") {
    stop("Error in argument `query`. (See the help entry: `?search.for.ncbi.ids`.)\n")
  } else { query <- query[1] }
  if (class(database)[1] != "character") {
    stop("Error in argument `database`. (See the help entry: `?search.for.ncbi.ids`.)\n")
  } else { database <- database[1] }
  if (!(database %in% entrez_dbs())) {
    warning("You have entered an invalid value for the argument `database`. Defaulting to `database = 'nuccore'.`\n")
    database <- "nuccore"
  }
  if (class(verbose)[1] != "logical") {
     stop("Errorin argument `verbose`. (See the help entry: `?search.for.ncbi.ids`.)\n")
  } else { verbose <- verbose[1] }
  # End preliminary vetting of the input and arguments

  # Do the thing
  search.result <- entrez_search(db=database, term=query, ...)

  # Verbose output
  if (verbose) { print(search.result) }

  # Return only the UIDs
  return(search.result$ids)

} # End of function
