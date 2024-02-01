#' Get simplified names for NCBI sequences
#'
#' @param x A character vector of complex names.
#' @param separator A character to use to separate the components of sequence names.
#' @param words An integer specifying the number of words to retain. Defaults to 2.
#' @param abbreviate.genus A logical argument specifying whether to treat the first word as a genus name and abbreviate it to its first character.
#' @param prefix A character to prefix to all names.
#' @param postfix A character to postfix to all names.
#'
#' @details This function takes complex sequence names and shortens them.
#'    Ideal for taking just the gene or species names that begin longer identifiers.
#'    If the separator is not specified then the function will attempt to detect one.
#'
#' @return Returns an character vector.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' simplify.names(anole.tree$tip.label)
#' simplify.names(anole.tree$tip.label, postfix = "ND2")

simplify.names <- function (x, separator = NULL, words = 2, abbreviate.genus = TRUE, prefix = "", postfix = "")
{ # Begin the function

  # Don't bother running anything if dependent package isn't installed!
  if (!require(stringr, quietly = TRUE, warn.conflicts = FALSE)) { stop("Please run  `install.packages('stringr')`  first.") }

  # Vet the input
  if (class(x)[1] != "character") {
    stop("Error in argument `x`. (See the help entry: `?simplify.names`.)\n")
  }
  # End preliminary vetting of the input and arguments

  if (is.null(separator)) {
    if (sum(grepl(" ",x)) > (length(x)/2)) { separator <- " " }
    if (sum(grepl("_",x)) > (length(x)/2)) { separator <- "_" }
    if (is.null(separator)) {
      stop("Tried and failed to detect a separator. Try to specify an arguement to `separator`.  (See the help entry: `?simplify.names`.)\n")
    }
  }

  # Simplify names
  s <- x
  for (i in 1:(words-1)) {
    s <- sub(separator, "PLACEHOLDER", s)
  }
  s <- str_split_fixed(s, separator, 2)[,1]
  s <- sub("PLACEHOLDER", separator,  s)

  if (abbreviate.genus) {
    s1 <- strtrim(s, 1)
    s2 <- str_split_fixed(s, separator, 2)[,2]
    s <- paste0(s1,".",separator,s2)
  }

  if (nchar(prefix) >0 ) {
    s <- paste0(prefix,separator,s)
  }
  if (nchar(postfix) >0 ) {
    s <- paste0(s,separator,postfix)
  }

  return(s)

} # End of function
