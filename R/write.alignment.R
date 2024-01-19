#' Write a Biostrings MultipleAlignment object to a Phylip file
#'
#' @param x an \code{MsaDNAMultipleAlignment} or \code{MsaAAMultipleAlignment} object.
#' @param filename A character with the path and filename to create on disk. Set to \code{NULL} for display to the screen.
#' @param clean.names A logical argument specifying whether to make the sequence names compatible with Phylip format, replacing spaces and punctuation with underscores Defaults to \code{TRUE}.
#' @param truncate.names An optional number of characters by which to truncate sequence names.
#' @param sequence.line.length The number of sequence characters to include on a line. Set to \code{0} or \code{NULL} to remove interleaving. Default is \code{60}.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' anole.alignment <- align.sequences(anole.ND2)
#'
#' write.alignment(anole.alignment)
#' write.alignment(anole.alignment, filename = "anole.alignment.phy")
#'

write.alignment <- function (
  x,
  filename = NULL,
  clean.names = TRUE,
  truncate.names = NULL,
  sequence.line.length = 60,
  ...
)
{ # Begin the function

  if (!require(stringr, quietly = TRUE)) { stop("Please run  `install.packages('stringr')`  first.") }

  # Vet the input
  if (!exists(quote(x))) {
    stop("No argument specified to `x`. (See the help entry: `?write.alignment`)\n")
  }
  if (!grepl("MultipleAlignment$",class(x))) {
    stop("Argument `x` is not a recognized data type. (See the help entry: `?write.alignment`)\n")
  }
  if (!(class(filename)[1] %in% c("character","NULL"))) {
    stop("Error in argument `filename`. (See the help entry: `?write.alignment`)\n")
  }
  if (class(clean.names)[1] != "logical") {
    stop("Error in argument `clean.names`. (See the help entry: `?write.alignment`)\n")
  } else { clean.names <- clean.names[1] }
  if (!(class(truncate.names)[1] %in% c("numeric","NULL"))) {
    stop("Error in argument `truncate.names`. (See the help entry: `?write.alignment`)\n")
  } else { truncate.names <- truncate.names[1] }
  if (!(class(sequence.line.length)[1] %in% c("numeric","NULL"))) {
    stop("Error in argument `sequence.line.length`. (See the help entry: `?write.alignment`)\n")
  } else {
    if (is.null(sequence.line.length)) { sequence.line.length <- 0 }
    sequence.line.length <- round(sequence.line.length[1])
  }

  # Do the thing
  x <- as.character(x)

  if (clean.names) {
    safe.character <- "_"
    names(x) <- gsub("\\s", safe.character, names(x))
    names(x) <- gsub("\\.", safe.character, names(x))
    names(x) <- gsub("\\,", safe.character, names(x))
    names(x) <- gsub("\\?", safe.character, names(x))
    names(x) <- gsub("\\!", safe.character, names(x))
    names(x) <- gsub("\\:", safe.character, names(x))
    names(x) <- gsub("\\;", safe.character, names(x))
    names(x) <- gsub("\\(", safe.character, names(x))
    names(x) <- gsub("\\)", safe.character, names(x))
    names(x) <- gsub("\\[", safe.character, names(x))
    names(x) <- gsub("\\]", safe.character, names(x))
    names(x) <- gsub("\\{", safe.character, names(x))
    names(x) <- gsub("\\}", safe.character, names(x))
    names(x) <- gsub("\\'", safe.character, names(x))
    names(x) <- gsub("\\`", safe.character, names(x))
    names(x) <- gsub("\\~", safe.character, names(x))
    names(x) <- gsub("\\@", safe.character, names(x))
    names(x) <- gsub("\\#", safe.character, names(x))
    names(x) <- gsub("\\$", safe.character, names(x))
    names(x) <- gsub("\\%", safe.character, names(x))
    names(x) <- gsub("\\^", safe.character, names(x))
    names(x) <- gsub("\\&", safe.character, names(x))
    names(x) <- gsub("\\*", safe.character, names(x))
    names(x) <- gsub("\\|", safe.character, names(x))
    names(x) <- gsub("\\/", safe.character, names(x))
    # names(x) <- gsub("\\<", safe.character, names(x))
    names(x) <- gsub("\\>", safe.character, names(x))
    names(x) <- gsub("\\-", safe.character, names(x))
    names(x) <- gsub("\\+", safe.character, names(x))
    names(x) <- gsub("\\=", safe.character, names(x))

    names(x) <- gsub(paste0(safe.character,safe.character), safe.character, names(x))
    names(x) <- gsub(paste0("^",safe.character), "", names(x))
    names(x) <- gsub(paste0(safe.character,"$"), "", names(x))
  }

  if (!is.null(truncate.names)) {
    if (class(truncate.names)[1] != "numeric") {
      stop("Argument `truncate.names` must be a whole number. (See the help entry: `?write.alignment`)\n")
    } else {
      names(x) <- strtrim(names(x), truncate.names[1])
    }
  }

  name.padding <- max(nchar(names(x))) +1

  if (sequence.line.length == 0) {
    n.rows <- 1
  } else {
    n.rows <- ceiling(nchar(x)[1] / sequence.line.length)
  }

  names(x) <- str_pad(names(x), name.padding, side='right', pad=' ')

  if (!is.null(filename)) { sink(filename) }

  # Header
  cat(length(x), nchar(x)[1],"\n")

  # Backup - delete later
  # x0 <- x

  if (n.rows == 1) {
    for (j in 1: length(x)) {
      cat(names(x)[j],x[j],"\n")
    }
  } else {
    for (i in 1:n.rows) {
      for (j in 1: length(x)) {
        s <- str_sub(x[j], 1, sequence.line.length)
        cat(names(x)[j],s,"\n")
        x[j] <- str_sub(x[j], sequence.line.length+1, nchar(x[j]))
      }
      cat("\n")
    }
  }

  if (!is.null(filename)) { sink() }

} # End of function
