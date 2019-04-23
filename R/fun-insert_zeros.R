#' insert zeros into a character vector
#'
#' Insert a string of zeros into every item of a character vector.
#' \code{A1} becomes \code{A01}.
#' The orginal intent was to fix sorting of strings where \code{2 > 12} but \code{02 < 12}.
#' More robust options were built in just in case.
#'
#' The \code{zeros} argument can be a fixed number but it defaults to \code{auto}. Then:
#' if \code{X} is of length 1, adds 1 \code{0};
#' if \code{X} is longer than 1, adds to each element a number of \code{0}
#' so that all elements of \code{X} have an equal number of characters.

#'
#' @param X character vector
#' @param zeros number of \code{0} characters to insert; see \code{Details}
#' @param after position of the character after which the zeros will be inserted
#'
#' @return Modified character vector, where strings of zeros have been inserted at the specified position.
#'
#' @examples
#' insert_zeros("A1")
#'
#' v <- paste0(LETTERS[1:10], 1:10)
#' rbind("original" = v,
#'       "auto" = insert_zeros(v),
#'       "force 1 zero" = insert_zeros(v, 1),
#'       "force after 2nd char" = insert_zeros(v, after = 2))
#'
#' @export

insert_zeros <- function(X, zeros = 'auto', after = 1) {
  # check arguments
  if (!is.character(X)) stop('"X" is not a character vector')
  if (zeros < 1) {
    print('waste of time...')
    return(X)
  }
  if (after < 1) stop('"after" is named "after for a reason')
  if (after > min(nchar(X))) stop('some items are too short; "after" too large')

  # create function that returns a string of zeros
  zero_string <- function(x, zeros) {
    if (is.numeric(zeros)) {
      n <- zeros
    } else if (zeros == 'auto') {
      if (length(X) == 1L) n <- 1 else n <- max(nchar(X)) - nchar(x)
    }
    return(paste(rep('0', n), collapse = ''))
  }
  # create function that inserts the string of zeros to the target string
  paster <- function(x) {
    paste0(substr(x, 1, after), paste(zero_string(x, zeros), collapse = ''), substr(x, after + 1, nchar(x)))
  }
  # apply over X
  vapply(X, paster, USE.NAMES = FALSE, FUN.VALUE = character(1))
}


