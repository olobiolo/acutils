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
#' @param x character or integer vector;
#'          double vectors are handled only if they can be converted to integer and pass as \code{all.equal}
#' @param zeros number of \code{0} characters to insert; see \code{Details}
#' @param after position of the character after which the zeros will be inserted
#' @param ... arguments passed to methods
#'
#' @return Modified character vector, where strings of zeros have been inserted at the specified position.
#'
#' @export
#'

insert_zeros <- function(x, ...) {
  UseMethod('insert_zeros')
}

#' @export
#' @describeIn insert_zeros the basic method
insert_zeros.character <- function(x, zeros = 'auto', after = 1, ...) {
  force(zeros)
  force(after)
  if (zeros < 1) {
    message('waste of time...')
    return(x)
  }
  if (after < 0) stop('"after" must not be lower than 0')
  if (after > min(nchar(x), na.rm = TRUE)) stop('some items are too short; "after" too large')

  nchar.max <- max(nchar(x), na.rm = TRUE)

  # create function that returns a string of zeros
  zero_string <- function(xx, zeros) {
    if (is.numeric(zeros)) {
      n <- zeros
    } else if (zeros == 'auto') {
      if (length(x) == 1L) n <- 1 else n <- nchar.max - nchar(xx)
    }
    return(paste(rep('0', n), collapse = ''))
  }
  # create function that inserts the string of zeros to the target string
  if (after == 0) {
    paster <- function(x) {
      if (is.na(x)) return(x) else
        paste0(paste(zero_string(x, zeros), collapse = ''), x)
    }
  } else {
    paster <- function(x) {
      if (is.na(x)) return(x) else
        paste0(substr(x, 1, after), paste(zero_string(x, zeros), collapse = ''), substr(x, after + 1, nchar(x)))
    }
  }

  # apply over X
  vapply(x, paster, USE.NAMES = FALSE, FUN.VALUE = character(1))
}

#' @export
#' @describeIn insert_zeros runs on levels of x rather than its body
insert_zeros.factor <- function(x, zeros = 'auto', after = 1, ...) {
  warning('"x" is a factor, coercing to character')
  x <- as.character(x)
  args <- as.list(match.call())[-1]
  args[[1]] <- x
  do.call(insert_zeros.character, args)
}

#' @export
#' @describeIn insert_zeros coerces to character and calls character method
insert_zeros.integer <- function(x, zeros = 'auto', after = 1, ...) {
  message('"x" is numeric, coercing to character')
  x <- as.character(x)
  args <- as.list(match.call())[-1]
  args[[1]] <- x
  do.call(insert_zeros.character, args)
}

#' @export
#' @describeIn insert_zeros attempts to convert to integer and calls integer method
insert_zeros.double <- function(x, zeros = 'auto', after = 1, ...) {
  X <- x
  x <- as.integer(x)
  if (isTRUE(all.equal(X, x))) {
    args <- as.list(match.call())[-1]
    args[[1]] <- x
    do.call(insert_zeros.integer, args)
    } else stop('doubles are valid arguments only if they are equal to integers')

}

#' @export
#' @describeIn insert_zeros throws error for non-supperted classes
insert_zeros.default <- function(x, zeros = 'auto', after = 1, ...) {
  stop('insert_zeros doesn\'t know how to handle class ', class(x), call. = FALSE)
}

#' @examples
#' insert_zeros("A1")
#'
#' v <- paste0(LETTERS[1:10], 1:10)
#' rbind("original" = v,
#'       "auto" = insert_zeros(v),
#'       "force 1 zero" = insert_zeros(v, 1),
#'       "force after 2nd char" = insert_zeros(v, after = 2),
#'       "force at beginning" = insert_zeros(v, after = 0))
#'
