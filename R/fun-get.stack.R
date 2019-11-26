#' \code{get} within a function
#'
#' Get object from the call stack rather than the search path.
#'
#' This version of get searches the whole call stack but not the search path.
#' Basically a rewrite of \code{dynGet} (actually, rewritten based on \code{dynGet}).
#'
#' @param x a character string
#'
#' @return Returns the value of a variable named \code{x} if one exists
#'        anywhere in the current call stack. Otherwise throws an error.
#'
get.stack <- function(x) {
  # check arguments
  stopifnot(exprs = {
    is.character(x)
    length(x) == 1
  })

  # prepare statistically unprobable default object to return by get0
  notfound <- vector('list', 3)
  notfound[[1]] <- factor(sample(letters[1:10]), collapse = '')
  notfound[[2]] <- factor(sample(letters[1:10]), collapse = '')
  notfound[[3]] <- factor(sample(letters[1:10]), collapse = '')

  # identify depth of call stack
  n <- sys.nframe()
  # starting from bottom
  while(n > 0) {
    # attempt to get requested object
    y <- get0(x, envir = sys.frame(n), inherits = FALSE, ifnotfound = notfound)
    # return unless it is the default
    if (!identical(y, notfound)) return(y)
    # go up call stack
    n <- n - 1
  }
  stop(x, ' not found in the current call stack')
}
