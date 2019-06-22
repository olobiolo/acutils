#' scale numeric vector
#'
#' Scales a vector to a requested range.
#'
#' @param x a numeric vector or matrix
#' @param range range which to scale to:
#'              \code{positive}: [0,1], \code{negative}: [-1,0], \code{both}: [-1,1]
#'
#' @return a numeric object of the same class as \code{x}
#'
#' @export
#'
rescale <- function(x, range) {
  UseMethod('rescale')
}

#' @export
#' @describeIn rescale defult method for vectors
rescale.default <- function(x, range) {
  if (!is.numeric(x)) stop('"x" must be numeric')
  #range <- match.arg('range', choices = c('positive', 'negative', 'both'), several.ok = FALSE)

  X <- (x - min(x)) / (max(x - min(x)))
  Y <- switch(range,
              'positive' = X,
              'negative' = X - 1,
              'both' = 2 * (X - 0.5))
  return(Y)
}

#' @export
#' @describeIn rescale calls default method and copies the
#'             \code{dim} attribute of \code{x} to result
rescale.matrix <- function(x, range) {
  X <- rescale.default(x, range = range)
  dim(X) <- dim(x)
  return(X)
}
