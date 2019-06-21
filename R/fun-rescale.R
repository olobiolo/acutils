#' scale numeric vector
#'
#' Scales a vector. Positive values are scaled from 0 to 1, negative ones are scaled -1 to 0.
#'
#' @param x a numeric vector or matrix
#'
#' @return a numeric object of the same class as \code{x}
#'
#' @export
#'
rescale <- function(x) {
  UseMethod('rescale')
}

#' @export
#' @describeIn rescale defult method for vectors
rescale.default <- function(x) {
  if (!is.numeric(x)) stop('"x" must be numeric')

  s.pos <- function(x)
    suppressWarnings((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
  s.neg <- function(x)
    suppressWarnings((x - max(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
  pos <- x[x > 0]
  neg <- x[x < 0]
  zer <- x[x == 0]
  pos.s <- s.pos(pos)
  neg.s <- s.neg(neg)

  X <- numeric(length(x))
  X[x > 0] <- pos.s
  X[x < 0] <- neg.s
  X[x == 0] <- 0
  return(X)
}

#' @export
#' @describeIn rescale calls default method and copies the
#'             \code{dim} attribute of \code{x} to result
rescale.matrix <- function(x) {
  X <- rescale.default(x)
  dim(X) <- dim(x)
  return(X)
}
