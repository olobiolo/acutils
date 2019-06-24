#' scale numeric vector
#'
#' Scales a vector to a requested range.
#'
#' Values in the vector are scaled such to fill \code{range}:
#' \itemize{
#'   \item{\code{positive}: from 0 to 1
#'   }
#'   \item{\code{negative}: from -1 to 0
#'   }
#'   \item{\code{both}: from -1 to 1
#'   }
#' }
#'
#' @param x a numeric vector or matrix
#' @param range range which to scale to
#'
#' @return a numeric object of the same class as \code{x}
#'
#' @export
#'
rescale <- function(x, range = c('positive', 'negative', 'both')) {
  UseMethod('rescale')
}

#' @export
#' @describeIn rescale defult method for vectors
rescale.default <- function(x, range = c('positive', 'negative', 'both')) {
  if (!is.numeric(x)) stop('"x" must be numeric')
  range <- match.arg(range, choices = c('positive', 'negative', 'both'), several.ok = FALSE)

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
rescale.matrix <- function(x, range = c('positive', 'negative', 'both')) {
  X <- rescale.default(x, range = range)
  dim(X) <- dim(x)
  return(X)
}
