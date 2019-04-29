#' calculate (robust) zscore
#'
#' Given a numeric vector, convert the numbers to z scores or robust z scores.
#' The function accepts raw values as well as normalized ones.
#'
#' The z score is a data point standardized to the distribution it comes from.
#' It is a measure of the deviation of the point from the distribution's
#' location parameter expressed in terms of the dispersion parameter of that distribution.
#'
#' For a normal z score the difference between a point and the distribution mean
#' is divided by the distribution's standard deviation. For a robust z score
#' the mean and standard deviation are replaced by the median and median absolute deviation,
#' respectively.
#'
#' @param x a numeric vector
#' @param robust logical flag whether to calculate normal or robust z scores
#' @param deviations logical flag whether the supplied data is raw or normalized
#' @param reference logical vector that points out reference observations
#'
#' @return a numeric vector of z scores
#'
#' @section Deviations:
#' Data is accepted as raw or normalized and the function can be informed of this with a logical flag.
#' For deviations the location parameter is assumed to be 0.
#' If normalization is done by simply subtracting the mean/median of the whole sample
#' from each data point, this is redundant but other normalization methods
#' may introduce differences differences.

#' @section Reference:
#' If desired, data points in the sample can be standardized against part of the distribution
#' rather than the whole. Should this be the case, supply a logical vector to determine
#' the reference subpopulation.
#'
#' @examples
#' a <- rnorm(1000, 55, 3)
#' hist(a, breaks = 50)
#' z <- zscore(a)
#' hist(z, breaks = 50)
#' plot(x = a, y = z)
#'
#' @export

zscore <- function(x, robust = TRUE, deviations = FALSE, reference = NULL) {
  if (!is.numeric(x)) stop('"x" must be numeric')

  ref <-
    if (is.null(reference)) {
      x
    } else {
      x[reference]
    }
  loc <-
    if (deviations) {
      0
    } else if (robust) {
      stats::median(ref, na.rm = TRUE)
    } else {
      mean(ref, na.rm = TRUE)
    }
  disp <-
    if (robust) {
      stats::mad(ref, na.rm = TRUE)
    } else {
      stats::sd(ref, na.rm = TRUE)
    }

  zsc <- (x - loc) / disp
  return(zsc)
}
