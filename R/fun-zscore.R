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
#' @param reference optional determination of reference observations;
#'                  for the dafault method, a logical vector;
#'                  for the data frame method, a logical vector
#'                  or any predicate (bare or as string) that refers to \code{x}'s variables
#' @param variables for data frame method, character vector of variables to standardize
#' @param ... arguments passed to methods
#'
#' @return a numeric vector of z scores or a data frame with added columns of z scores
#'
#' @section Deviations:
#' Data is accepted as raw or normalized and the function can be informed of this with a logical flag.
#' For deviations the location parameter is assumed to be 0.
#' If normalization is done by simply subtracting the mean/median of the whole sample
#' from each data point, this is redundant but other normalization methods
#' may introduce differences differences.

#' @section Reference:
#' Data points in the sample can be standardized against part
#' of the distribution rather than the whole. Should this be the case,
#' supply a logical vector (or predicate that will be evaluated
#' within the data frame) to determine the reference subpopulation.
#'
#' @export

zscore <- function(x, ...) {
  UseMethod('zscore')
}

#' @export
#' @describeIn zscore for numeric vectors
zscore.default <- function(x, robust = TRUE, deviations = FALSE, reference, ...) {
  if (!is.numeric(x)) stop('"x" must be numeric')

  ref <-
    if (missing(reference)) {
      x
    } else if (any(reference)) {
      x[reference]
    } else x
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
      mean(ref, na.rm = TRUE)
    }

  zsc <- (x - loc) / disp
  return(zsc)
}

#' @export
#' @describeIn zscore calculates zscores for requested variables in a data frame
zscore.data.frame <- function(x, robust = TRUE, deviations = FALSE, reference, variables, ...) {

  # check arguments
  if (!is.data.frame(x)) stop('x must be a data frame')
  if (missing(variables)) {
    message('no variables selected; taking all numeric variables except "well" and "column"')
    variables <- setdiff(names(Filter(is.numeric, x)), c('well', 'column'))
  } else {
    if (!is.character(variables)) stop('varaibles must be a character vector')
    if (!all(variables %in% names(x))) stop('invalid variables selected')
    if (!all(vapply(x[variables], is.numeric, logical(1)))) stop('non-numeric variables selected')
  }
  # get reference as logical vector
  if (!missing(reference)) {
    r <- substitute(reference)
    r <- if (is.call(r) | is.name(r)) r else if (is.character(r)) substitute(eval(parse(text = r)))
    Reference <- eval(r, x)
  } else Reference <- logical(nrow(x))
  # get arguments from original call
  arguments <- list(robust = robust, deviations = deviations)
  # compute z scores
  y <- lapply(x[variables],
              function(x) zscore.default(x,
                                         robust = arguments$robust,
                                         deviations = arguments$deviation,
                                         reference = Reference))
  # name results
  names(y) <- paste0(variables, '_zscore')
  # add results to x
  Y <- cbind(x, as.data.frame(y))
  return(Y)
}

#' @export
#' @describeIn zscore see \code{\link[metamethods]{data.frame__to__grouped_df}}
zscore.grouped_df <- metamethods::data.frame__to__grouped_df(zscore.data.frame)

#' @examples
#' a <- rnorm(1000, 55, 3)
#' hist(a, breaks = 50)
#' z <- zscore(a)
#' hist(z, breaks = 50)
#' plot(x = a, y = z)
