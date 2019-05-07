#' @name errors
#'
#' @title error functions
#'
#' @description
#' Functions to express the uncertainty of a sample.
#'
#' @details
#' These are helper functions for drawing error bars on plots.
#' They both calculate a measure of uncertainty and return a single number,
#' which is the width of the "uncertainty interval".
#' Add and subtract this unmber to the sample mean to get
#' the position of the error bars.
#'
#' @return a single numeric
#'
#' @section Disclaimer:
#' The confidence interval is calculated with a t distribution.
#' I don't understand how the confidence interval is calculated.
#'
NULL

#' @param x a numeric vector
#' @param na.rm logical flag whether to remove NA values
#'
#' @export
#' @describeIn errors
#'

sem <- function(x, na.rm = TRUE) {
  #' standard error of the mean, i.e.
  #' the standard deviation of the sample divided by
  #' the square root of the number of elements
  if (!is.numeric(x)) stop("\"x\" must be numeric")
  if (na.rm) x <- x[!is.na(x)]
  stats::sd(x) / sqrt(length(x))
}

#' @inheritParams sem
#' @param confidence the desired confidence
#'
#' @export
#' @describeIn errors
#'

conf <- function(x, na.rm = TRUE, confidence = 0.99) {
  #' half of the width of the confidence interval of the mean for a numeric vector
  if (!is.numeric(x)) stop("\"x\" must be numeric")
  if (na.rm) x <- x[!is.na(x)]
  to_quantile <- 1 - ((1 - confidence) / 2)
  stats::qt(to_quantile, df = length(x) - 1) * stats::sd(x) / sqrt(length(x))
}
