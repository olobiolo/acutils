#' compute sphere volume
#'
#' Calculate volume of n-dimentional sphere of radius r.
#'
#' @param r radius
#' @param n number of dimensions
#'
#' @return volume of a supersphere
#'
#' @export
#'

sphere <- function(r, n = 7L) {
  if (n %% 1 != 0 ) stop("no fractions allowed in \"n\"")
  (pi^(n/2) * r^n) / gamma(1 + n/2)
}
