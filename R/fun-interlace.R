#' interlace vectors
#'
#' Given a number vectors, e.g. x, y and z, outputs a vector x1, y1, z1, x2, y2, z2, etc.
#' If the vectors are of different types, they are coerced as usual.
#'
#' @param ... any number vectors
#' @param useNAs logical flag whether preexisting NAs should be preserved
#'
#' @return a single interlaced vector
#'
#' @examples
#' x1 <- rep('a', 3)
#' x2 <- rep('b', 3)
#' x3 <- rep('c', 3)
#' interlace(x1, x2, x3)
#'
#' x4 <- rep('d', 5)
#' interlace(x1, x4)
#'
#' @export
#'

interlace <- function(..., useNAs = TRUE) {
  vectors <- list(...)
  longest <- max(vapply(vectors, length, numeric(1)))

  res <- lapply(1:longest, function(x) sapply(vectors, function(y) y[x]))
  ans <- do.call(c, res)
#browser()
  # remove newly added NAs but preserve preexisting ones
  if (anyNA(unlist(vectors)) & useNAs) {
    NAs_vectors <- lapply(vectors, is.na)
    NAs_interlaced <- interlace(NAs_vectors, useNAs = FALSE)
    NAs_now <- is.na(NAs_interlaced)
    ans <- ans[!NAs_now]
  } else {
    ans <- ans[!is.na(ans)]
  }

  return(ans)
}
