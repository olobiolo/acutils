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

  res <- vector('list', length = longest)
  for (i in 1:longest) {
    res[[i]] <- sapply(vectors, function(x) x[i])
  }
  # alternative method
  #res <- lapply(1:longest, function(x) sapply(vectors, function(y) y[x]))
  ans <- do.call(c, res)

  # to preserve original NAs
  if (useNAa && anyNA(unlist(vectors))) {
    NAs_vectors <- lapply(vectors, is.na)
    NAs_interlaced <- interlace(NA_vectors)
    NAs_now <- is.na(NAs_interlaced)
    ans <- ans[!NAs_now]
  }

  return(ans)
}


# interlace2 <- function(...) {
#   vectors <- list(...)
#   longest <- max(vapply(vectors, length, numeric(1)))
#
#   res <- lapply(1:longest, function(x) sapply(vectors, function(y) y[x]))
#   ans <- do.call(c, res)
#
#   # to preserve original NAs
#   if (useNAs && anyNA(unlist(vectors))) {
#     NAs_vectors <- lapply(vectors, is.na)
#     NAs_interlaced <- interlace(NA_vectors)
#     NAs_now <- is.na(NAs_interlaced)
#     ans <- ans[!NAs_now]
#   }
#
#   return(ans)
# }
#
# a <- rep(TRUE, 5)
# b <- rep(FALSE, 5)
# microbenchmark::microbenchmark(loop = interlace(a,b), lapply = interlace2(a,b))