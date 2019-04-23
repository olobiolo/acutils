#' interleave two vectors
#'
#' Given two vectors, x and y, outputs a vector x1, y1, x2, y2, etc.
#' Currently only supports two vectors.
#'
#' @param x,y vectors
#'
#' @export
#'
#' TODO: write separate methods for numeric, character and factor

interleave <- function(x, y) {
  if (is.integer(x)) class(x) <- 'double'
  if (is.integer(y)) class(y) <- 'double'
  stopifnot(typeof(x) == typeof(y))

  if (length(x) != length(y)) {
    if (length(x) < length(y)) {
      shorter.vector <- x
      longer.vector <- y
    } else {
      shorter.vector <- y
      longer.vector <- x
    }
  }
  length.longer <- length(longer.vector)
  length.shorter <- length(shorter.vector)

  xy <- vector('list', length = length.longer)
  i = 0
  while (i < length.shorter) {
    i = i + 1
    xy[[i]] <- c(x[i], y[i])
  }
  while (i < length.longer) {
    i = i + 1
    xy[[i]] <- longer.vector[i]
  }
  do.call('c', xy)
}
