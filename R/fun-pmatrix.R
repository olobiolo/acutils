#' quick peek at a plate layout
#'
#' @section Feature display:
#' Each feature of the plate is represented as a matrix corresponding to the plate
#' dimensions. The format is determined based on the well number.
#'
#' \code{pmatrix} accepts a single feature (vector) or a whole layout object
#' (a data frame) or a list.
#'
#' The well number is rigidly determined based on the dimensions of x
#' then a wrapper for matrix() is created and applied to all columns
#'
#' @param x layout object: data frame, list or atomic vector
#' @param rows,cols backup for supplying non-standard matrix dimensions
#'
#' @export
#'
#' @describeIn plates

pmatrix <- function(x, rows, cols) {
  #' quick peek at a layout matrix
  # determine well number
  wells <- if (is.data.frame(x)) {
    nrow(x)
  } else if (is.list(x)) {
    lengths <- unique(sapply(x, length))
    if (length(lengths) != 1) stop('elements of x are of unequal elngth')
    lengths
  } else if (is.vector(x)) {
    length(x)
  }
  # create wrapper for matrix
  our_matrix <- switch(as.character(wells),
                       '384' = function(x) matrix(x, nrow = 16, ncol = 24, byrow = TRUE),
                       '96' = function(x) matrix(x, nrow = 8, ncol = 12, byrow = TRUE),
                       '48' = function(x) matrix(x, nrow = 6, ncol = 8, byrow = TRUE),
                       '24' = function(x) matrix(x, nrow = 4, ncol = 6, byrow = TRUE),
                       '12' = function(x) matrix(x, nrow = 3, ncol = 4, byrow = TRUE),
                       '6' = function(x) matrix(x, nrow = 2, ncol = 3, byrow = TRUE),
                       '336' = function(x) matrix(x, nrow = 14, ncol = 24, byrow = TRUE),
                       '288' = function(x) matrix(x, nrow = 12, ncol = 24, byrow = TRUE)
  )
  # in case of unexpected matrix dimensions they can be supplied manually
  if (is.null(our_matrix)) {
    if (missing(rows) & missing(cols)) stop('matrix grid unknown; use "rows" and "cols" arguments')
    if (missing(rows) || missing(cols)) stop('one matrix dimension missing')
    if (rows * cols != wells) stop('grid dimensions do not match well number')
    our_matrix <- function(x) matrix(x, nrow = rows, ncol = cols, byrow = TRUE)
  } else {
    message('ignoring "rows" and "cols"')
  }

  if (is.list(x)) lapply(x, our_matrix) else our_matrix(x)
}

#' @examples
#' a <- empty.plate(24)
#' a$siRNA <- NA
#' a$siRNA <- ifelse(a$column == 1, 'nt', a$siRNA)
#' a$siRNA <- ifelse(a$column == 2, 'neg', a$siRNA)
#' a$siRNA <- ifelse(a$column == 3, 'pos', a$siRNA)
#' a$siRNA <- ifelse(a$column == 4, 'test1', a$siRNA)
#' a$siRNA <- ifelse(a$column == 5, 'test2', a$siRNA)
#' a$siRNA <- ifelse(a$column == 6, 'test3', a$siRNA)
#' a$siRNA <- ifelse(a$column == 6, 'test4', a$siRNA)
#' pmatrix(a)
#'
