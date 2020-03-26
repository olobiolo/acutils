#' Pipe operator
#'
#' Forward piping operator.
#'
#' The value of the left hand side expression is passed to the right hand side expression.
#' It is matched to the first argument by default. This can be controlled by using the
#' dummy argument \code{.}, which stands in for the LHS value.
#'
#' See package \code{\link[magrittr]{magrittr}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
