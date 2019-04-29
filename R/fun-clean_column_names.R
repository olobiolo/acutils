#' clean up superfluous characters in column names
#'
#' Remove gate and object names from column names. Some reformatting to make them easier to read.
#'
#' ScanR export files are tab delimited tables whose columns contain the name of the object,
#' the gate it belongs to, the parameter in question and the measure being exported, e.g. mean or error.
#' The names are cumbersome and when loaded with \code{check.names = TRUE}, they can be quite confusing.
#' Usually most of these can be removed without any percievable loss.
#'
#' This is a simple stream of gsub calls that removes superfluous information from the column names.
#' It also improves the formatting. \code{\link{make.names}} is run first.
#'
#' @param x a \code{data frame}
#' @param gate gate name to be cleared, given as a regular expression;
#'             defaults to "dummy", which does nothing, unless there is a gate called "dummy"
#' @param object name of object to be cleared; defaults to "Main"
#'
#' @return data frame with modified column names
#'
#' @importFrom magrittr %>%
#'
# #' @examples
# #' d <- example_result_file
# #' head(d)
# #' dd <- clean_column_names(d)
# #' head(dd)
#'
#' @export

clean_column_names <- function(x, gate = 'dummy', object = 'Main') {
  if (!is.data.frame(x)) stop('"x" must be a data frame')

  names(x) <-
    names(x) %>%
    make.names() %>%
    gsub('....in.', '_in_', ., fixed = TRUE) %>%
    gsub(paste0(gate, '_in_', gate), gate, ., fixed = TRUE) %>%
    gsub(paste0(gate, '\\.'), '', .) %>%
    gsub(paste0(object, '\\.'), '', .) %>%
    gsub('\\.Mean', '', .) %>%
    gsub('\\.\\.Counts', '_count', .)
  return(x)
}
