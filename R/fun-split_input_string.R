#' extract substrings from text input
#'
#' Isolates elements from a string.
#'
#'
#'
#' @param x a character string
#' @param sep character string containing allowed separators
#'
#' @return A character vector of isolated substrings.
#'
#' @export

split_input_string <- function(x, sep = ';:,&+=/ ') {
  if (!is.character(x) | length(x) != 1) stop('x must be a character string of length 1')

  # preclude illegal separating characters
  illegal <- c('.', '-', '_', '\\')
  if (any(grepl(sep, illegal))) stop('illegal separating characters')

  # remove leading and trailing spaces
  y1 <- gsub('^ *| *$', '', x)
  y2 <- gsub(paste0('[', sep, ']'), '*', y1)
  y3 <- strsplit(y2, split = '\\*+')[[1]]
  y4 <- grep(sep, y3, value = TRUE, invert = TRUE)

  return(y4)
}

#' @examples
#' input <- ' one,two three, four / five+seven=eight'
#' split_input_string(input)

