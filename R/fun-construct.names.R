#' translate RSz construct names
#'
#' Convert construct numbers in the RSz series to their contents (inserts).
#'
#' The function utilizes a dictionary table to modify the \code{cells} column
#' in \code{data} accordingly. This must be supplied as a data frame or
#' a tab-delimited text file.
#' Factors are converted to character.
#'
#' @param data a \code{data.frame}; must contain a column called \code{cells}
#' @param dictionary either a dictionary \code{data.frame} or path to file containing one
#'
#' @return a modified data frame
#'
#' @importFrom magrittr %>%
#'
#' @export

construct.names <- function(data, dictionary = '../constructs.txt') {
  if (!'cells' %in% names(data)) stop('column "cells" missing from data')
  if ('293' %in% data$cells | 'HeLa' %in% data$cells) message('empty cell lines detected')
  if (is.character(dictionary)) dic <- utils::read.delim(dictionary) else dic <- dictionary
  dic$cells <- as.character(dic$cells)
  data$cells <- as.character(data$cells)
  CDa <- data$cells %>% unique %>% setdiff(., NA)
  if (!all(is.element(CDa, dic$cells))) {
    warning('some cells were not found in dictionary')
    print(setdiff(CDa, dic$cells))
  }
  data <- dplyr::left_join(data, dic, by = 'cells') %>%
    dplyr::mutate(insert = as.character(insert),
                  cells = as.character(cells),
                  insert = ifelse(is.na(insert), cells, insert)) %>%
    dplyr::select(-cells) %>% dplyr::rename(cells = insert)
  return(data)
}
