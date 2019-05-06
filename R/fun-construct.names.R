#' translate construct names
#'
#' Convert construct numbers to their contents (inserts).
#'
#' When planning an experiment it is common to use names of constructs rather than
#' the often lengthy descriptions of their inserts. This also helps when creating layout files, etc.
#' However, the insert names have to be introduced eventually so that figures are readable
#' and that may take a lot of typing.
#'
#' The function utilizes a dictionary table to modify the "cells" column
#' in \code{data} accordingly. By default this is taken from a file in the package's
#' \code{data} directory, which can be replaced or modified at will as long as it
#' is a tab delimited .txt file with two columns: "cells" and "insert".
#'
#' Alternatively a custom file or a data frame object can be provided. The same restrictions apply.
#'
#' Empty cell lines can be (and indeed are) placed in the dictionary.
#' Their "insert" field is the same as their "cells" filed.
#'
#' @param data a \code{data.frame}; must contain a column called \code{cells}
#' @param dictionary optional dictionary file or object, see \code{Details}
#'
#' @return a modified data frame
#'
#' @export
#'
#' @examples
#' a <- data.frame(cells = c('HeLa', '448', NA, '500'))
#' construct.names(a)

construct.names <- function(data, dictionary) {
  if (is.factor(data$cells)) data$cells <- as.character(data$cells)
  if (missing(dictionary)) dictionary <- paste(path.package('acutils'), 'data', 'constructs.txt', sep = '/')
  if (is.character(dictionary)) dictionary <- utils::read.delim(dictionary, stringsAsFactors = FALSE)
  if (!is.data.frame(dictionary)) stop('"key" must be a data frame or a path to a file containing one')
  if (any(sapply(dictionary, is.factor))) {
    key <- cbind(
      Filter(Negate(is.factor), dictionary),
      as.data.frame(lapply(Filter(Negate(is.factor), dictionary), factor))
    )
  }

  if (!is.element('cells', names(data))) stop('"cells" column missing from "data"')
  if (!all(is.element(c('cells', 'insert'), names(dictionary)))) stop('wrong format of "dictionary", check columns')

  empties <- mapply(identical, dictionary$cells, dictionary$insert)
  empties <- names(empties[empties])
  empties.present <- empties %in% data$cells

  if (any(empties.present))
    message('empty cell line(s) detected: ', paste(empties[empties.present], collapse = ','))

  cells.here <- setdiff(unique(data$cells), NA)
  cells.missing <- setdiff(cells.here, dictionary$cells)
  if (length(cells.missing > 0))
    warning('some cell lines missing in dictionary: ', paste(cells.missing, collapse = ','))

  res <- merge(data, dictionary, all.x = TRUE, all.y = FALSE, sort = FALSE)
  res$insert <- ifelse(is.na(res$insert), res$cells, res$insert)
  res <- res[, -which(names(res) == 'cells'), drop = FALSE]
  names(res)[which(names(res) == 'insert')] <- 'cells'
  return(res)
}
