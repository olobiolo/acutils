#' @section Scaffold:
#' The scaffold is a data frame with three columns: well, row, and column.
#' Rows and columns are numbered as the given format requires, e.g.
#' a 384-well plate has rows A through P and columns 1 through 24.
#'
#' @section Formats:
#' This function is only so smart.
#' It has some hard coded formats that it will recognize, otherwise it returns a NULL
#' \itemize{
#' \item{* full plates of 6, 12, 24, 48, 96 and 384 wells,}
#' \item{* partial 384 plates:}
#' \itemize{
#' \item{336 wells: only rows B through O}
#' \item{228 wells: only rows C through N}
#' }
#' }
#'
#' @param wells number of wells in layout
#'
#' @return a data frame or a NULL if the format is not supported
#'
#' @export
#' @describeIn plates
#'

empty.plate <- function(wells = 384) {
  #' create a layout scaffold
  p <- switch(as.character(wells),
              # full plates
              '384' = data.frame(well = 1:384, row = rep(LETTERS[1:16], each = 24), column = rep(1:24, 16)),
              '96' = data.frame(well = 1:96, row = rep(LETTERS[1:8], each = 12), column = rep(1:12, 8)),
              '48' = data.frame(well = 1:48, row = rep(LETTERS[1:6], each = 8), column = rep(1:8, 6)),
              '24' = data.frame(well = 1:24, row = rep(LETTERS[1:4], each = 6), column = rep(1:6, 4)),
              '12' = data.frame(well = 1:12, row = rep(LETTERS[1:3], each = 4), column = rep(1:4, 3)),
              '6' = data.frame(well = 1:6, row = rep(LETTERS[1:2], each = 3), column = rep(1:3, 2)),
              # partial plates
              '336' = data.frame(well = 1:384, row = rep(LETTERS[1:16], each = 24), column = rep(1:24, 16)),
              '228' = data.frame(well = 1:384, row = rep(LETTERS[1:16], each = 24), column = rep(1:24, 16))
  )
  if (wells == 336) p <- subset(p, !row %in% c('A', 'P'))
  if (wells == 228) p <- subset(p, !row %in% c('A', 'B', 'O', 'P'))
  if (is.null(p)) message('unsupported plate format')
  return(p)
}
