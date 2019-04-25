#' Display Shades of a Color
#'
#' Quick peek at available variations of a color in R.
#'
#' The function uses the \code{color} argument as a regular expression
#' with which to search \code{colors()} and displays all matching colors
#' on a rudimentary barplot.
#'
#' @param color a string
#'
#' @export

show_colors <- function(color) {
  par(mar = c(5,11,4,2) + 0.1)
  on.exit(par(mar = c(5,4,4,2) + 0.1))
  if (!is.character(color)) stop('"color" must be a regular expression')
  color_group <- grep(color, colors(), value = T)
  L <- length(color_group)
  if (L == 0) stop('no colors match the given name')

  args <- list(height = rep(1, L), col = color_group, names.arg = color_group, las = 1,
               xaxt = 'n', horiz = T, border = NA,
               xlab = '', ylab = '', main = paste('available shades of', color))
  do.call(graphics::barplot, args)
}
