#' print some plots to a pdf file
#'
#' Gather multipel existing ggplot objects and print them to one file
#' (one plot per page).
#'
#' This is a simple utility function. It finds all objects in the global
#' environment whose names match a given regular expression and tries to send
#' them to the pdf device. In an analysis where multiple plots are produced,
#' this can help to export them into neat files, with some care for naming them.
#'
#' NULL objects will be ignored
#'
#' @param file file to print to
#' @param pattern regular expression to locate plot objects by
#' @param ... arguments passed to other functions,
#'            specifically \code{width} and \code{height} to \code{pdf}
#'
#' @export


print_plots <- function(file = 'some_plots.pdf', pattern = 'plot\\.[0-9]+', ...) {
  # find objects mathing the pattern
  plot_names <- ls(pattern = pattern, envir = parent.frame())
  if (length(plot_names) == 0) {
    stop('no plots found')
  } else message('plots found:\n', paste(plot_names, collapse = '\n'))
  # get all the plots into a list
  plot_list <- mget(plot_names, envir = parent.frame())
  # open graphics device, print (explicitly) and close
  grDevices::pdf(file = file, ...)
  lapply(plot_list, print)
  grDevices::dev.off()
  message('plots have been printed')
}

