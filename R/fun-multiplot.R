#' Multiple plot function
#'
#' Print multiple ggplot plots on a single page. This is NOT splitting a single plot.
#' Unlike with facetting, the plots can be completely different.
#'
#' @param ... plot objects
#' @param plotlist list of plot objects
#' @param file path to file to print the plots into (?)
#' @param cols number of columns to arrange the plots into
#' @param layout matrix specifying the layout as explained in \code{\link{graphics::layout}};
#'               if present, \code{cols} is ignored
#'
#' @section Layout:
#' Layout specification by matrix works exactly like with graphics::layout.
#' If the layout is something like matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @section Reference:
#' \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}{R Graphics Cookbook}
#' by Winston Chang
#'
#' @import grid
#' @export
#'

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  # make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # if layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # make the panel
    # ncol: number of columns of plots
    # nrow: number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols),byrow = T)
  }

  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    # set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
