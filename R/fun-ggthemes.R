#' modify ggplot theme
#'
#' Convenience functions to slightly modify ggplot objects.
#'
#' @name ggthemes
NULL

#' @export
#' @describeIn ggthemes
center_titles <- function() {
  #' called from global environment;
  #' justifies plot titles and subtitles to center
  ggplot2::theme_update(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
}
#' @export
#' @describeIn ggthemes
rotate_labels <- function() {
  #' added to ggplot object;
  #' rotates x axis text to vertical and adjusts position
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
}
