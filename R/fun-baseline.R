#' subtract baseline from signal
#'
#' Calculate baseline signal and subtract it from all observations.
#'
#' @param x a \code{data.frame} or \code{grouped data frame}
#' @param variables character vector of variables (columns) to modify
#' @param reference logical predicate that identifies reference observations, given as string or bare;
#'                  will be passed to \code{\link{subset}}
#' @param method method with which to compute the baseline, mean or median; given as symbol or string
#' @param by_group character vector of variables to group by (\code{\link{dplyr::group_by}})
#'                 in case the baseline is to be taken from means of sets of observations
#'                 rather than from all observations directly
#'
#' @return a modified \code{data.frame}
#'
#' @section Grouped data frames (\code{dplyr} package):
#' The method for class \code{grouped_df} is home brewed as I don't know
#' how to properly handle this class.
#'
#' @export
#'

baseline <- function(x, variables, reference, method = mean, by_group) {
  UseMethod('baseline')
}

#' @export
#' @describeIn baseline
baseline.data.frame <- function(x, variables, reference, method = mean, by_group) {
  #' computes mean/median of desired variables with \code{vapply}
  #' and subtracts them from their respective variables using \code{mapply}

  # check arguments
  if (!is.data.frame(x)) stop('x must be a data frame')
  if (missing(variables)) {
    message('no variables selected; taking all numeric variables except "well" and "column"')
    variables <- setdiff(names(Filter(is.numeric, x)), c('well', 'column'))
  } else {
    if (!is.character(variables)) stop('varaibles must be a character vector')
    if (!all(variables %in% names(x))) stop('invalid variables selected')
    if (!all(vapply(x[variables], is.numeric, logical(1)))) stop('non-numeric variables selected')
  }
  if (!missing(by_group) && !all(by_group %in% names(x))) stop('invalid grouping selected: "by_group"')

  # capture logical predicate for subset and, if string, convert to expression
  r <- substitute(reference)
  r <- if (is.call(r)) r else if (is.character(r)) substitute(eval(parse(text = r)))
  # get method
  method <- match.fun(method)
  # capture column order
  column_order <- names(x)

  # isolate reference set
  x_ref <-
    if (missing(reference)) x else
      subset(x, subset = eval(r))
  # separate variables to be normalized from remaining ones
  x_vars <- x[variables]
  x_rems <- x[setdiff(names(x), variables)]
  if (!all(sapply(x_vars, is.numeric))) stop('some "variables" are not numeric')
  # average reference subgroups if grouping variables are identified
  if (!missing(by_group)) {
    x_ref <-
      do.call(rbind,
              by(data = x_ref,
                 INDICES = x_ref[by_group],
                 FUN = function(x) {for(i in variables) x[[i]] <- method(x[[i]]); return(x)},
                 simplify = FALSE))
  }
  # calculate baselines
  baselines. <- vapply(x_ref[variables], method, na.rm = T, numeric(1))
  # subtract baselines
  x_normalized <- mapply(FUN = '-', x_vars, baselines.)
  # add normalized columns to the unchanged ones
  y <- cbind(x_rems, x_normalized)
  y <- y[, column_order]
  return(y)
}

#' @export
#' @describeIn baseline constructed by \code{\link{acutils::metamethod}}
baseline.grouped_df <- function(x, variables, reference, method = mean, by_group) {
  #' saves attributes and grouping variables of \code{x}, captures original call
  #' and modifies it to be run by \code{by}, then restores attributes

  # capture call and drop function and data, keeping only additinoal arguments
  original_arguments <- as.list(match.call())[-(1:2)]
  # save attributes of x
  gats <- attributes(x)
  # obtain a list of factors from grouping variables
  f_list <- as.list(x[dplyr::group_vars(x)])
  # strip grouping
  x <- data.frame(x)
  # construct new call
  new_call <-
    as.call(
      append(
        list(as.name('by'), data = quote(x), INDICES = quote(f_list), FUN = quote(baseline.data.frame)),
        original_arguments)
    )
  # run the call
  y <- eval(new_call)
  # convert to data frame
  Y <- do.call(rbind, y)
  # re-group
  attributes(Y) <- gats
  return(Y)
}

#baseline.grouped_df <- metamethod(baseline.data.frame)

# x1 <- data.frame(
#   well = 1:100,
#   int1 = c(rnorm(10, 10, 1), rnorm(10, 150, 2), rnorm(80, 78, 4)),
#   class = c(rep('low', 10), rep('high', 10), rep('mid', 80)),
#   plate = 'plate 1'
#   )
# x2 <- x1 %>% dplyr::mutate(int1 = int1 * 1.25, plate = 'plate 2')
# x <- rbind(x1, x2)
# y <- x %>% dplyr::group_by(plate) %>% baseline('int1', class == 'low') %>% data.frame
# rbind(x, y) %>% dplyr::mutate(set = rep(c('x', 'y'), each = 200)) %>%
#   ggplot2::ggplot(ggplot2::aes(x = class, y = int1, color = set)) +
#   ggplot2::facet_wrap(~plate) +ggplot2::geom_point(position = ggplot2::position_jitterdodge())
