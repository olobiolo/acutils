#' remove extreme values from a vector
#'
#' Remove the smallest and greatest values from a vector of from a column of a data frame.
#'
#' The \code{clip} arguments determines the number of items/observations removed:
#' \itemize{
#' \item{if clip >= 1, this number of observations will be removed}
#' \item{if clip < 1, this fraction of observations will be removed}
#' \item{if clip < 1 and of length 1, half the fraction will be removed
#'       from the bottom and half from the top of the range}
#' \item{if clip >= 1 and of length 1, that number of items will be removed
#'       from both bottom and top of the range}
#' \item{if clip is of length 2, the first value determines clipping
#'       at the bottom of the range and the second - at the top}
#' }
#'
#' After clipping a t test is run to determine whether the clipping significantly
#' changed the mean of the vector. In case of a data frame this is also done for
#' every other numeric variable. The result of the test is reported as a warning
#' if positive (p < 0.05) and as a message if negative.
#'
#' @param x a vector or a data frame
#' @param clip \strong{either} number(s) or fraction(s) (rounding up)
#'             of values to be removed, see \code{Details}
#' @param preserve.order logical flag whether result should be returned
#'                        sorted or in the original order (default)
#'
#' @return A clipped vector or data frame.
#'
#' @author Aleksander Chlebowski, June 2018
#'
#' @export
#'

clip_data <- function(x, clip = 1L, preserve.order = TRUE) {
  # check arguments
  if (all(clip == 0)) {
    message('odd choice of clipping, returning unchanged')
    return(x)
  }
  if ((all(clip < 1) && (sum(clip) >= 1)) || (all(clip > 1) && sum(clip) >= length(x))) {
    stop('clipping specification would leave no data')
  }
  if ((length(clip) == 2 & sum(clip < 1) == 1) & !any(clip == 0)) {
    stop('determine clipping either as fractions or as numbers')
  }
  if (any(clip > 1 & clip %% 1 != 0)) {
    stop('if "clip" is larger than 1, it must be an integer')
  }

  UseMethod("clip_data")
}

#' @export
#' @describeIn clip_data
clip_data.default <- function(x, clip = 1L, preserve.order = TRUE) {
  #' default method

  # store original names
  was_named <- !is.null(names(x))
  if (was_named) original_names <- names(x)

  # create names for x that will be used for sorting y after clipping
  if (preserve.order) {
    names(x) <- insert_zeros(paste0('p', 1:length(x)))
  }

  # sort data
  X <- sort(x)

  # parse clipping conditions
  Clip <-
    if (all(clip < 1))
      if (length(clip) == 1) ceiling(length(X) * clip / 2) else
        c(ceiling(length(X) * clip[1]), ceiling(length(X) * clip[2])) else
          clip

  # do the deed
  Y <- X[(Clip[1] + 1) : (length(X) - Clip[length(Clip)])]

  # report number of values removed, if fractions were requested
  if (all(clip < 1)) {
    values_removed <- length(X) - length(Y)
    message(values_removed, ' values removed')
  }

  # restore original order
  if (preserve.order){
    Y <- Y[order(sort(names(X)))]
    Y <- Y[!is.na(Y)]
  }

  if (was_named)
    y <- setNames(Y, original_names[which(x %in% Y)]) else
      y <- setNames(Y, NULL)

  return(y)
}

#' @export
#' @describeIn clip_data
clip_data.factor <- function(x, clip = 1L, preserve.order = TRUE) {
  #' coerces \code{x} to characer, calls the character method, and restores the original levels
  lvls <- levels(x)
  xc <- as.character(x)
  yc <- clip_data.default(xc, clip = clip, preserve.order = preserve.order)
  y <- factor(yc, levels = lvls)

  return(y)
}

#' @export
#' @describeIn clip_data
clip_data.numeric <- function(x, clip = 1L, preserve.order = TRUE) {
  #' adds a location check
  y <- clip_data.default(x, clip = clip, preserve.order = preserve.order)
  # check and report how the clipping affected the mean of the data
  change <- tryCatch(
    t.test(x,y, alternative = 'two.sided', paired = FALSE),
    error = function(e) {
      message('from t.test: data are essentially unchanged')
      return(list(p.value = 1))
    }
  )
  pval <- change$p.value
  if (pval < 0.05)
    warning('clipping significantly changed the mean value (p >= 0.05 in t test)', call. = FALSE) else
      if (pval != 1) message('clipping did not significantly change the mean value (p >= 0.05 in t test)')

  return(y)
}

#' @export
#' @describeIn clip_data
clip_data.data.frame <- function(x, clip = 1L, column) {
  #' clips the specified column and test all numeric columns for changes
  # check argument
  if (!all(is.character(column), length(column) == 1)) stop('"column" must be a character of length 1')
  if (!column %in% names(x)) stop('requested column not found')
  message('clipping by column of class ', paste(class(x[[column]]), collapse = ' '))

  # take note of all the numeric columns in the data frame
  numeric_columns <- names(x)[sapply(x, is.numeric)]

  # hold on to the column in question (as vector)
  thecolumn <- x[[column]]

  # clip the specified column on the side, suppressing all communication
  suppressWarnings(
    suppressMessages(
      thecolumn_clipped <- clip_data(thecolumn, clip = clip, preserve.order = TRUE)
      )
    )

  # create index of remaining values and use it to drop rows from the data frame
  ind <- thecolumn %in% thecolumn_clipped
  X <- x[ind, ]

  # report number of values removed, if fractions were requested
  if (all(clip < 1)) {
    values_removed <- nrow(x) - nrow(X)
    message(values_removed, ' observations removed')
  }

  # check and report how the clipping affected the mean of all numeric data frames
  xX <- cbind(rbind(x, X), clipped = c(rep('pre', nrow(x)), rep('post', nrow(X))))
  formula_strings <- lapply(numeric_columns, function(x) paste(x, '~', 'clipped'))
  formulas <- lapply(formula_strings, as.formula)
  models <- lapply(formulas, function(f) tryCatch(
    t.test(f, data = xX, alternative = 'two.sided', paired = FALSE),
    error = function(e) return(list(p.value = 1)) ) )
  pvals <- sapply(models, function(x) x$p.value)
  if (any(pvals < 0.05)) {
    significantly_changed <- numeric_columns[which(pvals < 0.05)]
    print_us_please <- paste(significantly_changed, collapse = ', ')
    warning('clipping significantly changed the mean value of some numeric variables (p < 0.05 in a t test): ', print_us_please, call. = FALSE)
  } else {
    message('clipping did not significantly change the mean value (p >= 0.05 in t test)')
  }

  return(X)
}

# make vignette
