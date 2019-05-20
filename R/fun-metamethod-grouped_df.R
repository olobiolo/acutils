#' create S3 method for class \code{grouped_df}
#'
#' Given an S3 method for class \code{data.frame}, create an S3 method
#' for class \code{grouped_df} for the same generic function.
#'
#' This is a utility function for writing packages.
#' Functions that modify data frames by replacing or adding columns may fail
#' on grouped data frames so the class will require a dedicated method.
#' \code{metamethod} is a generalized constructor of such methods.
#'
#' In all cases the original call to \code{foo} is captured with \code{match.call}
#' and reconstructed into a call to \code{by}. The first argument of \code{foo},
#' which must be named \code{x}, is passed to the \code{data} argument of the new call.
#' Grouping variables are isolated as a list and passed to the \code{INDICES} argument.
#' The \code{FUN} argument is set to \code{foo.data.frame},
#' i.e. the \code{x} of \bold{this} function.
#' The remaining arguments are passed as is.
#'
#' @section How to use:
#' When editing an .R file for generic \code{foo} and its S3 methods,
#' simply add another entry where \code{foo.grouped_df} is defined as the result of
#' \code{metamethod} acting on \code{foo.data.frame}.
#'
#' If preceeded with the \code{roxygen} tag
#' "\code{@describeIn foo see \code{\link{acutils::metamethod}}}",
#' it will show up nicely in the documentation file.
#'
#' @param fun a function; this must be an S3 method for class \code{data.frame};
#'            also, its first argument must be named \code{x}
#'
#' @return a function: a method for class \code{grouped_df} (from package \code{dplyr})
#'
##' @export
#'
metamethod <- function(fun) {
  FORMALS <- formals(fun)
  if (names(FORMALS)[1] != 'x') stop('the first argument of the function must be named "x"')
  BODY <- quote(
    {
      # capture call
      original_call <- as.list(match.call())
      # drop function and first argument to isolate additional arguments
      original_arguments <- original_call[-(1:2)]
      # create name of data frame method to call
      new_method <- sub('grouped_df', 'data.frame', as.character(original_call[[1]]))
      # save attributes of x
      xats <- attributes(x)
      # obtain a list of factors from grouping variables
      f_list <- as.list(x[dplyr::group_vars(x)])
      # convert x to normal data frame
      X <- data.frame(x)
      # construct call to by
      new_call <-
        as.call(
          append(list(quote(by), data = quote(X), INDICES = f_list, FUN = get(new_method)),
                 original_arguments))
      # run operation
      y <- eval(new_call)
      # wrap resulting by object to data frame
      Y <- do.call(rbind, y)
      # update column names within the attribute list
      xats$names <- names(Y)
      # restore attributes
      attributes(Y) <- xats
      return(Y)
    })

  FUN <- as.function(append(FORMALS, BODY))
  return(FUN)
}

