#' rename reference observations
#'
#' Change category descriptions in a selected variable to enable prettier plotting.
#'
#' Experimental data are kept in a data frame and some observations
#' constitute a reference group. In order to be able to show them alongside
#' test observations, they may need to have their categorical variables altered
#' e.g. non-treated cells can be renamed to cells treated for 0 hours.
#'
#' The categorical variable can be stored as character or as a factor
#' and this will be preserved but the replaced level will be dropped.
#'
#' Grouping (by \code{dplyr}) will be preserved.
#'
#' @param X data frame
#' @param In_Variable name of the variable whose values are to be altered (character)
#' @param Replacee name of the value of \code{In_Variable} that is to be replaced (character)
#' @param Replacers character vector of values that will replace \code{Replacee};
#'                  defaults to all other values of \code{In_Variable}
#' @param return.composite logical flag whether to return just only reference rows
#'                         or the whole data set (with references renamed)
#'
#' @return
#' A data frame. If \code{return.composite} is \code{FALSE} only the (renamed)
#' observations will be returned. It \code{TRUE}, the returned data frame
#' contains the renamed reference observations as well as the original non-reference observations.
#'
#' @export

rename_reference_observations <-
  function(X, In_Variable = 'treatment', Replacee = 'nt', Replacers, return.composite = TRUE) {
    # check arguments
    if (!is.data.frame(X)) stop('"X" must be a data frame')
    if (!is.character(In_Variable) ||
        length(In_Variable != 1)) stop('"In_Variable" must ba a character vector of length 1')
    if (!is.character(Replacee)) stop('"Replacee" must ba a character vector of length 1')
    if (!Replacee %in% names(X)) stop('"Replacee" not present in column ', In_Variable)
    if (!missing(Replacers) && !is.character(Replacers)) stop('"Replacers" must be a character vector')

    # if X is grouped, the grouping must be stripped and grouping variables saved to be reapplied later
    is_it_grouped <- dplyr::is.grouped_df(X)
    if (is_it_grouped) {grouping <- dplyr::group_vars(X); X <- dplyr::ungroup(X)}

    # if Variable is a factor, save its levels and convert to character
    is_it_a_factor <- is.factor(X[[In_Variable]])
    # this version drop the replaced level
    if(is_it_a_factor) {
      levs <- setdiff(levels(X[[In_Variable]]), Replacee)
      X <- dplyr::mutate_at(X, In_Variable, as.character)
    }

    # see what values there are to replace
    if (missing(Replacers)) {
      Replacers <- X[[In_Variable]]
      Replacers <- if(is_it_a_factor) levs else unique(Replacers)
      Replacers <- setdiff(Replacers, Replacee)

    }
    # previous version
    # if (missing(Replacers)) Replacers <- X[[In_Variable]] %>%
    # {if(is_it_a_factor) levs else unique(.) %>% setdiff(., Replacee)}

    # define function that replaces one value with another
    rename_reference <- function(x, in_variable, replacee, replacer) {
      # replace one value by another in a specified (categorical) observation
      # subset data to isolate only the reference observations
      ref <- x[x[[in_variable]] == replacee, ]
      # replace treatment value
      ref[[in_variable]] <- replacer
      return(ref)
    }

    # run the above function with all elements of Replacers
    # returns set of reference observations
    refs <- lapply(Replacers, function(r) rename_reference(X, In_Variable, Replacee, r))
    refs <- do.call(rbind, refs)

    # return requested value
    if (return.composite) {
      # isolate non-reference observations
      non_refs <- X[X[[In_Variable]] != Replacee, ]
      # combine objects
      Y <- rbind(refs, non_refs)
      if (is_it_a_factor)
        Y <- dplyr::mutate_at(Y, In_Variable, function(x) factor(x, levels = levs))
      if (is_it_grouped)
        Y <- dplyr::group_by(Y, grouping)
      return(Y)
    } else {
      if (is_it_a_factor)
        refs <- dplyr::mutate_at(refs, In_Variable, function(x) factor(x, levels = levs))
      if (is_it_grouped)
        refs <- dplyr::group_by(refs, grouping)
      return(refs)
    }
  }
