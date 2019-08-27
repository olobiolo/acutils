#' acutils: some utilities
#'
#' Various functions I use more or less often.
#' Some are specifically designed to deal with ScanR data.
#'
#' @section functions:
#' \itemize{
#'   \item{ScanR related:
#'     \itemize{
#'       \item{\code{clean_column_names}
#'       removes superfluous information from column names in result files
#'       }
#'       \item{\code{fetch_files}
#'       gathers all result files in one location
#'       }
#'       \item{\code{freader}
#'       loads and preprocesses parameter data
#'       }
#'     }
#'   }
#'   \item{general purpose:
#'     \itemize{
#'       \item{\code{baseline}
#'       modifies one or more variables in a data frame
#'       by subtracting the mean value of a reference set
#'       }
#'       \item{\code{cilp_data}
#'       removes extreme elements of a vector or data frame
#'       }
#'       \item{\code{center_titles}
#'       adjusts ggplot theme so that plot titles are centered
#'       }
#'       \item{\code{conf}
#'       calculates confidence interval of the mean
#'       }
#'       \item{\code{construct.names}
#'       converts RSz construct names into their respective inserts
#'       for purposes of labeling figures
#'       }
#'       \item{\code{empty.plate}
#'       creates a scaffold for a plate annotation file
#'       }
#'       \item{\code{insert_zeros}
#'       inserts zeros into strings
#'       }
#'       \item{\code{interlace}
#'       "mixes" several vectors into one
#'       }
#'       \item{\code{multiplot}
#'       prints multiple ggplot plots on a single page
#'       }
#'       \item{\code{pmatrix}
#'       displays a plate layout in matrix form
#'       }
#'       \item{\code{print_plots}
#'       prints several ggplot plots to a single pdf file
#'       }
#'       \item{\code{rename_reference_observations}
#'       changes the category labels of reference observations
#'       so they can be plotted as e.g. "time = 0h" rather than "nt"
#'       }
#'       \item{\code{rescale}
#'       scales a numeric vector
#'       }
#'       \item{\code{rotate_labels}
#'       rotates axis text in a ggplot by 90 degrees
#'       }
#'       \item{\code{sem}
#'       calculates standard error of the mean
#'       }
#'       \item{\code{show_colors}
#'       displays available shades of a given color
#'       }
#'       \item{\code{sphere}
#'       calculates volume of a hyperspphere
#'       }
#'       \item{\code{zscore}
#'       calculates z scores
#'       }
# #'       \item{\code{}}
#'     }
#'   }
#' }

#' @docType package
#' @name acutils
#' @author Aleksander Chlebowski
#'
NULL
