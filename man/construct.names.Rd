% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun-construct.names.R
\name{construct.names}
\alias{construct.names}
\title{translate RSz construct names}
\usage{
construct.names(data, dictionary = "../constructs.txt")
}
\arguments{
\item{data}{a \code{data.frame}; must contain a column called \code{cells}}

\item{dictionary}{either a dictionary \code{data.frame} or path to file containing one}
}
\value{
a modified data frame
}
\description{
Convert construct numbers in the RSz series to their contents (inserts).
}
\details{
The function utilizes a dictionary table to modify the \code{cells} column
in \code{data} accordingly. This must be supplied as a data frame or
a tab-delimited text file.
Factors are converted to character.
}