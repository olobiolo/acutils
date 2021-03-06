% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun-ferader.R
\name{freader}
\alias{freader}
\title{read a ScanR data table}
\usage{
freader(path, pattern = "_")
}
\arguments{
\item{path}{path to file}

\item{pattern}{regular expression to select variables to keep}
}
\value{
a data frame
}
\description{
Use \code{data.table::fread} to (quickly) load a parameter data file
generated by ScanR and run the usual operations that make it usable.
}
\details{
This is a simple wrapper for fread from the package data.table.
It also uses some dplyr functions to modify the resulting data.table.
Some columns will be discarded. Usually the variables that are to be kept
will contain a specific pattern, e.g. an underscore (\code{_}).

The \code{data.table} class is stripped from the object.
}
