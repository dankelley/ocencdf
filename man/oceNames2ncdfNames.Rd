% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vartable.R
\name{oceNames2ncdfNames}
\alias{oceNames2ncdfNames}
\title{Translate NetCDF names to oce names}
\usage{
oceNames2ncdfNames(names, varTable = NULL, debug = 0)
}
\arguments{
\item{names}{vector of character values in oce convention (e.g. "temperature"
for temperature).}

\item{varTable}{character value indicating the variable-naming
scheme to be used, which is passed to \code{\link[=read.varTable]{read.varTable()}} to set
up variable names, units, etc.}

\item{debug}{integer, 0 (the default) for quiet action apart
from messages and warnings, or any larger value to see more
output that describes the processing steps.}
}
\description{
Translate NetCDF names to oce names
}
\author{
Dan Kelley
}
