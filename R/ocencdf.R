#' Save an oce-class object as a netcdf file.
#'
#' `oce2ncdf()` works by determining the class of its first argument,
#' and then dispatching to an internal function, as appropriate.
#'
#' @param x an oce-class object. The subclass of this object determines
#' which lower-level function [oce2ncdf()] calls, e.g. if `x` is of
#' `ctd` class, then [ctd2ncdf()] is called.
#'
#' @param varTable character value indicating the variable-naming
#' scheme to be used. This is provided to [read.varTable()], the
#' documentation of which explains the possibilities for pre-defined
#' schemes named `"argo"` and `"whp"` and user-supplied files of
#' similar format.
#'
#' @param ncfile character value naming the output file.  Use NULL
#' for a file name to be created automatically (e.g. `ctd.nc` for
#' a CTD object).
#'
#' @param debug integer, 0 (the default) for quiet action apart
#' from messages and warnings, or any larger value to see more
#' output that describes the processing steps.
#'
#' @importFrom utils  capture.output str
#' @importFrom yaml yaml.load_file
#' @importFrom ncdf4 ncatt_put nc_create nc_close ncdim_def nc_open ncvar_def ncvar_put
#' @importFrom oce numberAsPOSIXct T90fromT48 T90fromT68
#'
#'
#' @author Dan Kelley
#'
#' @export
oce2ncdf <- function(x, varTable=NULL, ncfile=NULL, debug=0)
{
    if (!inherits(x, "oce"))
        stop("'x' must be an oce object")
    xclass <- as.character(class(x))
    switch(xclass,
        ctd=ctd2ncdf(x, varTable=varTable, ncfile=ncfile, debug=debug),
        stop("oce2ncdf() cannot handle \"", xclass, "\" objects")
    )
}

