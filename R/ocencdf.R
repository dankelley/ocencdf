dmsg <- function(debug, ...)
    if (debug > 0)
        cat(..., sep="")

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
#' scheme to be used. If this ends in `.yml`, then the file of that
#' name is opened and processed with [yaml::yaml.load_file()]. Otherwise,
#' the only choice (at the moment) is `"argo"`, which causes the
#' function to work with
#' `system.file("extdata","argo.yml",package="ocencdf"))`.
#' If `varTable` is not supplied, then a guess will be made by the
#' lower-level function that [oce2ncdf()] calls to do the work.
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
        #ctd=ctd2ncdf(x, varTable=if (missing(varTable)) "argo" else varTable, ncfile=ncfile, debug=debug),
        ctd=ctd2ncdf(x, varTable=varTable, ncfile=ncfile, debug=debug),
        stop("oce2ncdf() cannot handle \"", xclass, "\" objects")
    )
}

readVarTable <- function(varTable="argo")
{
    if (varTable == "argo")
        varTable <- system.file("extdata", "argo.yml", package="ocencdf")
    if (!file.exists(varTable))
        stop("file \"", varTable, "\" does not exist")
    yaml::yaml.load_file(varTable)
}
