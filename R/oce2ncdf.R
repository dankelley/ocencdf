#' Save an oce-class object as a NetCDF file.
#'
#' `oce2ncdf()` works by determining the class of its first argument,
#' and then dispatching to an internal function, as appropriate.
#'
#' @param x an oce-class object. The subclass of this object determines
#' which lower-level function [oce2ncdf()] calls, e.g. if `x` is of
#' `ctd` class, then [ctd2ncdf()] is called.
#'
#' @template varTableTemplate
#'
#' @param ncfile character value naming the output file.  Use NULL
#' for a file name to be created automatically (e.g. `ctd.nc` for
#' a CTD object).
#'
#' @param force_v4 logical value which controls the NetCDF file version during
#' the \link[ncdf4]{nc_create} step. The default here is TRUE, whereas the
#' \link[ncdf4]{ncdf4-package} defaults to FALSE (ensuring that the NetCDF
#' file is compatible with NetCDF v3). Some features, including large data
#' sizes, may require v4.
#'
#' @param debug integer, 0 (the default) for quiet action apart
#' from messages and warnings, or any larger value to see more
#' output that describes the processing steps.
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom ncdf4 ncatt_put nc_create nc_close ncdim_def nc_open ncvar_def ncvar_put
#' @importFrom oce numberAsPOSIXct T90fromT48 T90fromT68
#' @importFrom utils str
## @importFrom yaml yaml.load_file
#'
#' @author Dan Kelley and Clark Richards
#'
#' @export
oce2ncdf <- function(x, varTable=NULL, ncfile=NULL, force_v4=TRUE, debug=0)
{
    if (!inherits(x, "oce"))
        stop("'x' must be an oce object")
    xclass <- as.character(class(x))
    switch(xclass,
        ctd=ctd2ncdf(x, varTable=varTable, ncfile=ncfile, force_v4=force_v4, debug=debug),
        adp=adp2ncdf(x, varTable=varTable, ncfile=ncfile, force_v4=force_v4, debug=debug),
        adv=adv2ncdf(x, varTable=varTable, ncfile=ncfile, force_v4=force_v4, debug=debug),
        stop("oce2ncdf() cannot handle \"", xclass, "\" objects")
    )
}

