dmsg <- function(debug, ...)
    if (debug > 0)
        cat(..., sep="")

#' Save an oce-class object as a netcdf file.
#'
#' `oce2ncdf()` works by determining the class of its first argument,
#' and then dispatching to an internal function, as appropriate.
#'
#' @param x an oce-class object.
#'
#' @param varTable character value indicating the variable-naming
#' scheme to be used.  Two forms are handled.  (1) If `varTable`
#' ends in `".yml"`, then `varTable` must be the name of a YAML
#' file describing the variables.  The format must be as in
#' the built-in file accessed with
#' `system.file("extdata","argo.yml",package="ocencdf"))`
#' On the other hand, (2) if `varTable` equals `"argo"`, then
#' it is replaced with a reference to the above-indicated
#' file.  In both cases, the contents are scanned with
#' [yaml::yaml.load_file()].  If `varTable` is not supplied,
#' then a guess will be made by the function that [oce2ncdf()]
#' calls to do the work, e.g. for a `ctd` object, the default
#' is `"argo"`.
#'
#' @param ncfile character value naming the output file.
#'
#' @param debug integer, 0 for quiet action, 1 or more to see processing information.
#'
#' @importFrom yaml yaml.load_file
#' @importFrom ncdf4 nc_create nc_close ncdim_def nc_open ncvar_def ncvar_put
#'
#' @examples
#'\dontrun{
#' library(ocencdf)
#' library(oce)
#' data(ctd, package="oce")
#' oce2ncdf(ctd, ncfile="ctd.nc")
#'}
#'
#' @author Dan Kelley
#'
#' @export
oce2ncdf <- function(x, varTable, ncfile="output.nc", debug=0)
{
    if (!inherits(x, "oce"))
        stop("'x' must be an oce object")
    xclass <- as.character(class(x))
    switch(xclass,
        ctd=ctd2ncdf(x, varTable=if (missing(varTable)) "argo" else varTable, ncfile=ncfile, debug=debug),
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
