# ocenc.R

#R #oce

library(oce)

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
#' @param \dots ignored in this version.
#'
#' @param outfile character value naming the output file.
#'
#' @param debug integer, 0 for quiet action, 1 or more to see processing information.
#'
#' @importFrom yaml yaml.load_file
#'
#' @examples
#'\dontrun{
#' library(ocencdf)
#' library(oce)
#' data(ctd, package="oce")
#' oce2ncdf(ctd)
#'}
#'
#' @author Dan Kelley
#'
#' @export
oce2ncdf <- function(x, varTable, ..., outfile="output.nc", debug=0)
{
    if (!inherits(x, "oce"))
        stop("'x' must be an oce object")
    xclass <- as.character(class(x))
    switch(xclass,
        ctd=ctd2ncdf(x, varTable=if (missing(varTable)) "argo" else varTable, ..., outfile=outfile, debug=debug),
        stop("ocencdf() cannot handle \"", xclass, "\" objects")
    )
}

ctd2ncdf <- function(x, varTable="argo", ..., outfile="ctd.nc", debug=0)
{
    dmsg(debug, "ctd2ncdf() {\n")
    if (!inherits(x, "ctd"))
        stop("'x' must be a ctd object")
    varTable <- readVarTable(varTable)
    for (name in names(x@data)) {
        if (name %in% names(varTable)) {
            dmsg(debug, "  FIXME: write \"", name, "\" as \"", varTable[[name]]$name, "\"\n")
        } else {
            dmsg(debug, "  FIXME: write \"", name, "\"\n")
        }
    }
    dmsg(debug, "} # ctd2ncdf\n")
}

readVarTable <- function(varTable="argo")
{
    if (varTable == "argo")
        varTable <- system.file("extdata", "argo.yml", package="ocencdf")
    if (!file.exists(varTable))
        stop("file \"", varTable, "\" does not exist")
    yaml::yaml.load_file(varTable)
}
