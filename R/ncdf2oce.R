#' Read a netcdf file and create a general oce object
#'
#' Read a netcdf file, interpreting variable names according to
#' `varTable` (if provided).  The object does *not* get a specialized
#' oce class, because this is not known within netcdf files.  For
#' ctd data, try [ncdf2ctd()] instead of [ncdf2oce()], or wrap the
#' result of calling the latter in [oce::as.ctd()].
#'
#' @param ncfile character value naming the input file.
#'
#' @template varTableTemplate
#'
#' @template debugTemplate
#'
#' @importFrom ncdf4 ncatt_get ncvar_get nc_open
#' @importFrom methods new
#'
#' @return [ncdf2oce()] returns an [oce-class] object.
#'
#' @author Dan Kelley
#'
#' @export
ncdf2oce <- function(ncfile=NULL, varTable=NULL, debug=0)
{
    if (is.null(ncfile))
        stop("must supply ncfile")
    if (!is.character(ncfile))
        stop("ncfile must be a character value")
    if (!file.exists(ncfile))
        stop("ncfile \"", ncfile, "\" not found")
    dmsg(debug, "ncdf2oce() {\n")
    f <- nc_open(ncfile)
    res <- new("oce")
    names <- names(f$var)
    data <- list()
    for (name in names) {
        dmsg(debug, "  handling \"", name, "\"\n")
        if (grepl("^history_", name, ignore.case=TRUE))
            next
        units <- ncatt_get(f, name, "units")
        if (units$hasatt) {
            res@metadata$units[[name]] <- oce::as.unit(units$value)
            dmsg(debug, "    inferring units from \"", units$value, "\"\n")
        } else {
            dmsg(debug, "    (no units found for this variable)\n")
        }
        item <- ncvar_get(f, name)
        if (is.array(item) && 1 == length(dim(item))) # 1D array converted to 1 column matrix
            item <- as.vector(item)
        if (tolower(name) == "time") {
            if (units$hasatt && units$value == "seconds since 1970-01-01 UTC") {
                res@metadata[["time"]] <- numberAsPOSIXct(item[1])
            } else {
                warning("time unit is not understood, so it remains simply numeric")
            }
        } else if (tolower(name) == "station") {
            res@metadata[["station"]] <- trimws(item[1])
        } else {
            res@data[[name]] <- item
        }
    }
    # metadata
    tmp <- ncatt_get(f, 0, "metadata")
    if (tmp$hasatt) {
        res@metadata <- eval(parse(text=tmp$value))
        dmsg(debug, "  handling metadata\n")
    }
    # Update naming convention, if varTable was provided.
    if (!is.null(varTable)) {
        names(res@data) <- ncdfNames2oceNames(names=names(res@data), varTable=varTable, debug=debug)
        if ("units" %in% names(res@metadata))
            names(res@metadata$units) <- ncdfNames2oceNames(names=names(res@metadata$units), varTable=varTable, debug=debug)
        if ("flags" %in% names(res@metadata))
            names(res@metadata$flags) <- ncdfNames2oceNames(names=names(res@metadata$flags), varTable=varTable, debug=debug)
    }
    dmsg(debug, "} # ncdf2oce()\n")
    res
}

#' Read a netcdf file and create a ctd object
#'
#' @inheritParams ncdf2oce
#'
#' @return [ncdf2ctd()] returns an [ctd-class] object.
#'
#' @importFrom oce as.ctd
#'
#' @family things related to CTD data
#'
#' @author Dan Kelley
#'
#' @export
ncdf2ctd <- function(ncfile=NULL, varTable=NULL, debug=0)
{
    as.ctd(ncdf2oce(ncfile=ncfile, varTable=varTable, debug=debug))
}
