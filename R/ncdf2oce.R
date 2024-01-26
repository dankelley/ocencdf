# See e.g. ctd.R for ncdf2ctd().

#' Read a NetCDF file and create a general `oce` object
#'
#' Read a NetCDF file such as are created with e.g. [oce2ncdf()],
#' interpreting variable names according to `varTable` (if provided).
#' This is intended to work with NetCDF files created by
#' [oce2ncdf()], but it may also handle some other NetCDF files.
#' (Try [oce::read.netcdf()] if this fails.  If that also fails,
#' you will need to work with the `ncdf4` library directly.)
#' Note that the returned object does *not* get a specialized `oce` class,
#' because this is not known within NetCDF files.  For ctd data,
#' try [ncdf2ctd()] instead of [ncdf2oce()], or wrap the result
#' of calling the latter in [oce::as.ctd()].
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
            dmsg(debug, "    no units found for this variable\n")
        }
        item <- ncvar_get(f, name)
        if (is.array(item) && 1 == length(dim(item))) { # 1D array converted to 1 column matrix
            dmsg(debug, "    converted from 1-column matrix to vector\n")
            item <- as.vector(item)
        }
        if (name %in% c("time", "timeBurst", "timeSlow")) {
            if (units$hasatt && grepl("1970-01-01", units$value)) {
                dmsg(debug, "    interpreted as POSIXct(), since an attribute indicates a ref. time\n")
                res@data[[name]] <- numberAsPOSIXct(item)
            } else {
                dmsg(debug, "    interpreted as a general variable, even though the name indicates a time\n")
                res@data[[name]] <- item
            }
        } else if (tolower(name) == "station") {
            res@metadata[["station"]] <- trimws(item[1])
        } else {
            dmsg(debug, "    interpreted as a general variable\n")
            res@data[[name]] <- item
        }
    }
    # metadata
    tmp <- ncatt_get(f, 0, "metadata")
    if (tmp$hasatt) {
        #res@metadata <- eval(parse(text=tmp$value))
        res@metadata <- json2metadata(tmp$value)
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

