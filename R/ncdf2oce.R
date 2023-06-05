#' Read a netcdf file and create a general oce object
#'
#' This reads a netcdf file, interpreting units but not renaming
#' variables or placing quality-control information in the `metadata`
#' slot.  To get renaming and quality-control handling, call
#' another oce function as appropriate, e.g. [as.ctd()] is
#' used in the examples provided in the [ctd2ncdf()] documentation.
#'
#' @param ncfile character value naming the input file.
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
ncdf2oce <- function(ncfile=NULL, debug=0)
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
    # Try to get some global attributes.
    # Inelegantly permit first letter lower-case or upper-case
    if (ncatt_get(f, 0, "Longitude")$hasatt)
        res@metadata$longitude <- ncatt_get(f, 0, "Longitude")$value
    if (ncatt_get(f, 0, "longitude")$hasatt)
        res@metadata$longitude <- ncatt_get(f, 0, "longitude")$value
    if (ncatt_get(f, 0, "Latitude")$hasatt)
        res@metadata$latitude <- ncatt_get(f, 0, "Latitude")$value
    if (ncatt_get(f, 0, "latitude")$hasatt)
        res@metadata$latitude <- ncatt_get(f, 0, "latitude")$value
    if (ncatt_get(f, 0, "Station")$hasatt)
        res@metadata$station <- ncatt_get(f, 0, "Station")$value
    if (ncatt_get(f, 0, "station")$hasatt)
        res@metadata$station <- ncatt_get(f, 0, "station")$value
    if (ncatt_get(f, 0, "Ship")$hasatt)
        res@metadata$ship <- ncatt_get(f, 0, "Ship")$value
    if (ncatt_get(f, 0, "ship")$hasatt)
        res@metadata$ship <- ncatt_get(f, 0, "ship")$value
    if (ncatt_get(f, 0, "Cruise")$hasatt)
        res@metadata$cruise <- ncatt_get(f, 0, "Cruise")$value
    if (ncatt_get(f, 0, "cruise")$hasatt)
        res@metadata$cruise <- ncatt_get(f, 0, "cruise")$value
    if (ncatt_get(f, 0, "time")$hasatt)
        res@metadata$time <- ncatt_get(f, 0, "time")$value
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
#' @author Dan Kelley
#'
#' @export
ncdf2ctd <- function(ncfile=NULL, debug=0)
{
    as.ctd(ncdf2oce(ncfile=ncfile, debug=debug), debug=debug)
}
 
