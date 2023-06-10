#' Save an adp object to a netcdf file
#'
#' Given an `adp` object created by the `oce` package, this function
#' creates a netcdf file that can later by read by [ncdf2adp()] to approximately
#' reproduce the original contents.
#'
#' Note that [oce2ncdf()] defaults `varTable` to `"-"`,
#' meaning that no variable-name translation is done.
#'
#' @param x an oce object of class `adp`, as created by e.g. [oce::read.adp()].
#'
#' @inheritParams oce2ncdf
#'
#' @examples
#' library(ocencdf)
#'
#' # Example with an adp file from oce package
#' data(adp, package="oce")
#' summary(adp)
#' plot(adp)
#' # Transfer to netcdf and back to see if results make sense
#' oce2ncdf(adp, ncfile="adp.nc")
#' ADP <- ncdf2adp("adp.nc")
#' summary(ADP)
#' plot(ADP)
#'
#' # Remove temporary file
#' file.remove("adp.nc")
#'
#' @family things related to adp data
#'
#' @author Dan Kelley
#'
#' @export
adp2ncdf <- function(x, varTable=NULL, ncfile=NULL, debug=0)
{
    dmsg(debug, "adp2ncdf(..., ncfile=\"", ncfile, "\") {\n")
    if (!inherits(x, "adp"))
        stop("'x' must be a adp object")
    if (is.null(varTable)) {
        varTable <- "-"
        message("Defaulting varTable to \"", varTable, "\".")
    }
    if (is.null(ncfile)) {
        ncfile <- "adp.nc"
        message("Will save adp object to \"", ncfile, "\".")
    }
    varTableOrig <- varTable
    varTable <- read.varTable(varTable)
    vdim <- dim(x@data$v)
    extant <- list()
    for (item in c("v", "a", "g", "q"))
        extant[[item]] <- item %in% names(x@data)
    if (!extant$v)
        stop("there is no data item named 'v', which is mandatory for an oce adp object")
    #time <- ncdim_def(name="TIME", units="s", vals=as.numeric(x@data$time))
    time <- ncdim_def(name="TIME", units="", vals=seq_len(vdim[1]), create_dimvar=FALSE,
        longname="seconds since 1970-01-01 UTC")
    #cell <- ncdim_def(name="DISTANCE", units="m", vals=x@data$distance, longname="Distance to cell")
    distance <- ncdim_def(name="DISTANCE", units="", vals=seq_len(vdim[2]), create_dimvar=FALSE,
        longname="Distance to cell")
    beam <- ncdim_def(name="BEAM", units="", vals=seq_len(vdim[3]))
    vars <- list()
    # time and distance (do they show up as n$var now?)
    dmsg(debug, "    time (length ", vdim[1], ")\n")
    vars[["time"]] <- ncvar_def(name="time", units="seconds since 1970-01-01 UTC", dim=time, missval=1.0e30, prec="float")
    dmsg(debug, "    distance (length ", vdim[2], ")\n")
    vars[["distance"]] <- ncvar_def(name="distance", units="m", dim=distance, missval=1.0e30)
    dmsg(debug, "  Setting up variable dimensions for ", paste(vdim, collapse="x"), " arrays:\n")
    # array data
    dmsg(debug, "    v\n")
    vars[["v"]] <- ncvar_def(name="v", units="m/s", dim=list(time, distance, beam), missval=1.0e30)
    if (extant$a) {
        dmsg(debug, "    a\n")
        vars[["a"]] <- ncvar_def("a", "", list(time, distance, beam), 1.0e30)
    }
    if (extant$g) {
        dmsg(debug, "    g\n")
        vars[["g"]] <- ncvar_def("g", "", list(time, distance, beam), 1.0e30)
    }
    if (extant$q) {
        dmsg(debug, "    q\n")
        vars[["q"]] <- ncvar_def("q", "", list(time, distance, beam), 1.0e30)
    }
    # time-series data
    dmsg(debug, "  Setting up dimensions for time-series vectors of length ", vdim[1], ":\n")
    #cat("NEXT: names in @data:\n");print(sort(names(x@data)))
    for (item in names(x@data)) {
        if (item != "time" && item != "distance" && is.vector(x@data[[item]])) {
            dmsg(debug, "    ", item, "\n")
            vars[[item]] <- ncvar_def(item, "", time, 1.0e30)
        }
    }
    nc <- nc_create(ncfile, vars)
    dmsg(debug, "  Storing time and distance\n")
    dmsg(debug, "    time\n")
    ncvar_put(nc, "time", as.numeric(x@data$time))
    #message("first 3 times: ", paste(x@data$time[1:3], collapse=" "))
    #message("first 3 times: ", paste(as.numeric(x@data$time)[1:3], collapse=" "))
    dmsg(debug, "    distance\n")
    ncvar_put(nc, "distance", as.numeric(x@data[["distance"]]))
    dmsg(debug, "  Storing arrays:\n")
    for (item in c("v", "a", "g", "q")) {
        if (extant[[item]]) {
            dmsg(debug, "    ", item, "\n")
            ncvar_put(nc, item, as.numeric(x@data[[item]]))
        }
    }
    dmsg(debug, "  Storing time-series vectors:\n")
    for (item in names(x@data)) {
        if (item != "distance" && item != "time" && is.vector(x@data[[item]])) {
            dmsg(debug, "    ", item, "\n")
            vars[[item]] <- ncvar_put(nc, item, as.numeric(x@data[[item]]))
        }
    }
    dmsg(debug, "  Storing global attributes:\n")
    dmsg(debug, "    varTable\n")
    ncatt_put(nc=nc, varid=0, attname="varTable", attval=varTableOrig)
    dmsg(debug, "    class\n")
    ncatt_put(nc=nc, varid=0, attname="class", attval=as.character(class(x)))
    dmsg(debug, "    metadata\n")
    ncatt_put(nc, 0, "metadata", paste(deparse(x@metadata), collapse="\n"))
    nc_close(nc)
    dmsg(debug, paste0("} # adp2ncdf created file \"", ncfile, "\"\n"))
}

#' Read a netcdf file and create an adp object
#'
#' This works by calling [ncdf2oce()] and then using [class()] on
#' the result to make it be of subclass `"adp"`.
#'
#' @inheritParams ncdf2oce
#'
#' @return [ncdf2adp()] returns an [adp-class] object.
#'
#' @family things related to adp data
#'
#' @examples
#' library(ocencdf)
#'
#' # Example with an adp file from oce package
#' data(adp, package="oce")
#' summary(adp)
#' plot(adp)
#' # Transfer to netcdf and back to see if results make sense
#' oce2ncdf(adp, ncfile="adp.nc")
#' ADP <- ncdf2adp("adp.nc")
#' summary(ADP)
#' plot(ADP)
#'
#' # Remove temporary file
#' file.remove("adp.nc")
#'
#' @author Dan Kelley
#'
#' @export
ncdf2adp <- function(ncfile=NULL, varTable=NULL, debug=0)
{
    adp <- ncdf2oce(ncfile=ncfile, varTable=varTable, debug=debug)
    class(adp) <- "adp"
    adp
}