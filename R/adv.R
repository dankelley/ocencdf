#' Save an adv object to a netcdf file
#'
#' Given an `adv` object created by the `oce` package, this function
#' creates a netcdf file that can later by read by [ncdf2adv()] to approximately
#' reproduce the original contents.
#'
#' Note that [adv2ncdf()] defaults `varTable` to `"adv"`.
#'
#' The entire contents of the metadata slot are saved in the global attribute named
#' `"metadata"`, in a JSON format.  The JSON material is developed with
#' [metadata2json()], which yields a value that can be decoded with
#' [json2metadata()].
#'
#' @param x an oce object of class `adv`, as created by e.g. [oce::read.adv()].
#'
#' @inheritParams oce2ncdf
#'
#' @examples
#' library(ocencdf)
#'
#' # Example with an adv file from oce package
#' data(adv, package="oce")
#' summary(adv)
#' plot(adv)
#' # Transfer to netcdf and back to see if results make sense
#' oce2ncdf(adv, ncfile="adv.nc")
#' ADV <- ncdf2adv("adv.nc")
#' summary(ADV)
#' plot(ADV)
#'
#' # Remove temporary file
#' file.remove("adv.nc")
#'
#' @family things related to adv data
#'
#' @author Dan Kelley and Clark Richards
#'
#' @export
adv2ncdf <- function(x, varTable=NULL, ncfile=NULL, force_v4=TRUE, debug=0)
{
    dmsg(debug, "adv2ncdf(..., ncfile=\"", ncfile, "\") {\n")
    if (!inherits(x, "adv"))
        stop("'x' must be a adv object")
    if (is.null(varTable)) {
        varTable <- "adv"
        #message("Defaulting varTable to \"", varTable, "\".")
    }
    if (is.null(ncfile)) {
        ncfile <- "adv.nc"
        #message("Will save adv object to \"", ncfile, "\".")
    }
    varTableOrig <- varTable
    varTable <- read.varTable(varTable)
    dataNames <- names(x@data)
    if (!"v" %in% dataNames)
        stop("there is no data item named 'v', which is mandatory for an oce adv object")
    vdim <- dim(x@data$v)
    timeFastLen <- vdim[1]
    dmsg(debug, "  Defining overall variables:\n")
    dmsg(debug, "    timeFastLen: ", timeFastLen, "\n")
    slowIndices <- grep("Slow$", dataNames)
    anySlow <- length(slowIndices) > 0L
    timeSlowLen <- if (anySlow) length(x@data[[slowIndices[1]]]) else 0
    dmsg(debug, "    timeSlowLen: ", timeSlowLen, "\n")
    timeFast <- ncdim_def(name="TIME_FAST", units="", vals=seq_len(timeFastLen), create_dimvar=FALSE,
        longname="seconds since 1970-01-01 UTC")
    if (anySlow)
        timeSlow <- ncdim_def(name="TIME_SLOW", units="", vals=seq_len(timeSlowLen), create_dimvar=FALSE,
            longname="seconds since 1970-01-01 UTC")
    beam <- ncdim_def(name="BEAM", units="", vals=seq_len(vdim[2]))
    vars <- list()
    #FIXME: use this : FillValue <- getVarInfo("-", varTable=varTable)$FillValue
    # Set up space for each item
    dmsg(debug, "  Set up space for data items:\n")
    for (name in dataNames) {
        dmsg(debug, "    name: \"", name, "\"\n")
        item <- x@data[[name]]
        if (is.matrix(item)) {
            if (!identical(dim(item), vdim))
                stop("dimension of \"", name, "\" (",
                    paste(dim(item), collapse="x"), ") does not match dimension of \"v\" (",
                    paste(vdim, collapse="x"), ")")
            if (identical(name, "v")) {
                dmsg(debug, "      a matrix storing velocity, so given units m/s\n")
                vars[[name]] <- ncvar_def(name, units="m/s", dim=list(timeFast, beam))
            } else {
                dmsg(debug, "      a matrix of unknown units\n")
                vars[[name]] <- ncvar_def(name, units="", dim=list(timeFast, beam))
            }
        } else {
            isTime <- grepl("time", name, ignore.case=TRUE)
            if (length(item) == timeSlowLen) {
                if (isTime) {
                    dmsg(debug, "      slow time\n")
                    vars[[name]] <- ncvar_def(name, units="seconds since 1970-01-01 UTC", dim=list(timeSlow), prec="double")
                } else if (grepl("records", name)) {
                    vars[[name]] <- ncvar_def(name, units="", dim=list(timeSlow), prec="integer")
                    dmsg(debug, "      a record-count item at the slow time scale\n")
                } else {
                    dmsg(debug, "      an item at the slow time scale\n")
                    vars[[name]] <- ncvar_def(name, units="", dim=list(timeSlow))
                }
            } else if (length(item) == timeFastLen) {
                if (isTime) {
                    dmsg(debug, "      fast time\n")
                    vars[[name]] <- ncvar_def(name, units="seconds since 1970-01-01 UTC", dim=list(timeFast), prec="double")
                } else if (grepl("records", name)) {
                    vars[[name]] <- ncvar_def(name, units="", dim=list(timeFast), prec="integer")
                    dmsg(debug, "      a record-count item at the fast time scale\n")
                } else {
                    dmsg(debug, "      an item at the fast time scale\n")
                    vars[[name]] <- ncvar_def(name, units="", dim=list(timeFast))
                }
            } else {
                stop("item \"", name, "\" has length ", length(item), " but it should be either ", timeSlowLen,
                    " or ", timeFastLen)
            }
        }
    }
    nc <- nc_create(ncfile, vars, force_v4=force_v4)
    dmsg(debug, "  Storing data:\n")
    for (name in dataNames) {
        item <- x@data[[name]]
        dmsg(debug, "    ", name, "\n")
        if (is.matrix(item)) {
            #message("  matrix")
            ncvar_put(nc, name, as.numeric(item))
        } else {
            if (length(item) == timeSlowLen) {
                #message("  slow vector")
                ncvar_put(nc, name, item)
            } else if (length(item) == timeFastLen) {
                #message("  fast vector")
                ncvar_put(nc, name, item)
            } else {
                stop("item \"", name, "\" has length ", length(item), " but it should be either ", timeSlowLen,
                    " or ", timeFastLen)
            }
        }
    }
    dmsg(debug, "  Storing global attributes:\n")
    dmsg(debug, "    metadata_explanation\n")
    explanation <- paste("This file was created with adv2ncdf from the ocencdf R package,\n",
        "available at www.github.com/dankelley/ocencdf.\n\n",
        paste(readLines(system.file("extdata", "ncdf_explanation.md", package="ocencdf")), collapse="\n"),
        collapse="\n")
    ncatt_put(nc, 0, "metadata_explanation", explanation)
    dmsg(debug, "    metadata\n")
    ncatt_put(nc, 0, "metadata", metadata2json(x@metadata))
    # FIXME: perhaps store some individual metadata items, for simple access
    dmsg(debug, "    varTable\n")
    ncatt_put(nc=nc, varid=0, attname="varTable", attval=varTableOrig)
    dmsg(debug, "    class\n")
    ncatt_put(nc=nc, varid=0, attname="class", attval=as.character(class(x)))
    dmsg(debug, "    creator\n")
    ncatt_put(nc=nc, varid=0, attname="creator", attval=paste0("ocencdf version ", packageVersion("ocencdf")))
    nc_close(nc)
    dmsg(debug, paste0("} # adv2ncdf created file \"", ncfile, "\"\n"))
}

#' Read a netcdf file and create an adv object
#'
#' This works by calling [ncdf2oce()] and then using [class()] on
#' the result to make it be of subclass `"adv"`.  This is intended
#' to work with Netcdf files created with [adv2ncdf()], which embeds
#' sufficient information in the file to permit [ncdf2adv()] to
#' reconstruct the original adv object. See the documentation
#' for [adv2ncdf()] to learn more about what it stores, and therefore
#' what [ncdf2adv()] attempts to read.
#'
#' @inheritParams ncdf2oce
#'
#' @return [ncdf2adv()] returns an [adv-class] object.
#'
#' @family things related to adv data
#'
#' @examples
#' library(ocencdf)
#'
#' # Example with an adv file from oce package
#' data(adv, package="oce")
#' summary(adv)
#' plot(adv)
#' # Transfer to netcdf and back to see if results make sense
#' oce2ncdf(adv, ncfile="adv.nc")
#' ADV <- ncdf2adv("adv.nc")
#' summary(ADV)
#' plot(ADV)
#'
#' # Remove temporary file
#' file.remove("adv.nc")
#'
#' @author Dan Kelley
#'
#' @export
ncdf2adv <- function(ncfile=NULL, varTable=NULL, debug=0)
{
    adv <- ncdf2oce(ncfile=ncfile, varTable=varTable, debug=debug)
    # Need to tailor the types of some things that were not stored properly
    # in the Netcdf.  I think it's clearer to do this here, as opposed to
    # in ncdf2oce().
    mnames <- names(adv@metadata)
    if ("measurementStart" %in% mnames)
        adv@metadata$measurementStart <- as.POSIXct(adv@metadata$measurementStart, tz="UTC")
    if ("measurementEnd" %in% mnames)
        adv@metadata$measurementEnd <- as.POSIXct(adv@metadata$measurementEnd, tz="UTC")
    if ("hardwareConfiguration" %in% mnames)
        adv@metadata$hardwareConfiguration <- as.raw(paste0("0x", adv@metadata$hardwareConfiguration))
    class(adv) <- "adv"
    adv
}
