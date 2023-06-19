#' Save an adv object to a netcdf file
#'
#' Given an `adv` object created by the `oce` package, this function
#' creates a netcdf file that can later by read by [ncdf2adv()] to approximately
#' reproduce the original contents.
#'
#' Note that [oce2ncdf()] defaults `varTable` to `"-"`,
#' meaning that no variable-name translation is done; the names used in
#' oce are retained in the file.
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
#' @author Dan Kelley
#'
#' @export
adv2ncdf <- function(x, varTable=NULL, ncfile=NULL, debug=0)
{
    dmsg(debug, "adv2ncdf(..., ncfile=\"", ncfile, "\") {\n")
    if (!inherits(x, "adv"))
        stop("'x' must be a adv object")
    if (is.null(varTable)) {
        varTable <- "-"
        message("Defaulting varTable to \"", varTable, "\".")
    }
    if (is.null(ncfile)) {
        ncfile <- "adv.nc"
        message("Will save adv object to \"", ncfile, "\".")
    }
    varTableOrig <- varTable
    varTable <- read.varTable(varTable)
    dataNames <- names(x@data)
    if (!"v" %in% dataNames)
        stop("there is no data item named 'v', which is mandatory for an oce adv object")
    vdim <- dim(x@data$v)
    timeFastLen <- vdim[1]
    dmsg(debug, "timeFastLen=", timeFastLen, "\n")
    slowIndices <- grep("Slow$", dataNames)
    anySlow <- length(slowIndices) > 0L
    dmsg(debug, "anySlow=", anySlow, "\n")
    timeSlowLen <- if (anySlow) length(x@data[[slowIndices[1]]]) else 0
    dmsg(debug, "timeSlowLen=", timeSlowLen, "\n")
    timeFast <- ncdim_def(name="TIME_FAST", units="", vals=seq_len(timeFastLen), create_dimvar=FALSE,
        longname="seconds since 1970-01-01 UTC")
    if (anySlow)
        timeSlow <- ncdim_def(name="TIME_SLOW", units="", vals=seq_len(timeSlowLen), create_dimvar=FALSE,
            longname="seconds since 1970-01-01 UTC")
    beam <- ncdim_def(name="BEAM", units="", vals=seq_len(vdim[2]))
    vars <- list()
    # Set up space for each item
    for (name in dataNames) {
        item <- x@data[[name]]
        if (is.matrix(item)) {
            if (!identical(dim(item), vdim))
                stop("dimension of \"", name, "\" (",
                    paste(dim(item), collapse="x"), ") does not match dimension of \"v\" (",
                    paste(vdim, collapse="x"), ")")
            vars[[name]] <- ncvar_def(name, "", list(timeFast, beam), 1.0e30)
        } else {
            isTime <- grepl("time", name, ignore.case=TRUE)
            if (length(item) == timeSlowLen) {
                vars[[name]] <- if (isTime)
                    ncvar_def(name, "seconds since 1970-01-01 UTC", list(timeSlow), 1.0e30, prec="double")
                else
                    ncvar_def(name, "", list(timeSlow), 1.0e30)
            } else if (length(item) == timeFastLen) {
                vars[[name]] <- if (isTime)
                    ncvar_def(name, "seconds since 1970-01-01 UTC", list(timeFast), 1.0e30, prec="double")
                else
                    ncvar_def(name, "", list(timeFast), 1.0e30)
            } else {
                stop("item \"", name, "\" has length ", length(item), " but it should be either ", timeSlowLen,
                    " or ", timeFastLen)
            }
        }
    }
    nc <- nc_create(ncfile, vars)
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
