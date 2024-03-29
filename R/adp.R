#' Save an ADP object to a NetCDF file
#'
#' Given an `adp` object created by the `oce` package, this function
#' creates a NetCDF file that can later by read by [ncdf2adp()] to approximately
#' reproduce the original contents.
#'
#' Note that [adp2ncdf()] defaults `varTable` to `"adp"`.
#'
#' The entire contents of the metadata slot are saved in the global attribute named
#' `"metadata"`, in a JSON format.  The JSON material is developed with
#' [metadata2json()], which yields a value that can be decoded with
#' [json2metadata()].
#' In addition, the following metadata
#' items are saved as individual global attributes:
#' `"beamAngle"`,
#' `"frequency"`,
#' `"instrumentType"`,
#' `"instrumentSubtype"`,
#' `"numberOfBeams"`,
#' `"numberOfBeams"`,
#' and
#' `"oceCoordinate"`.
#'
#' @param x an `oce` object of class `adp`, as created by e.g. [oce::read.adp()].
#'
#' @inheritParams oce2ncdf
#'
#' @examples
#' library(ocencdf)
#'
#' # Example with an ADP file from `oce` package
#' data(adp, package = "oce")
#' summary(adp)
#' plot(adp)
#' # Transfer to NetCDF and back to see if results make sense.
#' # Use a temporary nc file to let package pass CRAN checks.
#' ncfile <- tempfile(pattern = "adp", fileext = ".nc")
#' oce2ncdf(adp, ncfile = ncfile)
#' ADP <- ncdf2adp(ncfile)
#' summary(ADP)
#' plot(ADP)
#' file.remove(ncfile)
#'
#' @family things related to adp data
#'
#' @author Dan Kelley and Clark Richards
#'
#' @export
adp2ncdf <- function(x, varTable = NULL, ncfile = NULL, force_v4 = TRUE, debug = 0) {
    dmsg(debug, "adp2ncdf(..., ncfile=\"", ncfile, "\") {\n")
    if (!inherits(x, "adp")) {
        stop("'x' must be a adp object")
    }
    if (is.null(varTable)) {
        varTable <- "adp"
        # message("Defaulting varTable to \"", varTable, "\".")
    }
    if (is.null(ncfile)) {
        ncfile <- "adp.nc"
        # message("Will save adp object to \"", ncfile, "\".")
    }
    varTableOrig <- varTable
    varTable <- read.varTable(varTable)
    vdim <- dim(x@data$v)
    extant <- list()
    for (item in c("v", "a", "g", "q")) {
        extant[[item]] <- item %in% names(x@data)
    }
    if (!extant$v) {
        stop("there is no data item named 'v', which is mandatory for an oce adp object")
    }
    # time <- ncdim_def(name="TIME", units="s", vals=as.numeric(x@data$time))
    time <- ncdim_def(
        name = "TIME", units = "", vals = seq_len(vdim[1]), create_dimvar = FALSE,
        longname = "seconds since 1970-01-01 UTC"
    )
    # cell <- ncdim_def(name="DISTANCE", units="m", vals=x@data$distance, longname="Distance to cell")
    distance <- ncdim_def(
        name = "DISTANCE", units = "", vals = seq_len(vdim[2]), create_dimvar = FALSE,
        longname = "Distance to cell"
    )
    beam <- ncdim_def(name = "BEAM", units = "", vals = seq_len(vdim[3]))
    vars <- list()
    FillValue <- getVarInfo("-", varTable = varTable)$FillValue
    # time and distance (do they show up as n$var now?)
    dmsg(debug, "    time (length ", vdim[1], ")\n")
    vars[["time"]] <- ncvar_def(name = "time", units = "seconds since 1970-01-01 UTC", dim = time, prec = "double")
    dmsg(debug, "    distance (length ", vdim[2], ")\n")
    vars[["distance"]] <- ncvar_def(name = "distance", units = "m", dim = distance)
    dmsg(debug, "  Setting up variable dimensions for ", paste(vdim, collapse = "x"), " arrays:\n")
    # array data
    dmsg(debug, "    v\n")
    vars[["v"]] <- ncvar_def(name = "v", units = "m/s", dim = list(time, distance, beam))
    if (extant$a) {
        dmsg(debug, "    a\n")
        vars[["a"]] <- ncvar_def("a", units = "", dim = list(time, distance, beam))
    }
    if (extant$g) {
        dmsg(debug, "    g\n")
        vars[["g"]] <- ncvar_def("g", units = "", dim = list(time, distance, beam))
    }
    if (extant$q) {
        dmsg(debug, "    q\n")
        vars[["q"]] <- ncvar_def("q", units = "", dim = list(time, distance, beam))
    }
    # time-series data
    dmsg(debug, "  Setting up dimensions for time-series vectors of length ", vdim[1], ":\n")
    # cat("NEXT: names in @data:\n");print(sort(names(x@data)))
    for (item in names(x@data)) {
        if (item != "time" && item != "distance" && is.vector(x@data[[item]])) {
            dmsg(debug, "    ", item, "\n")
            vars[[item]] <- ncvar_def(item, units = "", dim = time)
        }
    }
    nc <- nc_create(ncfile, vars, force_v4 = force_v4)
    dmsg(debug, "  Storing time and distance\n")
    dmsg(debug, "    time\n")
    ncvar_put(nc, "time", as.numeric(x@data$time))
    # message("first 3 times: ", paste(x@data$time[1:3], collapse=" "))
    # message("first 3 times: ", paste(as.numeric(x@data$time)[1:3], collapse=" "))
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
    dmsg(debug, "    metadata_explanation\n")
    explanation <- paste("This file was created with adp2ncdf from the ocencdf R package,\n",
        "available at www.github.com/dankelley/ocencdf.\n",
        readLines(system.file("extdata", "ncdf_explanation.md", package = "ocencdf")),
        collapse = "\n"
    )
    ncatt_put(nc, 0, "metadata_explanation", explanation)
    dmsg(debug, "    metadata\n")
    ncatt_put(nc, 0, "metadata", metadata2json(x@metadata))
    # Store some individual metadata items, for simple access
    for (item in c(
        "beamAngle", "frequency",
        "instrumentType", "instrumentSubtype",
        "numberOfBeams", "numberOfBeams",
        "oceCoordinate"
    )) {
        dmsg(debug, "    ", item, "\n")
        storeNetCDFAttribute(x, item, nc, item)
    }
    dmsg(debug, "    varTable\n")
    ncatt_put(nc = nc, varid = 0, attname = "varTable", attval = varTableOrig)
    dmsg(debug, "    class\n")
    ncatt_put(nc = nc, varid = 0, attname = "class", attval = as.character(class(x)))
    dmsg(debug, "    creator\n")
    ncatt_put(nc = nc, varid = 0, attname = "creator", attval = paste0("ocencdf version ", packageVersion("ocencdf")))
    nc_close(nc)
    dmsg(debug, paste0("} # adp2ncdf created file \"", ncfile, "\"\n"))
}

#' Read a NetCDF file and create an ADP object
#'
#' This works by calling [ncdf2oce()] and then using [class()] on
#' the result to make it be of subclass `"adp"`.  This is intended
#' to work with NetCDF files created with [adp2ncdf()], which embeds
#' sufficient information in the file to permit [ncdf2adp()] to
#' reconstruct the original adp object. See the documentation
#' for [adp2ncdf()] to learn more about what it stores, and therefore
#' what [ncdf2adp()] attempts to read.
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
#' # Example with an ADP file from `oce` package
#' data(adp, package = "oce")
#' summary(adp)
#' plot(adp)
#' # Transfer to NetCDF and back to see if results make sense
#' # Use a temporary nc file to let package pass CRAN checks.
#' ncfile <- tempfile(pattern = "adp", fileext = ".nc")
#' oce2ncdf(adp, ncfile = ncfile)
#' ADP <- ncdf2adp(ncfile)
#' summary(ADP)
#' plot(ADP)
#' file.remove(ncfile)
#'
#' @family things related to adp data
#'
#' @author Dan Kelley
#'
#' @export
ncdf2adp <- function(ncfile = NULL, varTable = NULL, debug = 0) {
    adp <- ncdf2oce(ncfile = ncfile, varTable = varTable, debug = debug)
    class(adp) <- "adp"
    adp
}
