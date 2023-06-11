#' Save a ctd object to a netcdf file
#'
#' This creates a netcdf file in a convention that permits later reading by
#' [ncdf2ctd()], and that may be convenient for other purposes as well.
#'
#' Note that [oce2ncdf()] defaults `varTable` to `"argo"`.
#'
#' The contents of the `data` slot of the oce object `x` are as netcdf
#' data items.  If flags are present in the `metadata` slot, they are
#' also saved as data, with names ending in `_QC`.
#'
#' In addition to storage in the netcdf data section, several attributes
#' are saved as well. These include units for the data, which are tied
#' to the corresponding variables.  The entire `metadata` slot is stored
#' as a global attribute named `metadata`, so that a later call to
#' [ncdf2ctd()] will be able to recover the information, using an
#' `eval(parse(text=))` construct.  As an aid to processing in other
#' languages, the following `metadata` items are stored as
#' individual global attributes: `"latitude"`, `"longitude"`,
#' `"startTime"` and `"station"`.
#'
#'
#' @param x an oce object of class `ctd`, as created by e.g. [oce::as.ctd()]
#' or [oce::read.ctd()].
#'
#' @inheritParams oce2ncdf
#'
#' @examples
#' library(ocencdf)
#'
#' # example 1: a ctd file without per-variable QC flags
#' data(ctd, package="oce")
#' oce2ncdf(ctd, ncfile="ctd.nc")
#' CTD <- as.ctd(ncdf2oce("ctd.nc"))
#' summary(CTD)
#' plot(CTD)
#'
#' # example 2: a ctd file with per-variable QC flags
#' data(section, package="oce")
#' stn <- section[["station", 100]]
#' oce2ncdf(stn, ncfile="stn.nc")
#' STN <- as.ctd(ncdf2oce("stn.nc"))
#' summary(STN)
#' plot(STN)
#'
#' # Remove temporary files
#' file.remove("ctd.nc")
#' file.remove("stn.nc")
#'
#' @family things related to CTD data
#'
#' @author Dan Kelley
#'
#' @export
ctd2ncdf <- function(x, varTable=NULL, ncfile=NULL, debug=0)
{
    dmsg(debug, "ctd2ncdf(..., ncfile=\"", ncfile, "\") {\n")
    if (!inherits(x, "ctd"))
        stop("'x' must be a ctd object")
    if (is.null(varTable)) {
        varTable <- "argo"
        message("Defaulting varTable to \"", varTable, "\".")
    }
    if (is.null(ncfile)) {
        ncfile <- "ctd.nc"
        message("Will save ctd object to \"", ncfile, "\".")
    }
    varTableOrig <- varTable
    varTable <- read.varTable(varTable)
    # Set up variable dimensions etc, using an argo file
    # (~/data/argo/D4901788_045.nc) as a pattern.
    NLEVEL <- length(x@data[[1]])
    NLEVELdim <- ncdim_def(name="N_LEVEL", units="", vals=seq_len(NLEVEL), create_dimvar=FALSE)
    NPROFILEdim <- ncdim_def(name="N_PROFILE", units="", vals=1L, create_dimvar=FALSE)
    STRING16dim <- ncdim_def(name="STRING16", units="", vals=seq.int(1, 16), create_dimvar=FALSE)
    STRING32dim <- ncdim_def(name="STRING32", units="", vals=seq.int(1, 32), create_dimvar=FALSE)
    # create vars, using varmap for known items, and using just names otherwise
    # TO DO: determine whether we ought to examine the units in the oce object
    vars <- list()
    standardNames <- list() # called STANDARD_NAME in argo files
    dmsg(debug, "  Defining netcdf structure.\n")
    dmsg(debug, "    defining variable properties\n")
    for (name in names(x@data)) {
        dmsg(debug, "      ", name, "\n")
        varInfo <- getVarInfo(oce=x, name=name, varTable=varTable)
        units <- varInfo$unit
        # For the "argo" case, use "psu" as a unit for salinity.
        if (grepl("salinity", name) && varTable$type$name == "argo")
            units <- "psu"
        vars[[name]] <- ncvar_def(
            name=varInfo$name,
            units=units,
            longname=varInfo$long_name,
            missval=varTable$values$missing_value,
            dim=NLEVELdim,
            prec="float")
        standardNames[[name]] <- varInfo$standard_name
    }
    dmsg(debug, "    defining flag (QC) properties (if any exist)\n")
    flagnames <- names(x@metadata$flags)
    for (flagname in flagnames) {
        varInfo <- getVarInfo(oce=x, name=flagname, varTable=varTable)
        flagnameNCDF <- paste0(varInfo$name, "_QC")
        dmsg(debug, "      ", flagname, " -> ", flagnameNCDF, "\n")
        vars[[flagnameNCDF]] <- ncvar_def(
            name=flagnameNCDF,
            units="",
            longname=paste("QC for ", flagname),
            missval=varTable$values$missing_value,
            dim=NLEVELdim,
            prec="float")
    }
    dmsg(debug, "    defining variables for selected @metadata items\n")
    # location may be in data or metadata.  If the former, store in
    # a variable.  If the latter, store in an attribute.
    locationInData <- !is.null(x@data$longitude) && !is.null(x@data$latitude)
    locationInMetadata <- !is.null(x@metadata$longitude) && !is.null(x@metadata$latitude)
    if (locationInData) { # assume one value per profile
        vars[["longitude"]] <- ncvar_def(
            name=getVarInfo(name="longitude", varTable=varTable)$name,
            units="degree_east",
            longname="Longitude of the station, best estimate",
            missval=varTable$values$missing_value,
            dim=NPROFILEdim,
            prec="float")
        standardNames[["longitude"]] <- "longitude"
        dmsg(debug, "      longitude\n")
        vars[["latitude"]] <- ncvar_def(
            name=getVarInfo(name="latitude", varTable=varTable)$name,
            units="degree_north",
            longname="Latitude of the station, best estimate",
            missval=varTable$values$missing_value,
            dim=NPROFILEdim,
            prec="float")
        standardNames[["latitude"]] <- "latitude"
        dmsg(debug, "      latitude\n")
    }
    nc <- nc_create(ncfile, vars)
    dmsg(debug, "  Storing data.\n")
    for (name in names(x@data)) {
        dmsg(debug, "    ", name, " (", NLEVEL, " values)\n")
        vals <- x@data[[name]]
        vals[is.na(vals)] <- varTable$values$missing_value
        if (grepl("temperature", name, ignore.case=TRUE)) {
            scale <- x[[paste0(name, "Unit")]]$scale
            if (grepl("IPTS-68", scale, ignore.case=TRUE)) {
                message("Converting temperature from IPTS-68 scale to ITS-90 scale.")
                vals <- oce::T90fromT68(vals)
            } else if (grepl("ITS-48", scale, ignore.case=TRUE)) {
                message("Converting temperature from IPTS-48 scale to ITS-90 scale.")
                vals <- oce::T90fromT48(vals)
            }
        } else if (grepl("salinity", name, ignore.case=TRUE)) {
            scale <- x[[paste0(name, "Unit")]]$scale
            if (grepl("PSS-68", scale, ignore.case=TRUE)) {
                warning("cannot convert from PSS-68, so saving it unaltered")
            }
        }
        ncvar_put(nc=nc, varid=vars[[name]], vals=vals)
        sn <- standardNames[[name]]
        if (!is.null(sn))
            ncatt_put(nc=nc, varid=vars[[name]], attname="standard_name", attval=sn)
    }
    dmsg(debug, "  Storing QC values.\n")
    for (flagname in names(x@metadata$flags)) {
        varInfo <- getVarInfo(oce=x, name=flagname, varTable=varTable)
        vals <- x@metadata$flags[[flagname]]
        flagnameNCDF <- paste0(varInfo$name, "_QC")
        dmsg(debug, "      ", flagname, "Flag -> ", flagnameNCDF, "\n")
        ncvar_put(nc=nc, varid=vars[[flagnameNCDF]], vals=vals)
    }
    dmsg(debug, "  Storing global attributes.\n")
    dmsg(debug, "    metadata\n")
    ncatt_put(nc, 0, "metadata", paste(deparse(x@metadata), collapse="\n"))
    # Store some individual metadata items, for simple access
    for (item in c("station", "latitude", "longitude")) {
        dmsg(debug, "    ", item, "\n")
        storeNetcdfAttribute(x, item, nc, item)
    }
    dmsg(debug, "    varTable\n")
    ncatt_put(nc=nc, varid=0, attname="varTable", attval=varTableOrig)
    dmsg(debug, "    class\n")
    ncatt_put(nc=nc, varid=0, attname="class", attval=as.character(class(x)))
    dmsg(debug, "    creator\n")
    ncatt_put(nc=nc, varid=0, attname="creator", attval=paste0("ocencdf version ", packageVersion("ocencdf")))
    dmsg(debug, "  Closing netcdf file.\n")
    nc_close(nc)
    dmsg(debug, paste0("} # ctd2ncdf created file \"", ncfile, "\"\n"))
}


#' Read a netcdf file and create a ctd object
#'
#' @inheritParams ncdf2oce
#'
#' @return [ncdf2ctd()] returns an [ctd-class] object.
#'
#' @importFrom oce as.ctd
#' @importFrom utils packageVersion
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
