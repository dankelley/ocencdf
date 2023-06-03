#' Save a ctd object to a netcdf file
#'
#' @param x an oce object of class `ctd`, as created by e.g. [oce::as.ctd()]
#' or [oce::read.ctd()].
#'
#' @inheritParams oce2ncdf
#'
#' @export
#'
#' @examples
#' library(ocencdf)
#'
#' # example 1: a ctd file with no per-variable QC flags
#' data(ctd) # from 'oce' package
#' oce2ncdf(ctd, ncfile="ctd1.nc")
#' d <- read.netcdf('ctd1.nc') |> as.ctd()
#' plot(d)
#'
#' # example 2: a ctd file with per-variable QC flags
#' data(section) # from 'oce' package
#' stn <- section[["station", 100]] # 100-th station in section, not station '100'
#' oce2ncdf(stn, ncfile="ctd2.nc")
#' d <- read.netcdf('ctd2.nc') |> as.ctd()
#' plot(d)
#'
#' # clean up temporary files (to prevent CRAN test failure)
#' unlink("ctd1.nc")
#' unlink("ctd2.nc")
#'
#' @author Dan Kelley
ctd2ncdf <- function(x, varTable=NULL, ncfile=NULL, debug=0)
{
    dmsg(debug, "ctd2ncdf(..., ncfile=\"", ncfile, "\") {\n")
    if (!inherits(x, "ctd"))
        stop("'x' must be a ctd object")
    if (is.null(varTable)) {
        varTable <- "argo"
        message("defaulting varTable to \"", varTable, "\"")
    }
    if (is.null(ncfile)) {
        ncfile <- "ctd.nc"
        message("Will save ctd object to \"", ncfile, "\".")
    }
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
    dmsg(debug, "  defining variable properties\n")
    for (name in names(x@data)) {
        dmsg(debug, "    ", name, "\n")
        varInfo <- getVariableInfo(oce=x, name=name, varTable=varTable)
        vars[[name]] <- ncvar_def(
            name=varInfo$name,
            units=varInfo$unit,
            longname=varInfo$long_name,
            missval=varTable$values$missing_value,
            dim=NLEVELdim,
            prec="float")
    }
    dmsg(debug, "  defining flag (QC) properties (if any exist)\n")
    flagnames <- names(x@metadata$flags)
    for (flagname in flagnames) {
        #message(oce::vectorShow(flagname))
        varInfo <- getVariableInfo(oce=x, name=flagname, varTable=varTable)
        #print(varInfo)
        #browser()
        flagnameNCDF <- paste0(varInfo$name, "_QC")
        dmsg(debug, "    ", flagname, " -> ", flagnameNCDF, "\n")
        vars[[flagnameNCDF]] <- ncvar_def(
            name=flagnameNCDF,
            units="",
            longname=paste("QC for ", flagname),
            missval=varTable$values$missing_value,
            dim=NLEVELdim,
            prec="float")
    }
    dmsg(debug, "  defining variables for selected @metadata items\n")
    # Start time (which some files have)
    time <- x[["time"]][1]
    timeExists <- !is.null(time)
    if (timeExists) {
        vars[["time"]] <- ncvar_def(
            name="TIME",
            units="seconds since 1970-01-01 UTC",
            longname="",
            missval=varTable$values$missing_value,
            dim=NPROFILEdim,
            prec="float")
        dmsg(debug, "    time\n")
    }
    # Station (which some files have)
    station <- x[["station"]][1]
    stationExists <- !is.null(station)
    if (stationExists) {
        vars[["station"]] <- ncvar_def(
            name="station",
            units="",
            longname="",
            missval="",
            dim=STRING16dim,
            prec="char")
        dmsg(debug, "    station\n")
    }
    # location
    longitude <- x[["longitude"]]
    latitude <- x[["latitude"]]
    locationExists <- is.finite(longitude) && is.finite(latitude)
    if (locationExists) {
        vars[["longitude"]] <- ncvar_def(
            name=getVariableInfo(name="longitude", varTable=varTable)$name,
            units="degree_east",
            longname="Longitude of the station, best estimate",
            missval=varTable$values$missing_value,
            dim=NPROFILEdim,
            prec="float")
        dmsg(debug, "    longitude\n")
        vars[["latitude"]] <- ncvar_def(
            name=getVariableInfo(name="latitude", varTable=varTable)$name,
            units="degree_north",
            longname="Latitude of the station, best estimate",
            missval=varTable$values$missing_value,
            dim=NPROFILEdim,
            prec="float")
        dmsg(debug, "    latitude\n")
    }
    nc <- nc_create(ncfile, vars)
    dmsg(debug, "  storing variable values\n")
    for (name in names(x@data)) {
        dmsg(debug, "    ", name, " (", NLEVEL, " values)\n")
        vals <- x@data[[name]]
        vals[is.na(vals)] <- varTable$values$missing_value
        if (grepl("temperature", name, ignore.case=TRUE)) {
            scale <- x[[paste0(name, "Unit")]]$scale
            if (grepl("IPTS-68", scale, ignore.case=TRUE)) {
                message("Note: converted \"", name, "\" from the IPTS-68 scale to the ITS-90 scale.")
                vals <- oce::T90fromT68(vals)
            } else if (grepl("ITS-48", scale, ignore.case=TRUE)) {
                warning("converting \"", name, "\" from IPTS-48 scale to ITS-90 scale")
                vals <- oce::T90fromT48(vals)
            }
        } else if (grepl("salinity", name, ignore.case=TRUE)) {
            scale <- x[[paste0(name, "Unit")]]$scale
            if (grepl("PSS-68", scale, ignore.case=TRUE)) {
                warning("cannot convert from PSS-68, so saving it unaltered")
            }
        }
        ncvar_put(nc=nc, varid=vars[[name]], vals=vals)
    }
    dmsg(debug, "  storing QC values\n")
    for (flagname in names(x@metadata$flags)) {
        varInfo <- getVariableInfo(oce=x, name=flagname, varTable=varTable)
        vals <- x@metadata$flags[[flagname]]
        flagnameNCDF <- paste0(varInfo$name, "_QC")
        dmsg(debug, "    ", flagname, "Flag -> ", flagnameNCDF, "\n")
        ncvar_put(nc=nc, varid=vars[[flagnameNCDF]], vals=vals)
    }
    dmsg(debug, "  storing selected @metadata items\n")
    if (timeExists) {
        ncvar_put(nc=nc, varid=vars[["time"]], vals=as.numeric(time[1]))
        dmsg(debug, "    time (", time[1], " i.e. ", format(oce::numberAsPOSIXct(time[1])), ")\n")
    }
    if (stationExists) {
        ncvar_put(nc=nc, varid=vars[["station"]], vals=sprintf("%-16s", substr(station[1], 1L, 16L)))
        dmsg(debug, "    station (", station[1], ")\n")
    }
    if (locationExists) {
        ncvar_put(nc=nc, varid=vars[["longitude"]], vals=longitude[1])
        dmsg(debug, "    longitude (", longitude, ")\n")
        ncvar_put(nc=nc, varid=vars[["latitude"]], vals=latitude[1])
        dmsg(debug, "    latitude (", latitude, ")\n")
    }
    ncatt_put(nc, 0, "metadata", paste(capture.output(str(x@metadata)), collapse="\n"))
    nc_close(nc)
    dmsg(debug, paste0("} # ctd2ncdf created file \"", ncfile, "\"\n"))
}

