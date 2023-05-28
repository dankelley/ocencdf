#' @inheritParams oce2ncdf
ctd2ncdf <- function(x, varTable="argo", ncfile="ctd.nc", debug=0)
{
    dmsg(debug, "ctd2ncdf(..., ncfile=\"", ncfile, "\") {\n")
    if (!inherits(x, "ctd"))
        stop("'x' must be a ctd object")
    varmap <- readVarTable(varTable)

    # Set up variable dimensions etc, using an argo file
    # (~/data/argo/D4901788_045.nc) as a pattern.
    missing_value <- 99999.0
    NLEVEL <- length(x@data[[1]])
    NLEVELdim <- ncdim_def(name="N_LEVEL", units="", vals=seq_len(NLEVEL), create_dimvar=FALSE)
    NPROFILEdim <- ncdim_def(name="N_PROFILE", units="", vals=1L, create_dimvar=FALSE)
    # create vars, using varmap for known items, and using just names otherwise
    # TO DO: determine whether we ought to examine the units in the oce object
    vars <- list()
    dmsg(debug, "  defining variable properties\n")
    for (name in names(x@data)) {
        if (name %in% names(varmap)) {
            dmsg(debug, "    ", name, " (to be called ", varmap[[name]]$name, ")\n")
            vars[[name]] <- ncvar_def(
                name=varmap[[name]]$name,
                units=varmap[[name]]$unit,
                longname=varmap[[name]]$long_name,
                missval=varmap[[name]]$missing_value,
                dim=NLEVELdim,
                prec="float")
        } else {
            dmsg(debug, "    ", name, "\n")
            vars[[name]] <- ncvar_def(
                name=name,
                units="",
                longname=name,
                missval=missing_value,
                dim=NLEVELdim,
                prec="float")
        }
    }
    # Start time (which some files have)
    time <- x[["time"]][1]
    timeExists <- !is.null(time)
    if (timeExists) {
        vars[["time"]] <- ncvar_def(
            name="TIME",
            units="seconds since 1970-01-01 UTC",
            longname="",
            missval=missing_value,
            dim=NPROFILEdim,
            prec="float")
        dmsg(debug, "    time\n")
    }
    # location
    longitude <- x[["longitude"]]
    latitude <- x[["latitude"]]
    locationExists <- is.finite(longitude) && is.finite(latitude)
    if (locationExists) {
        vars[["longitude"]] <- ncvar_def(
            name="longitude",
            units="degree_east",
            longname="Longitude of the station, best estimate",
            missval=missing_value,
            dim=NPROFILEdim,
            prec="float")
        dmsg(debug, "    longitude\n")
        vars[["latitude"]] <- ncvar_def(
            name="latitude",
            units="degree_north",
            longname="Latitude of the station, best estimate",
            missval=missing_value,
            dim=NPROFILEdim,
            prec="float")
        dmsg(debug, "    latitude\n")
    }
    nc <- nc_create(ncfile, vars)
    dmsg(debug, "  storing variable data\n")
    for (name in names(x@data)) {
        dmsg(debug, "    ", name, " (", NLEVEL, " values)\n")
        ncvar_put(nc=nc, varid=vars[[name]], vals=x@data[[name]])
    }
    if (timeExists) {
        ncvar_put(nc=nc, varid=vars[["time"]], vals=as.numeric(time[1]))
        dmsg(debug, "    time (", time[1], " i.e. ", oce::numberAsPOSIXct(time[1]), ")\n")
    }
    if (locationExists) {
        ncvar_put(nc=nc, varid=vars[["longitude"]], vals=longitude[1])
        dmsg(debug, "    longitude (", longitude, ")\n")
        ncvar_put(nc=nc, varid=vars[["latitude"]], vals=latitude[1])
        dmsg(debug, "    latitude (", latitude, ")\n")
    }
    ncatt_put(nc, 0, "metadata", paste(capture.output(str(x@metadata)), collapse="\n"))
    nc_close(nc)
    dmsg(debug, "} # ctd2ncdf\n")
}

