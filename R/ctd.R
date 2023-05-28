#' @inheritParams oce2ncdf
ctd2ncdf <- function(x, varTable="argo", ncfile="ctd.nc", debug=0)
{
    dmsg(debug, "ctd2ncdf(..., ncfile=\"", ncfile, "\") {\n")
    if (!inherits(x, "ctd"))
        stop("'x' must be a ctd object")
    varmap <- readVarTable(varTable)

    # 2. write to netcdf
    N <- length(x@data[[1]]) # FIXME: do in the loop
    # Pattern units, names, etc., on argo file ~/data/argo/D4901788_045.nc
    missing_value <- 99999.0
    Ndim <- ncdim_def(name="N_LEVEL", units="", vals=seq_len(N), create_dimvar=FALSE)
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
                dim=Ndim,
                prec="float")
        } else {
            dmsg(debug, "    ", name, "\n")
            vars[[name]] <- ncvar_def(
                name=name,
                units="",
                longname=name,
                missval=missing_value,
                dim=Ndim,
                prec="float")
        }
    }
    nc <- nc_create(ncfile, vars)
    dmsg(debug, "  storing variable data\n")
    for (name in names(x@data)) {
        dmsg(debug, "    ", name, "\n")
        ncvar_put(nc=nc, varid=vars[[name]], vals=x@data[[name]])
    }
    ncatt_put(nc, 0, "metadata", paste(capture.output(str(x@metadata)), collapse="\n"))
    nc_close(nc)
    dmsg(debug, "} # ctd2ncdf\n")
}

