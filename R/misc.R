dmsg <- function(debug, ...)
    if (debug > 0)
        cat(..., sep="")

#' Read a variable-information table
#'
#' Entries in the file must look like the following.  Note that not all fields
#' need to be provided.  The first line in each block is the name of
#' the field as stored in oce objects.  The sub-fields will all default
#' if not provided, with `units` becoming `""`, with `name`, `long_name`
#' and `standard_name` all defaulting to the oce name.  A value
#' of 99999.0 is used for the missing value if that is not
#' provided.
#'\preformatted{
#'temperature:
#'    name: "TEMP"
#'    units: "degree_Celcius"
#'    long_name: "Sea temperature in-situ ITS-90 scale"
#'    standard_name: "sea_water_temperature"
#'    missing_value: 99999.0
#'}
#'
#' @param varTable character value indicating the name of the table.  If this
#' ends in `.yml`, then it is taken as a file to be read.  Alternatively, it
#' is taken to be the name of a built-in scheme that is defined in a filename
#' given by appending `.yml` on the end of `varTable`, to be found in the
#' `inst/ext_data` source directory, as in the example.  At the moment,
#' the two provided files correspond to `varTable` values of `"whp"`
#' and `"argo"`.
#'
#' @return [read.varTable()] returns a list that specifies some information
#' to be stored in netcdf files created by e.g. [ctd2ncdf()].
#'
#' @examples
#' library(ocencdf)
#' str(read.varTable("whp"))
#'
#' @export
#'
#' @author Dan Kelley
read.varTable <- function(varTable="argo")
{
    if (!grepl(".yml$", varTable))
        varTable <- system.file("extdata", paste0(varTable, ".yml"), package="ocencdf")
    if (!file.exists(varTable))
        stop("file \"", varTable, "\" does not exist")
    rval <- yaml::yaml.load_file(varTable)
    # Fill in empty units and longnames with defaults that at least permit
    # ctd2ncdf(), etc., to run without error.
    rvalNames <- names(rval)
    for (i in seq_along(rval)) {
        name <- rvalNames[i]
        if (is.null(rval[[i]]$units))
            rval[[i]]$units <- ""
        if (is.null(rval[[i]]$long_name))
            rval[[i]]$long_name <- name
        if (is.null(rval[[i]]$standard_name))
            rval[[i]]$standard_name <- name
        if (is.null(rval[[i]]$missing_value))
            rval[[i]]$missing_value <- 99999.0
    }
    rval
}
