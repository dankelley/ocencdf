#' Read a variable-information table
#'
#' This function, meant for internal use by the package, uses
#' [yaml::yaml.load_file()] to read YAML files that describe the
#' output netcdf format created by e.g. [ctd2ncdf()]. Users wishing
#' to define such files for their own use should follow the pattern
#' of the source directory `inst/extdata/argo.yml`.
#'
#' @param varTable character value indicating the name of the table.  If this
#' ends in `.yml`, then it is taken as a file to be read.  Alternatively, it
#' is taken to be the name of a built-in scheme that is defined in a filename
#' given by appending `.yml` on the end of `varTable`, to be found in the
#' `inst/ext_data` source directory, as in the example.  At the moment,
#' the two provided files correspond to `varTable` values of `"whp"`
#' and `"argo"`.
#'
#' @template debugTemplate
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
read.varTable <- function(varTable="argo", debug=0)
{
    if (!is.character(varTable))
        stop("varTable must be a character value")
    if (!is.numeric(debug))
        stop("debug must be a numeric value, but is ", debug)
    varTableOrig <- varTable
    if (!grepl(".yml$", varTable))
        varTable <- system.file("extdata", paste0(varTable, ".yml"), package="ocencdf")
    if (nchar(varTable) < 1L)
        stop("file there is no \"", varTableOrig, ".yml\" file in ", system.file(package="ocencdf"))
    if (!file.exists(varTable))
        stop("file \"", varTableOrig, "\" does not exist")
    dmsg(debug, "read.varTable(\"", varTable, "\") {\n")
    rval <- yaml::yaml.load_file(varTable)
    # Fill in empty units and longnames with defaults that at least permit
    # ctd2ncdf(), etc., to run without error.
    # FIXME: keep this, or write functions to extract things and let them
    # default?  The latter seems clearer, and using a function will help users
    # who want to adjust the coding.
    variableNames <- names(rval$variables)
    for (i in seq_along(variableNames)) {
        name <- variableNames[i]
        dmsg(debug, "  handling \"", name, "\"\n")
        #if (is.null(rval$variables[[i]]$units))
        #    rval$variables[[i]]$units <- ""
        if (is.null(rval$variables[[i]]$long_name))
            rval$variables[[i]]$long_name <- name
        if (is.null(rval$variables[[i]]$standard_name))
            rval$variables[[i]]$standard_name <- name
        if (is.null(rval$variables[[i]]$missing_value))
            rval$variables[[i]]$missing_value <- 99999.0
    }
    dmsg(debug, "} # read.varTable()\n")
    rval
}

#' Get information on a variable, using varTable
#'
#' This is used by e.g. [ctd2ncdf()] to determine how to describe the variable in a
#' particular flavour of netcdf file, as specified by [read.varTable()].
#'
#' @param name character value naming the variable.  If `name` is
#' not the name of a chemical species, then trailing digits are removed,
#' as a way to handle instruments that, for example, have multiple
#' temperature sensors.  Chemical species are recognized by
#' the presence of any one of the following strings:
#' `"CO"`, `"NO"`, `"PO" and "O2"`.
#'
#' @param varTable either a variable table as read by [read.varTable()],
#' or a character string that is to be passed to that function to
#' create a variable table.
#'
#' @param oce (optional) an oce object.  If provided, then an attempt
#' is made to infer the unit from it.  Otherwise, the returned `unit`
#' entry is an empty string.
#'
#' @template debugTemplate
#'
#' @return [getVarInfo()] returns a list containing `name` (the
#' name as used in argo netcdf files), `long_name` (again, as used in
#' Argo netcdf files, although the usefulness of this is debatable),
#' `standard_name` (not used by [ctd2ncdf()] as of now), `FillValue`
#' (used by [ctd2ncdf()] for missing values) and, if `oce` is provided
#' and it can be determined, `unit` (a character string specifying
#' the unit).
#'
#' @examples
#' library(ocencdf)
#'
#' # Example
#' data(ctd)
#' vt <- read.varTable("argo")
#' getVarInfo("temperature", vt, ctd)
#'
#' @author Dan Kelley
#'
#' @export
getVarInfo <- function(name=NULL, varTable=NULL, oce=NULL, debug=0)
{
    # Error checking.
    if (!is.null(oce) && !inherits(oce, "oce"))
        stop("oce must be an 'oce' object, e.g. made by read.oce()")
    if (is.null(name))
        stop("must supply name")
    if (!is.character(name))
        stop("name must be a character value")
    if (is.null(varTable))
        stop("must supply varTable")
    dmsg(debug, "getVarInfo(name=\"", name, "\", varTable=\"", varTable, "\")\n")
    if (is.character(varTable))
        varTable <- read.varTable(varTable)
    if (!is.list(varTable))
        stop("varTable must be a character value, or the output of read.varTable()")
    # Remove trailing numbers e.g. temperature2, but not in e.g. NO2.
    suffix <- ""
    dmsg(debug, "  name=\"", name, "\"\n")
    if (!grepl("(CO)|(NO)|(PO)|(O2)", name) && grepl("[0-9]$", name)) {
        suffix <- gsub("[a-zA-Z_]*([0-9]+)$", "\\1", name)
        name <- gsub("([a-zA-Z_]*)[0-9]+$", "\\1", name)
        dmsg(debug, "    after transformation, name=\"", name, "\"; suffix=\"", suffix, "\"\n")
    }
    FillValue <- varTable$values$missing_value
    # Establish a default return value.
    rval <- list(name=name, long_name=name, FillValue=FillValue, unit="")
    # Fill in variable names and fill value, if they can be determined.
    if (name %in% names(varTable$variables)) {
        tmp <- varTable$variables[[name]]$name
        if (!is.null(tmp))
            rval$name <- tmp
        tmp <- varTable$variables[[name]]$long_name
        if (!is.null(tmp))
            rval$long_name <- tmp
        tmp <- varTable$variables[[name]]$standard_name
        if (!is.null(tmp))
            rval$standard_name <- tmp
    }
    rval$name <- paste0(rval$name, suffix)
    # Fill in units, if they can be determined.
    # BUG: with this scheme, there is no way to change salinity units (an empty
    # string for oce objects) to e.g. "psu", which is what argo expects.  I am
    # not willing to change oce to record a unit, in contravention to convention,
    # but we could special-case this in ctd2ncdf().
    if (!is.null(oce)) {
        unit <- as.character(oce[[paste0(name, "Unit")]]$unit)
        if (length(unit) && (unit %in% names(varTable$units)))
            rval$unit <- varTable$units[[unit]]$name
    }
    dmsg(debug, "} # getVarInfo()\n")
    rval
}

#' Translate netcdf names to oce  names
#'
#' @param names vector of character values in oce convention (e.g. "TEMP"
#' for temperature, if varTable is "argo").
#'
#' @template varTableTemplate
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @export
ncdfNames2oceNames <- function(names, varTable=NULL, debug=0)
{
    vt <- read.varTable(varTable)
    synonyms <- lapply(vt$variables, function(v) v$name)
    translation <- data.frame(
        oce=names(synonyms),
        ncdf=unlist(unname(synonyms)))
    dmsg(debug, "input: ", paste(names, collapse=" "), "\n")
    for (name in names) {
        w <- which(name == translation$ncdf)
        if (length(w) > 0L)
            names <- gsub(translation[w, "ncdf"], translation[w, "oce"], names)
    }
    dmsg(debug, "returning: ", paste(names, collapse=" "), "\n")
    names
}

#' Translate netcdf names to oce names
#'
#' @param names vector of character values in oce convention (e.g. "temperature"
#' for temperature).
#'
#' @template varTableTemplate
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @export
oceNames2ncdfNames <- function(names, varTable=NULL, debug=0)
{
    vt <- read.varTable(varTable)
    synonyms <- lapply(vt$variables, function(v) v$name)
    translation <- data.frame(
        oce=names(synonyms),
        ncdf=unlist(unname(synonyms)))
    dmsg(debug, "input: ", paste(names, collapse=" "), "\n")
    for (name in names) {
        w <- which(name == translation$oce)
        if (length(w) > 0L)
            names <- gsub(translation[w, "oce"], translation[w, "ncdf"], names)
    }
    dmsg(debug, "returning: ", paste(names, collapse=" "), "\n")
    names
}

