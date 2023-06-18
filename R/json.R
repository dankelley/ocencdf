#' Convert an oce metadata slot to JSON
#'
#' This converts the contents of an object's `metadata` slot
#' into JSON format, using the `toJSON()` function from the
#' `jsonlite` library.  Some conversion is required, and this
#' is also handled if the results are reconstituted using
#' [json2metadata()].  See \dQuote{Details}.
#'
#' Some `metadata` items cannot be handled by [jsonlite::toJSON()], so
#' these are transformed as follows before storage in the return value
#' NOTE: the reverse function, [json2metadata()], reverses all of these
#' transformations, so that the original `metadata` can be recovered.
#' 1. Oce uses `expression` objects to store units, and these are converted
#' to character values using [as.character()] before converting to JSON.
#' 2. Oce stores a raw matrix called `codes` for `adp` objects created from
#' RDI/Teledyne files, and so this is converted to an integer matrix
#' before converting to JSON.
#' 3. The following items are converted from POSIXct values to character
#' values: `date`, `endTime`, `startTime`, and `systemUploadTime`.
#'
#' @param m contents of the `metadata` slot of an oce object.
#'
#' @param digits integer, the number of digits to store in the JSON
#' representation.
#'
#' @return [metadata2json] returns a character value holding
#' the `metadata` slot in JSON, transformed as indicated in
#' the \dQuote{Details} section.
#'
#' @examples
#' # Example 1: ctd data
#' data("ctd")
#' metadata2json(ctd@metadata)
#'
#' # Example 2: adp data
#' data("adp")
#' metadata2json(adp@metadata)
#'
#' @family things relating to serialization
#'
#' @author Dan Kelley
#'
#' @export
metadata2json <- function(m, digits=15)
{
    if ("units" %in% names(m)) {
        for (item in names(m$units)) {
            m$units[[item]]$unit <- as.character(m$units[[item]]$unit)
        }
    }
    C <- m$codes
    if (!is.null(C) && is.matrix(C)) {
        Cnew <- as.integer(C)
        dim(Cnew) <- dim(C)
        m$codes <- Cnew
    }
    jsonlite::toJSON(m, digits=digits, pretty=TRUE, raw="hex")
}

#' Convert a JSON string to an oce metadata slot
#'
#' This is the reverse of [metadata2json()], and is used by
#' [ncdf2oce()] and related functions.  See [metadata2json()]
#' for some conversions that are done by that function and
#' then reversed here.
#'
#' @param j character value, typically the output from [metadata2json()].
#'
#' @return [json2metadata] returns a list in the format of a `metadata`
#' slot from an `oce` object.
#'
#' @family things relating to serialization
#'
#' @author Dan Kelley
#'
#' @export
json2metadata <- function(j)
{
    m <- jsonlite::fromJSON(j)
    for (item in names(m$units))
        m$units[[item]]$unit <- parse(text=m$units[[item]]$unit, keep.source=FALSE)
    for (t in c("date", "endTime", "startTime", "systemUploadTime")) {
        if (!is.null(m[[t]]))
            m[[t]] <- as.POSIXct(m[[t]], tz="UTC")
    }
    C <- m$codes
    if (is.matrix(m$codes)) {
        C <- as.raw(m$codes)
        dim(C) <- dim(m$codes)
        m$codes <- C
    }
    m
}

