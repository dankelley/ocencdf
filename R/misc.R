makeNumeric <- function(debug, x) {
    if (!is.numeric(x)) {
        if (is.array(x)) {
            dim <- dim(x)
            x <- as.numeric(x)
            dim(x) <- dim
        } else {
            x <- as.numeric(x)
        }
    }
    x
}

typeNcdf <- function(x) {
    Rtype <- typeof(x)
    switch(Rtype,
        double = "double",
        integer = "integer",
        raw = "byte",
        character = "char"
    )
}

dmsg <- function(debug, ...) {
    if (debug > 0) {
        cat(..., sep = "")
    }
}

storeNetCDFAttribute <- function(x, ocename, nc, ncname) {
    if (missing(ncname)) {
        ncname <- ocename
    }
    item <- x@metadata[[ocename]]
    if (!is.null(item)) {
        ncatt_put(nc, 0, ncname, item)
    }
}
