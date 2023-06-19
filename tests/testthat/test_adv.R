# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
data(adv, package="oce")

test_that("adv2ncdf on data(adv) creates a file with expected variable names",
    {
        ncfile <- tempfile(pattern="adv", fileext=".nc")
        expect_message(adv2ncdf(adv, ncfile=ncfile), "Defaulting varTable")
        o <- nc_open(ncfile)
        expect_equal(names(o$var),
            c("v", "a", "q", "time", "pressure", "timeBurst", "recordsBurst",
                "voltageSlow", "timeSlow", "headingSlow", "pitchSlow",
                "rollSlow", "temperatureSlow"))
        unlink(ncfile)
    })

test_that("ncdf2adv creates a file with expected variable names",
    {
        ncfile <- tempfile(pattern="adv", fileext=".nc")
        expect_message(adv2ncdf(adv, ncfile=ncfile), "Defaulting varTable")
        o <- nc_open(ncfile)
        ADV <- ncdf2adv(ncfile)
        for (name in names(adv@metadata)) {
            # Use is.numeric() rather than identical() because we want to match
            # e.g. 0 and 0L.  (Otherwise we'll need to have ncdf2adv() or
            # adv2ncdf() specify float for some things and integer for others,
            # but how can we know that for fields from other device types?
            if (is.numeric(adv@metadata[[name]])) {
                if (!all.equal(adv@metadata[[name]], ADV@metadata[[name]])) {
                    cat("conflict in", name, ":\n")
                    cat("  orig:  ", adv@metadata[[name]], "\n")
                    cat("  new:   ", ADV@metadata[[name]], "\n")
                }
            } else {
                if (!identical(adv@metadata[[name]], ADV@metadata[[name]])) {
                    cat("Conflict in", name, ":\n")
                    cat("  orig:  ", adv@metadata[[name]], "\n")
                    cat("  new:   ", ADV@metadata[[name]], "\n")
                }
            }
        }
        for (name in names(adv@data)) {
            # FIXME: move this comparison scheme into a new function
            # in misc.R; I'm just testing here.
            #.cat(name, "\n")
            if (is.integer(adv@data[[name]])) {
                if (!all.equal(adv@data[[name]], ADV@data[[name]])) {
                    cat("conflict in integer-class item ", name, ":\n")
                    cat("  orig:  ", adv@data[[name]], "\n")
                    cat("  new:   ", ADV@data[[name]], "\n")
                }
            } else if (is.raw(adv@data[[name]])) {
                dim <- dim(adv@data[[name]])
                ADV@data[[name]] <- as.raw(ADV@data[[name]])
                dim(ADV@data[[name]]) <- dim
                if (!all.equal(adv@data[[name]], ADV@data[[name]])) {
                    cat("conflict in raw-class item", name, ":\n")
                    cat("  orig:  ", adv@data[[name]], "\n")
                    cat("  new:   ", ADV@data[[name]], "\n")
                }
            } else if (inherits(adv@data[[name]], "POSIXt")) {
                if (!all.equal(adv@data$time, ADV@data$time)) {
                    cat("conflict in POSIXt-class item", name, ":\n")
                    cat("  orig:  ", adv@data[[name]], "\n")
                    cat("  new:   ", ADV@data[[name]], "\n")
                }
            } else {
                if (!all.equal(adv@data[[name]], ADV@data[[name]], tolerance=1e-7)) {
                    cat("conflict in float item ", name, ":\n")
                    cat("  orig:  ", adv@data[[name]], "\n")
                    cat("  new:   ", ADV@data[[name]], "\n")
                }
            }
        }
        unlink(ncfile)
    })
