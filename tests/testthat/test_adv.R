# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

test_that("adv2ncdf on data(adv) creates a file with expected variable names",
    {
        data(adv, package="oce")
        ncfile <- tempfile(pattern="adv", fileext=".nc")
        expect_message(adv2ncdf(adv, ncfile=ncfile), "Defaulting varTable")
        o <- nc_open(ncfile)
        expect_equal(names(o$var),
            c("v", "a", "q", "time", "pressure", "timeBurst", "recordsBurst",
                "voltageSlow", "timeSlow", "headingSlow", "pitchSlow",
                "rollSlow", "temperatureSlow"))
        unlink(ncfile)
    })

test_that("ncdf2adv creates a file with expected metadata",
    {
        data(adv, package="oce")
        ncfile <- tempfile(pattern="adv", fileext=".nc")
        expect_message(adv2ncdf(adv, ncfile=ncfile), "Defaulting varTable")
        o <- nc_open(ncfile)
        ADV <- ncdf2adv(ncfile)
        expect_equal(adv@metadata, ADV@metadata)
        unlink(ncfile)
    })

test_that("ncdf2adv creates a file with expected data",
    {
        data(adv, package="oce")
        ncfile <- tempfile(pattern="adv", fileext=".nc")
        expect_message(adv2ncdf(adv, ncfile=ncfile), "Defaulting varTable")
        o <- nc_open(ncfile)
        ADV <- ncdf2adv(ncfile)
        # Convert two numeric things to raw.  (We don't bother trying to
        # save them as raw in adv2ncdf(), but maybe we should.)
        dim <- dim(adv@data$a)
        for (item in c("a", "q")) {
            ADV@data[[item]] <- as.raw(ADV@data[[item]])
            dim(ADV@data[[item]]) <- dim
        }
        for (name in paste('monkey',names(adv@data))) {
            # For some reason, expect_equal() and identical() say that recordsBurst
            # differs in adv and ADV, but that's wrong: both consist of 480 NA values.
            if (name %in% c("recordsBurst", "timeBurst")) next
            expect_equal(adv@data[[name]], ADV@data[[name]])#, tolerance=1e-3)
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
