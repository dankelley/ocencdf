# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
data(ctd, package="oce")

test_that("ctd2nc creates a file with expected variable names",
    {
        ncfile <- tempfile(pattern="ctd", fileext=".nc")
        expect_message(
            expect_message(ctd2ncdf(ctd, ncfile=ncfile),
                "defaulting varTable to \"argo\""),
            "Note: converted \"temperature\"")
        o <- nc_open(ncfile)
        expect_equal(names(o$var),
            c("scan", "timeS", "PRES", "depth", "TEMP", "PSAL", "flag", "TIME",
                "station", "LONGITUDE", "LATITUDE"))
        unlink(ncfile)
    })

