# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
data(ctd, package="oce")
data(section, package="oce")
stn <- section[["station", 100]]

test_that("ctd2nc on data(ctd) creates a file with expected variable names",
    {
        ncfile <- tempfile(pattern="ctd", fileext=".nc")
        expect_message(
            expect_message(ctd2ncdf(ctd, ncfile=ncfile),
                "Defaulting varTable"),
            "Converting temperature")
        o <- nc_open(ncfile)
        expect_equal(names(o$var),
            c("scan", "timeS", "PRES", "depth", "TEMP", "PSAL", "flag", "TIME",
                "station", "LONGITUDE", "LATITUDE"))
        unlink(ncfile)
    })

test_that("ctd2nc on a section station creates a file with expected variable names",
    {
        ncfile <- tempfile(pattern="ctd", fileext=".nc")
        expect_message(
            expect_message(ctd2ncdf(stn, ncfile=ncfile),
                "Defaulting varTable"),
            "Converting temperature")
        #o <- nc_open(ncfile)
        #expect_equal(names(o$var),
        #    c("scan", "timeS", "PRES", "depth", "TEMP", "PSAL", "flag", "TIME",
        #        "station", "LONGITUDE", "LATITUDE"))
        unlink(ncfile)
    })
