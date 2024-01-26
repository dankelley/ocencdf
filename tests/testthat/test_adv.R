# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

test_that("adv2ncdf/ncdf2adv duplicates original data and metadata",
    {
        data(adv, package="oce")
        ncfile <- tempfile(pattern="adv", fileext=".nc")
        expect_silent(adv2ncdf(adv, ncfile=ncfile))
        ADV <- ncdf2adv(ncfile)
        expect_equal(adv@data, ADV@data)
        expect_equal(adv@metadata, ADV@metadata)
        file.remove(ncfile)
    })

