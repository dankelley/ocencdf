# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# Test the scheme used to convert metadata contents to a JSON string, for
# storage in the NetCDF file as a global attribute named 'metadata'.

test_that("ctd metadata",
    {
        data(ctd, package="oce")
        m <- ctd@metadata
        j <- metadata2json(m)
        mj <- json2metadata(j)
        expect_equal(m, mj)
    })

test_that("adp metadata",
    {
        data(adp, package="oce")
        m <- adp@metadata
        j <- metadata2json(m)
        mj <- json2metadata(j)
        expect_equal(m, mj)
    })

