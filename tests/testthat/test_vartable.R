# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
data(ctd, package="oce")

test_that("ctd2nc on data(ctd) creates a file with expected variable names",
    {
        varInfo <- getVarInfo("temperature", "argo")
        expect_equal(varInfo$name, "TEMP")
        expect_equal(varInfo$unit, "")
        varInfo <- getVarInfo("temperature2", "argo")
        expect_equal(varInfo$name, "TEMP2")
        expect_equal(varInfo$unit, "")
        varInfo <- getVarInfo("temperature", "argo", ctd)
        expect_equal(varInfo$name, "TEMP")
        expect_equal(varInfo$unit, "degree_Celcius")
    })

