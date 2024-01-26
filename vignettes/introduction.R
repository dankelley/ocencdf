## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(ocencdf)
library(oce)
data(section)
stn <- section[["station", 100]]
summary(stn)
plot(stn)

## -----------------------------------------------------------------------------
ncfile <- tempfile(pattern = "argo", fileext = ".nc")
oce2ncdf(stn, varTable = "argo", ncfile = ncfile)

## -----------------------------------------------------------------------------
STN <- ncdf2ctd(ncfile, varTable = "argo")
summary(STN)
plot(STN)

## ----echo=FALSE---------------------------------------------------------------
file.remove(ncfile)

