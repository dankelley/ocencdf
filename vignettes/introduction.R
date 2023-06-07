## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(ocencdf)
library(oce)
data(section)
stn <- section[["station", 100]]
summary(stn)

## -----------------------------------------------------------------------------
oce2ncdf(stn, varTable="argo", ncfile="stn.nc")

## -----------------------------------------------------------------------------
A <- ncdf2ctd("stn.nc", varTable="argo")
summary(A)

## ----echo=FALSE---------------------------------------------------------------
unlink("stn.nc")

