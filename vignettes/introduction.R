## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(ocencdf)
library(oce)
data(section)
stn <- section[["station", 100]]
summary(stn)
plot(stn)

## -----------------------------------------------------------------------------
oce2ncdf(stn, varTable="argo", ncfile="stn.nc")

## -----------------------------------------------------------------------------
STN <- ncdf2ctd("stn.nc", varTable="argo")
summary(STN)
plot(STN)

## ----echo=FALSE---------------------------------------------------------------
unlink("stn.nc")

