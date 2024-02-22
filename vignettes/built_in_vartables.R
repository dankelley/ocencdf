## ----eval=FALSE---------------------------------------------------------------
#  system.file("extdata", "adp.yml", package="ocencdf")

## ----echo=FALSE---------------------------------------------------------------
lines <- readLines(system.file("extdata", "adp.yml", package="ocencdf"))
for (line in lines)
    cat(line, "\n")

## ----eval=FALSE---------------------------------------------------------------
#  system.file("extdata", "adv.yml", package="ocencdf")

## ----echo=FALSE---------------------------------------------------------------
lines <- readLines(system.file("extdata", "adv.yml", package="ocencdf"))
for (line in lines)
    cat(line, "\n")

## ----eval=FALSE---------------------------------------------------------------
#  system.file("extdata", "argo.yml", package="ocencdf")

## ----echo=FALSE---------------------------------------------------------------
lines <- readLines(system.file("extdata", "argo.yml", package="ocencdf"))
for (line in lines)
    cat(line, "\n")

## ----eval=FALSE---------------------------------------------------------------
#  system.file("extdata", "whp.yml", package="ocencdf")

## ----echo=FALSE---------------------------------------------------------------
lines <- readLines(system.file("extdata", "whp.yml", package="ocencdf"))
for (line in lines)
    cat(line, "\n")

