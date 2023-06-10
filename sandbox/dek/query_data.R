library(oce)
data(adp)
for (name in names(adp@data)) {
    x <- adp@data[[name]]
    if (is.array(x))
        cat("* ", name, ": ", class(x[1,1,1]), " array, dimension ", paste(dim(x), collapse="x"), "\n", sep="")
    else
        cat("* ", name, ": ", class(x), " vector, length ", length(x), "\n", sep="")
}
