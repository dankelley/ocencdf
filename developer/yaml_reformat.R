library(yaml)
types <- c("argo", "whp")
for (type in types) {
    y <- yaml.load_file(paste0("../inst/extdata/", type, ".yml"))
    y <- y[order(names(y))]
    for (name in names(y)) {
        o <- order(names(y[[name]]))
        y[[name]] <- y[[name]][o]
    }
    write_yaml(y, paste0(type, ".yml"))
}
message("Hint: copy/paste the following into your terminal, to update.")
for (type in types)
    message(paste0("cp ", type, ".yml ../inst/extdata/"))
