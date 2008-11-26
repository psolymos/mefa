.First.lib <- function(lib, pkg){
    cat("This is abmi ", utils::packageDescription("abmi", field="Version"),
    " (", utils::packageDescription("abmi", field="Date"), ")\n", sep="")
}
