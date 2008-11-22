.First.lib <- function(lib, pkg){
    cat("This is abmi ", utils::packageDescription("mefa", field="Version"),
    " (", utils::packageDescription("mefa", field="Date"), ")\n", sep="")
}
