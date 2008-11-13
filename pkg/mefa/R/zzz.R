.First.lib <- function(lib, pkg){
    cat("This is mefa ", utils::packageDescription("mefa", field="Version"),
    " (", utils::packageDescription("mefa", field="Date"), ")\n", sep="")
}
