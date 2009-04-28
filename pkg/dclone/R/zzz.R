.First.lib <- function(lib, pkg){
    cat("This is dclone ", utils::packageDescription("dclone", field="Version"),
    " (", utils::packageDescription("dclone", field="Date"), ")\n", sep="")
}
