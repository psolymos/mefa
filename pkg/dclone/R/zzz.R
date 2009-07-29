.First.lib <- function(lib, pkg){
    cat("This is dclone ", utils::packageDescription("dclone", field="Version"),
    " (", utils::packageDescription("dclone", field="Date"), ")\n", sep="")
    if (is.null(getOption("dclone.crit"))) {
        options("dclone.crit"=c(lmax=0.1, mstp=0.1))
    }
}
