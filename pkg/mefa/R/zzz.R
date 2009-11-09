#.First.lib <- function(lib, pkg){
#    cat("This is mefa ", utils::packageDescription("mefa", field="Version"),
#    " (", utils::packageDescription("mefa", field="Date"), ")\n", sep="")
#}

.onAttach <- function(libname,pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    invisible(NULL)
}

.onUnload <- function(libpath){
    invisible(NULL)
}

