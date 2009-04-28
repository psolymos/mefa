ftp.upload <-
function(dumplist, ..., ftp, file="results",
ext=if (binary) "Rdata" else "txt", binary=FALSE, dir=getwd())
{
    olddir <- getwd()
    setwd(dir)
    extension <- if (binary)
        "Rdata" else "txt"
    file <- paste(file, ext, sep=".")
    if (length(list(...)))
         dumplist <- list(dumplist, ...)

    if (binary) {
        if (is.character(dumplist))
            save(list=dumplist, file=file)
        if (is.list(dumplist))
            save(dumplist, file=file)
    } else dump(dumplist, file)
    ftpstring <- paste(ftp, file, sep="")
    curl <- RCurl::ftpUpload(file, ftpstring)
    setwd(olddir)
    invisible(curl)
}

