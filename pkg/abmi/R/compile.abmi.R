compile.abmi <- function(file, dir) {
    opb <- pboptions(type="tk", title="Compiling ABMI Data in Progress")
    fnam <- file
    dir.in <- paste(dir, "/", "data", sep="")
    dir.out <- paste(dir, "/", "out", sep="")
    odir <- getwd()
    setwd(dir)
    unzip(fnam, exdir="data")
    dir.create(dir.out)
    ## get input file list
    files <- list.files(dir.in)
    files <- files[substr(files, nchar(files)-2, nchar(files)) == "csv"]
    Names <- sapply(strsplit(files, "_"), function(z) paste(z[-length(z)], collapse="_"))
    ##
    check <- c(" ... OK\n", " ... ERROR\n")
    ## use options
    options("abmi"=list(dir=dir, dir.in=dir.in, dir.out=dir.out, check=check,
        files=files, names=Names))

    ## birds
    if("RAW_T26BreedingBirds" %in% Names) {
        Path <- file.path(system.file(package = "abmi"), "scripts", "SCR_T26BreedingBirds.R")
        source(Path)
    }

    ## mammals
    if("RAW_T27BWinterSnowtrackingSpecies" %in% Names) {
        Path <- file.path(system.file(package = "abmi"), "scripts", "SCR_T27BWinterSnowtrackingSpecies.R")
        source(Path)
    }


    ## how to keep track of what has been done?? high level log?
    ## also: keep track of things on screen with flush screen?

    ## what to return nere? what was looked for and what was done, etc
    setwd(odir)
    pboptions(opb)
    options("abmi"=NULL)
    invisible(NULL)
}
