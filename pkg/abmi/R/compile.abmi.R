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

    ## collecting results
    res <- list()
    rescounter <- 1
    cat("*** ABMI Data Compilation Start ***")

    ## birds
    if("RAW_T26BreedingBirds" %in% Names) {
        cat("\n\n+++ T26BreedingBirds +++\n\n")
        Path <- file.path(system.file(package = "abmi"), "scripts", "SCR_T26BreedingBirds.R")
        source(Path)
        res[[rescounter]] <- "T26BreedingBirds"
        rescounter <- rescounter + 1
    }

    ## mammals
    if("RAW_T27BWinterSnowtrackingSpecies" %in% Names) {
        cat("\n\n+++ T27BWinterSnowtrackingSpecies +++\n\n")
        Path <- file.path(system.file(package = "abmi"), "scripts", "SCR_T27BWinterSnowtrackingSpecies.R")
        source(Path)
        res[[rescounter]] <- "T27BWinterSnowtrackingSpecies"
        rescounter <- rescounter + 1
    }
    ## mammal disturbance
    if("RAW_T27CWinterSnowTrackingSiteDisturbance" %in% Names) {
        cat("\n\n+++ T27CWinterSnowTrackingSiteDisturbance +++\n\n")
        Path <- file.path(system.file(package = "abmi"), "scripts", "SCR_T27CWinterSnowTrackingSiteDisturbance.R")
        source(Path)
        res[[rescounter]] <- "T27CWinterSnowTrackingSiteDisturbance"
        rescounter <- rescounter + 1
    }
    ## mammal snow
    if("RAW_T27AWinterSnowTrackingSiteSnowCover" %in% Names) {
        cat("\n\n+++ T27AWinterSnowTrackingSiteSnowCover +++\n\n")
        Path <- file.path(system.file(package = "abmi"), "scripts", "SCR_T27AWinterSnowTrackingSiteSnowCover.R")
        source(Path)
        res[[rescounter]] <- "T27AWinterSnowTrackingSiteSnowCover"
        rescounter <- rescounter + 1
    }

    ## what to return nere? what was looked for and what was done, etc
    setwd(odir)
    pboptions(opb)
    options("abmi"=NULL)
    cat("\n\n*** ABMI Data Compilation End ***\n\n")
    data.frame(precessed=unlist(res))
}
