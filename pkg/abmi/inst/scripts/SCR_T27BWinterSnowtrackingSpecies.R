## mammals

## open connection for log file
zz <- file(paste(getOption("abmi")$dir.out, "/", "OUT_T27BWinterSnowtrackingSpecies", ".log", sep=""), "w")
cat2("* Start log file on ", date(), " - OK\n", file=zz, sep="")

cat2("* Reading file:\n  ", getOption("abmi")$files[getOption("abmi")$names == "RAW_T27BWinterSnowtrackingSpecies"], file = zz, sep = "")
fmamm <- paste(getOption("abmi")$dir.in, "/", getOption("abmi")$files[getOption("abmi")$names == "RAW_T27BWinterSnowtrackingSpecies"], sep="")
cc <- c("factor","integer","integer","factor","factor","integer","numeric","numeric","factor","numeric","factor","numeric",
    "factor","factor","factor","integer","integer")
Mamm <- try(read.abmi(fmamm, colClasses=cc))
cat2(getOption("abmi")$check[inherits(Mamm, "try-error")+1], file = zz, sep = "")

cat2("* Adding Year.Site.TransectSegment ID", file = zz, sep = "")
id <- try(interaction(Mamm$Year, Mamm$ABMI.Site, Mamm$Transect.Segment))
Mamm <- data.frame(id=id, Mamm)
cat2(getOption("abmi")$check[inherits(id, "try-error")+1], file = zz, sep = "")

## aggregation on PC level

cat2("* Aggregating variables at Transect Segment (Year.Site.TS) level\n  using first values", file = zz, sep = "")
MammVars <- try(keepfirst(Mamm, Mamm$id))

## excluding variables that are related to taxa identification or duration
MammVars$Common.Name <- NULL
MammVars$Scientific.Name <- NULL
MammVars$Taxonomic.Resolution <- NULL
MammVars$Unique.Taxonomic.Identification.Number <- NULL
MammVars$Track.Orientation <- NULL

cat2(getOption("abmi")$check[inherits(MammVars, "try-error")+1], file = zz, sep = "")

## Site level aggregation of variables

cat2("* Adding Year.Site ID", file = zz, sep = "")
id2 <- try(interaction(MammVars$Year, MammVars$ABMI.Site))
MammVars$id2 <- id2
MammVars <- MammVars[order(MammVars$Year, MammVars$ABMI.Site, MammVars$Transect.Segment),]
cat2(getOption("abmi")$check[inherits(id2, "try-error")+1], file = zz, sep = "")

cat2("* Aggregating variables at site (Year.Site) level using\n  smallest Transect Segment IDs", file = zz, sep = "")
MammVars2 <- try(keepfirst(MammVars,MammVars$id2))
MammVars2$Percentage.of.Primary.Habitat <- NULL
MammVars2$Percentage.of.Secondary.Habitat <- NULL
MammVars2$id <- MammVars2$id2
MammVars2$id2 <- NULL

MammVars2$Transect.Segment.mean <- aggregate.abmi(MammVars$Transect.Segment, MammVars$id2, max, MammVars2$id)
MammVars2$Transect.Distance.metres.meandiff <- aggregate.abmi(MammVars$Transect.Distance.metres, MammVars$id2, 
    function(z) mean(diff(z)), MammVars2$id)
MammVars2$Primary.Habitat.freq <- aggregate.abmi(MammVars$Primary.Habitat, MammVars$id2, "freq", MammVars2$id)
MammVars2$Secondary.Habitat.freq <- aggregate.abmi(MammVars$Secondary.Habitat, MammVars$id2, "freq", MammVars2$id)
MammVars2$Transect.Segment <- NULL
MammVars2$Transect.Distance.metres <- NULL
MammVars2$Primary.Habitat <- NULL
MammVars2$Secondary.Habitat <- NULL

cat2(getOption("abmi")$check[inherits(MammVars2, "try-error")+1], file = zz, sep = "")


## crosstabs at PC level

cat2("* Collecting taxonomic information", file = zz, sep = "")
tax <- keepfirst(Mamm[,c("Unique.Taxonomic.Identification.Number",
    "Common.Name","Scientific.Name","Taxonomic.Resolution")], 
    Mamm$Unique.Taxonomic.Identification.Number)
tax <- tax[!is.na(tax[,1]),]
cat2(getOption("abmi")$check[inherits(tax, "try-error")+1], file = zz, sep = "")

cat2("* Cross tabulating species: ID x TSNID", file = zz, sep = "")
## Track.Orientation can be used as segment...
s <- Mamm[c("id", "Unique.Taxonomic.Identification.Number")]
levels(s[,2]) <- c(levels(s[,2]), "zero.pseudo")
s[is.na(s[,2]),2] <- "zero.pseudo"
s$count <- ifelse(s[,2]=="zero.pseudo", 0, 1)
s <- suppressWarnings(stcs(s))
m <- try(suppressWarnings(mefa(s, MammVars, tax, 1, 1)))
cat2(getOption("abmi")$check[inherits(m, "try-error")+1], file = zz, sep = "")

## crosstabs at site level

cat2("* Aggregating species cross table to site (Year.Site) level", file = zz, sep = "")
m2 <- try(aggregate(m, by.samp="id2"))
m2 <- mefa(m2$xtab, MammVars2, id.samp=1)
cat2(getOption("abmi")$check[inherits(m2, "try-error")+1], file = zz, sep = "")

## writing tables into files

fn0 <- "OUT_T27BWinterSnowtrackingSpecies_Variables_TS.csv"
cat2("* Writing variables (TS level) into file:\n  ", fn0, file = zz, sep = "")
write.csv(m$samp, file=paste(getOption("abmi")$dir.out
, "/", fn0, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn0 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

fn1 <- "OUT_T27BWinterSnowtrackingSpecies_Variables_Site.csv"
cat2("* Writing variables (Site level) into file:\n  ", fn1, file = zz, sep = "")
write.csv(m2$samp, file=paste(getOption("abmi")$dir.out
, "/", fn1, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn1 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

fn2 <- "OUT_T27BWinterSnowtrackingSpecies_Species_TS.csv"
cat2("* Writing species cross-table (TS level) into file:\n  ", fn2, file = zz, sep = "")
write.csv(m$xtab, file=paste(getOption("abmi")$dir.out
, "/", fn2, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn2 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

fn3 <- "OUT_T27BWinterSnowtrackingSpecies_Species_Site.csv"
cat2("* Writing species cross-table (Site level) into file:\n  ", fn3, file = zz, sep = "")
write.csv(m2$xtab, file=paste(getOption("abmi")$dir.out
, "/", fn3, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn3 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

fn4 <- "OUT_T27BWinterSnowtrackingSpecies_Taxa.csv"
cat2("* Writing taxonomic information into file:\n  ", fn4, file = zz, sep = "")
write.csv(m$taxa, file=paste(getOption("abmi")$dir.out
, "/", fn4, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn4 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

## close connection for log file
cat2("* End log file on ", date(), " - OK\n", file = zz, sep = "")
close(zz)


## issues: aggregate.abmi and keepfirst must exclude NAs !!!
