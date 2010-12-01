## birds

## open connection for log file
zz <- file(paste(getOption("abmi")$dir.out, "/", "OUT_T26BreedingBirds", ".log", sep=""), "w")
cat2("* Start log file on ", date(), " - OK\n", file=zz, sep="")

cat2("* Reading file:\n  ", getOption("abmi")$files[getOption("abmi")$names == "RAW_T26BreedingBirds"], file = zz, sep = "")
fbird <- paste(getOption("abmi")$dir.in, "/", getOption("abmi")$files[getOption("abmi")$names == "RAW_T26BreedingBirds"], sep="")
Birds <- try(read.abmi(fbird))
cat2(getOption("abmi")$check[inherits(Birds, "try-error")+1], file = zz, sep = "")

cat2("* Adding Year.Site.PointCount ID", file = zz, sep = "")
id <- try(interaction(Birds$Year, Birds$ABMI.Site, Birds$Point.Count.Station))
Birds <- data.frame(id=id, Birds)
cat2(getOption("abmi")$check[inherits(id, "try-error")+1], file = zz, sep = "")

WHAT.TO.EXCLUDE <- "nothing excluded"
cat2("* Excluding Behaviour types: ", WHAT.TO.EXCLUDE, file = zz, sep = "")
Birds <- try(Birds[!(Birds$Behaviour %in% WHAT.TO.EXCLUDE),])
cat2(getOption("abmi")$check[inherits(Birds, "try-error")+1], file = zz, sep = "")

## aggregation on PC level

cat2("* Calculating decimal time of day from\n  Start.of.Point.Count.24.hour.clock", file = zz, sep = "")
dt <- strsplit(as.character(Birds$Start.of.Point.Count.24.hour.clock), ":")
hour <- as.numeric(sapply(dt, function(z) z[1]))
mins <- as.numeric(sapply(dt, function(z) z[2]))
Start.of.Point.Count.Decimal <- try((hour*60+mins) / 60)
Birds$Start.of.Point.Count.Decimal <- Start.of.Point.Count.Decimal
cat2(getOption("abmi")$check[inherits(Start.of.Point.Count.Decimal, "try-error")+1], file = zz, sep = "")

cat2("* Aggregating variables at Point Count (Year.Site.PC) level\n  using first values", file = zz, sep = "")
#keep <- pbsapply(unique(as.character(Birds$id)), function(z) which(Birds$id==z)[1])
#BirdVars <- Birds[keep,]
BirdVars <- try(keepfirst(Birds, Birds$id))

## excluding variables that are related to taxa identification or duration
BirdVars$Identification.Date <- NULL
BirdVars$Identification.Analyst <- NULL
BirdVars$Common.Name <- NULL
BirdVars$Scientific.Name <- NULL
BirdVars$Taxonomic.Resolution <- NULL
BirdVars$Unique.Taxonomic.Identification.Number <- NULL
BirdVars$Time.First.Detected.seconds <- NULL
BirdVars$Interval.1.0.200.seconds <- NULL
BirdVars$Interval.2.201.400.seconds <- NULL
BirdVars$Interval.3.401.600.seconds <- NULL
BirdVars$Behaviour <- NULL
cat2(getOption("abmi")$check[inherits(BirdVars, "try-error")+1], file = zz, sep = "")

## Site level aggregation of variables

cat2("* Adding Year.Site ID", file = zz, sep = "")
id2 <- try(interaction(BirdVars$Year, BirdVars$ABMI.Site))
BirdVars$id2 <- id2
BirdVars <- BirdVars[order(BirdVars$Year, BirdVars$ABMI.Site, BirdVars$Point.Count.Station),]
cat2(getOption("abmi")$check[inherits(id2, "try-error")+1], file = zz, sep = "")

cat2("* Aggregating variables at site (Year.Site) level using\n  smallest Point Count IDs", file = zz, sep = "")
BirdVars2 <- try(keepfirst(BirdVars,BirdVars$id2))
cat2(getOption("abmi")$check[inherits(BirdVars2, "try-error")+1], file = zz, sep = "")

cat2("* Adding variable Number.of.Point.Counts at site (Year.Site) level", file = zz, sep = "")
Number.of.Point.Counts <- pbsapply(BirdVars2$id2, function(z) {
    length(BirdVars$Point.Count.Station[BirdVars$id2==z])
})
BirdVars2$Number.of.Point.Counts <- Number.of.Point.Counts
cat2(getOption("abmi")$check[inherits(Number.of.Point.Counts, "try-error")+1], file = zz, sep = "")
BirdVars2$id <- BirdVars2$id2
BirdVars2$id2 <- NULL

## crosstabs at PC level

cat2("* Collecting taxonomic information", file = zz, sep = "")
tax <- keepfirst(Birds[,c("Unique.Taxonomic.Identification.Number",
    "Common.Name","Scientific.Name","Taxonomic.Resolution")], 
    Birds$Unique.Taxonomic.Identification.Number)
tax <- tax[!is.na(tax[,1]),]
cat2(getOption("abmi")$check[inherits(tax, "try-error")+1], file = zz, sep = "")

cat2("* Cross tabulating species: ID x TSNID", file = zz, sep = "")
## Behaviour can be used as segment...
s <- Birds[c("id", "Unique.Taxonomic.Identification.Number")]
levels(s[,2]) <- c(levels(s[,2]), "zero.pseudo")
s[is.na(s[,2]),2] <- "zero.pseudo"
s$count <- ifelse(s[,2]=="zero.pseudo", 0, 1)
s <- suppressWarnings(stcs(s))
m <- try(suppressWarnings(mefa(s, BirdVars, tax, 1, 1)))
cat2(getOption("abmi")$check[inherits(m, "try-error")+1], file = zz, sep = "")

## crosstabs at site level

cat2("* Aggregating species cross table to site (Year.Site) level", file = zz, sep = "")
m2 <- try(aggregate(m, by.samp="id2"))
m2 <- mefa(m2$xtab, BirdVars2, id.samp=1)
cat2(getOption("abmi")$check[inherits(m2, "try-error")+1], file = zz, sep = "")

## writing tables into files

fn0 <- "OUT_T26BreedingBirds_Variables_PC.csv"
cat2("* Writing variables (PC level) into file:\n  ", fn0, file = zz, sep = "")
write.csv(m$samp, file=paste(getOption("abmi")$dir.out
, "/", fn0, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn0 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

fn1 <- "OUT_T26BreedingBirds_Variables_Site.csv"
cat2("* Writing variables (Site level) into file:\n  ", fn1, file = zz, sep = "")
write.csv(m2$samp, file=paste(getOption("abmi")$dir.out
, "/", fn1, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn1 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

fn2 <- "OUT_T26BreedingBirds_Species_PC.csv"
cat2("* Writing species cross-table (PC level) into file:\n  ", fn2, file = zz, sep = "")
write.csv(m$xtab, file=paste(getOption("abmi")$dir.out
, "/", fn2, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn2 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

fn3 <- "OUT_T26BreedingBirds_Species_Site.csv"
cat2("* Writing species cross-table (Site level) into file:\n  ", fn3, file = zz, sep = "")
write.csv(m2$xtab, file=paste(getOption("abmi")$dir.out
, "/", fn3, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn3 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

fn4 <- "OUT_T26BreedingBirds_Taxa.csv"
cat2("* Writing taxonomic information into file:\n  ", fn4, file = zz, sep = "")
write.csv(m$taxa, file=paste(getOption("abmi")$dir.out
, "/", fn4, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn4 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

## close connection for log file
cat2("* End log file on ", date(), " - OK\n", file = zz, sep = "")
close(zz)

## issues

# OK Joan fixed it database - csv line endings with , should be removed
# OK Joan fixed it database - place separator eg. _ before random number in filenames to get pattern RAW_abc_12345.csv
# is it a problem? - strange file name: RAW_T14%CoverShrubSpecies28587.csv percent sign???
# OK Peter fixed it in R - convert NAs
# fix stcs when count isnot given zero.pseudo fails!

