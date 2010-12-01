## T27CWinterSnowTrackingSiteDisturbance

zz <- file(paste(getOption("abmi")$dir.out, "/", "OUT_T27CWinterSnowTrackingSiteDisturbance", ".log", sep=""), "w")
cat2("* Start log file on ", date(), " - OK\n", file=zz, sep="")

cat2("* Reading file:\n  ", getOption("abmi")$files[getOption("abmi")$names == "RAW_T27CWinterSnowTrackingSiteDisturbance"], file = zz, sep = "")
fmamm2 <- paste(getOption("abmi")$dir.in, "/", getOption("abmi")$files[getOption("abmi")$names == "RAW_T27CWinterSnowTrackingSiteDisturbance"], sep="")
MammDist <- try(read.abmi(fmamm2))
cat2(getOption("abmi")$check[inherits(MammDist, "try-error")+1], file = zz, sep = "")

cat2("* Adding Year.Site.TransectSegment ID", file = zz, sep = "")
id <- try(interaction(MammDist$Year, MammDist$ABMI.Site, MammDist$Transect.Segment))
id2 <- try(interaction(MammDist$Year, MammDist$ABMI.Site))
MammDist <- data.frame(id=id, id2=id2, MammDist)
cat2(getOption("abmi")$check[inherits(MammDist, "try-error")+1], file = zz, sep = "")

cat2("* Converting Transect Segment into integer (START=0)", file = zz, sep = "")
startid <- try(toupper(levels(MammDist$Transect.Segment)) %in% "START")
levels(MammDist$Transect.Segment)[startid] <- "0"
MammDist$Transect.Segment <- as.integer(as.character(MammDist$Transect.Segment))
cat2(getOption("abmi")$check[inherits(startid, "try-error")+1], file = zz, sep = "")

cat2("* Aggregating variables at site (Year.Site) level using\n  smallest Transect Segment IDs", file = zz, sep = "")
MammDist2 <- try(keepfirst(MammDist,MammDist$id2))
MammDist2$id <- MammDist2$id2
MammDist2$id2 <- NULL
cat2(getOption("abmi")$check[inherits(MammDist2, "try-error")+1], file = zz, sep = "")

cat2("* Aggregating Human Disturbance by freq", file = zz, sep = "")
Human.Disturbance.freq <- aggregate.abmi(MammDist$Human.Disturbance, MammDist$id2, "freq", MammDist2$id)
MammDist2$Human.Disturbance.freq <- Human.Disturbance.freq
MammDist2$Human.Disturbance <- NULL
cat2(getOption("abmi")$check[inherits(Human.Disturbance.freq, "try-error")+1], file = zz, sep = "")

fn1 <- "OUT_T27CWinterSnowTrackingSiteDisturbance_TS.csv"
cat2("* Writing taxonomic information into file:\n  ", fn1, file = zz, sep = "")
write.csv(MammDist, file=paste(getOption("abmi")$dir.out
, "/", fn1, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn1 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

fn2 <- "OUT_T27CWinterSnowTrackingSiteDisturbance_Site.csv"
cat2("* Writing taxonomic information into file:\n  ", fn2, file = zz, sep = "")
write.csv(MammDist2, file=paste(getOption("abmi")$dir.out
, "/", fn2, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn2 %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

cat2("* End log file on ", date(), " - OK\n", file = zz, sep = "")
close(zz)

