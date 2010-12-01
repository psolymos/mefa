## T27AWinterSnowTrackingSiteSnowCover

zz <- file(paste(getOption("abmi")$dir.out, "/", "OUT_T27AWinterSnowTrackingSiteSnowCover", ".log", sep=""), "w")
cat2("* Start log file on ", date(), " - OK\n", file=zz, sep="")

cat2("* Reading file:\n  ", getOption("abmi")$files[getOption("abmi")$names == "RAW_T27AWinterSnowTrackingSiteSnowCover"], file = zz, sep = "")
fmamm2 <- paste(getOption("abmi")$dir.in, "/", getOption("abmi")$files[getOption("abmi")$names == "RAW_T27AWinterSnowTrackingSiteSnowCover"], sep="")
MammSnow <- try(read.abmi(fmamm2))
cat2(getOption("abmi")$check[inherits(MammSnow, "try-error")+1], file = zz, sep = "")

cat2("* Adding Year.Site.TransectSegment ID", file = zz, sep = "")
id <- try(interaction(MammSnow$Year, MammSnow$ABMI.Site))
MammSnow <- data.frame(id=id, MammSnow)
cat2(getOption("abmi")$check[inherits(MammSnow, "try-error")+1], file = zz, sep = "")

cat2("* Calculating decimal time of day from Start Time", file = zz, sep = "")
dt <- strsplit(as.character(MammSnow$Start.Time), ":")
hour <- as.numeric(sapply(dt, function(z) z[1]))
mins <- as.numeric(sapply(dt, function(z) z[2]))
Start.Time.Decimal <- try((hour*60+mins) / 60)
MammSnow$Start.Time.Decimal <- Start.Time.Decimal
cat2(getOption("abmi")$check[inherits(Start.Time.Decimal, "try-error")+1], file = zz, sep = "")

cat2("* Calculating decimal time of day from End Time", file = zz, sep = "")
dt <- strsplit(as.character(MammSnow$End.Time), ":")
hour <- as.numeric(sapply(dt, function(z) z[1]))
mins <- as.numeric(sapply(dt, function(z) z[2]))
End.Time.Decimal <- try((hour*60+mins) / 60)
MammSnow$End.Time.Decimal <- End.Time.Decimal
cat2(getOption("abmi")$check[inherits(End.Time.Decimal, "try-error")+1], file = zz, sep = "")

fn <- "OUT_T27AWinterSnowTrackingSiteSnowCover_Site.csv"
cat2("* Writing taxonomic information into file:\n  ", fn, file = zz, sep = "")
write.csv(MammSnow, file=paste(getOption("abmi")$dir.out
, "/", fn, sep=""))
cat2(getOption("abmi")$check[as.integer(!(fn %in% list.files(getOption("abmi")$dir.out
)))+1], file = zz, sep = "")

cat2("* End log file on ", date(), " - OK\n", file = zz, sep = "")
close(zz)

