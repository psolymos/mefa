wait <- function(vign=FALSE) {
  if (vign) {
    ANSWER <- readline("Do you want to open the vignette now? ")
    if (substr(tolower(ANSWER), 1, 1) == "y")
      vignette("mefa-vignette")
  } else {
    ANSWER <- readline("Please press ENTER to continue ... ")
  }
}

########################################################
###          Demo for the R package 'mefa'           ###
########################################################

### You can use the mefa vignette for reference.

wait(TRUE)

########################################################
###         Processing count data with 'mefa'        ###
########################################################

### loading 'mefa' library
library(mefa)

wait()
### loading count data to use
data(dol.count)
str(dol.count)

wait()
### notebook style count data

dol.count[1:10,]

wait()
### object of class 'sscount' based on dol.count

(ssc <- sscount(dol.count, zc="zero.count", fill=TRUE))

wait()
### with data filled up

ssc$data[1:10,]

wait()
### object of class 'xcount' based on ssc
### for all segments

(xc.all <- xcount(ssc))

wait()
### and for broken shells only

(xc.broken <- xcount(ssc, 2))

wait()
### 

plot(xc.all, col="red")

wait()
### 

plot(xc.all, type="rank", col="red")

wait()
### loading sample attribute table

data(dol.sample)
str(dol.sample)

wait()
### check compatibility of xc.all and dol.sample

check.attrib(xc.all, which = "samples", dol.sample, 1)

wait()
### object of class 'xorder' for sample attributes

(xo1 <- xorder(xc.all, which="samples", dol.sample, 1))

wait()
### loading species attribute table

data(landsnail)
str(landsnail)

wait()
### check compatibility of xc.all and landsnail

check.attrib(xc.all, which = "species", landsnail, 2)

wait()
### object of class 'xorder' for species attributes

(xo2 <- xorder(xc.all, which="species", landsnail, 2))

wait()
### object of class 'mefa' combining xc.all, xo1 and xo2

(mf <- mefa(xc.all, xo1, xo2))

wait()
### 

plot(mf, "microhabitat", "shell.dimension", col="red")

wait()
###

(mic <- strify(mf, "microhabitat", "samples"))

wait()
###

(fam <- strify(mf, "familia", "species"))

wait()
###

(mic.fam <- strify(
  as.xcount(mic), mf$species.attr$familia, "species")
)

wait()
###

mic.fam$data

wait()
###

(ex1 <- exclmf(mf, which = "samples", empty = TRUE, 
        excl = which(mf$sample.attr$microhabitat != "litter")))

wait()
###

plot(ex1, "replicate", type="b", col="red")

wait()
###

(ex2 <- exclmf(
  mic.fam, "species", c("Ellobiidae", "Endodontidae"), empty = TRUE)
)

wait()
### let's see the structure of a 'mefa' object

str(ex2)

wait()
########################################################
###      Data analysis based on 'mefa' objects       ###
########################################################

wait()
###*****************************************************
### What is the effect of microhabitat on species richness?

wait()
### linear modeling indicates:
### microhabitat effect is significant,
### richness is higher in the rock
### and dead wood microsites

summary(lm(mf$srichn ~ mf$sample.attr$microhabitat -1))

wait()
###*****************************************************
### What is the similarity relationship among the samples?

### hierarchical cluster dendrogram indicates:
### rock samples have the most distinct species composition

wait()
plot(
  hclust(dist(mf$data, "euclidean"), "ward"),
  sub="Euclidean distance, Ward's method",
  main="Cluster dendrogram of abundance data",xlab=""
)

wait()
###*****************************************************
### Is there any microhabitat preferences 
### for land snail families?

wait()
### the data

ex2$data

wait()
### the Chi-squared test indicates:
### the association is significant

chisq.test(ex2$data, simulate.p.value = TRUE, B = 2000)

wait()
###*****************************************************
### What is the effect of different life stages
### on interpretation of the results?

wait()
### making the 'broken' part

broken <- exclmf(
  strify(strify(xc.broken, mf$sample.attr$microhabitat, "samples"),
    mf$species.attr$familia, "species"),
    "species", c("Ellobiidae", "Endodontidae"))$data

wait()
### barplot for comparison

par(mfrow=c(1, 2), pty="s")
barplot(t(broken), horiz=TRUE, col=heat.colors(3), main="Broken", xlab="Frequency")
barplot(t(ex2$data), horiz = TRUE, col=heat.colors(3), main="Intact", xlab="Frequency", legend.text=TRUE)
par(mfrow=c(1,1))

wait()
########################################################
###      To use 'mefa' for reporting, see            ###
###      - help on the function report.mefa          ###
###      - and mefadocs("SampleReport")              ###
########################################################

wait()
###              End of 'mefa' demo

wait()
