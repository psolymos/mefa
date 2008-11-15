## Sample ABMI data set for bird count
## Sample data is derived from 'RAW_T_BirdCounts*****.csv'
## data from Rotation 1, 2007, June
## original sourve downloaded from www.abmi.ca
## see terms of use at www.abmi.ca
RawBirdCountsSample <- read.abmi("RawBirdCountsSample.csv",
    dir=paste(.libPaths()[1], "/abmi/data", sep="", collapse=""))
