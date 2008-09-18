`accumulate` <-

function(x){

tabl <- x$data[order(x$data$segment),]
nyr <- length(levels(tabl$segment))

nsamp <- newsamp <- nrecord <- nspec <- newspec <- rep(0, nyr)
first <- tabl[which(tabl$segment == levels(tabl$segment)[1]),]
first[] <- lapply(first, function(x) x[drop = TRUE])
nrecord[1] <- sum(first$count)
nspec[1] <- newspec[1] <- nrow(first)
oldlist1 <- first$species
oldlist2 <- first$sample

for (i in 2:nyr){
    second <- tabl[which(tabl$segment == levels(tabl$segment)[i]),]
    second[] <- lapply(second, function(x) x[drop = TRUE])
    nrecord[i] <- sum(second$count)
    newlist1 <- union(oldlist1, levels(second$species))
    nspec[i] <- nlevels(as.factor(newlist1))
    newspec[i] <- nspec[i] - nspec[(i-1)]
    oldlist1 <- newlist1
    newlist2 <- union(oldlist2, levels(second$sample))
    nsamp[i] <- nlevels(as.factor(newlist2))
    newsamp[i] <- nsamp[i] - nsamp[(i-1)]
    oldlist2 <- newlist2
    }

out <- data.frame(as.numeric(levels(tabl$segment)), nrecord, newsamp, newspec, cumsum(nrecord), nsamp, nspec)
colnames(out) <- c("segment", "newrecord", "newsamp", "newspec", "cumrecord", "cumsamp", "cumspec")
return(out)}

