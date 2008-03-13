`accumulate` <-
function(x){

if (class(x) != "sscount") 
        stop("Object '", x, "' is not of 'sscount' class.")

tabl <- x$data[order(x$data$segment),]
nyr <- length(levels(tabl$segment))

nrecord <- nspec <- newspec <- rep(0, nyr)
first <- tabl[which(tabl$segment == levels(tabl$segment)[1]),]
first[] <- lapply(first, function(x) x[drop = TRUE])
nrecord[1] <- sum(first$count)
nspec[1] <- newspec[1] <- nrow(first)
oldlist <- first$species

for (i in 2:nyr){
    second <- tabl[which(tabl$segment == levels(tabl$segment)[i]),]
    second[] <- lapply(second, function(x) x[drop = TRUE])
    nrecord[i] <- sum(second$count)
    newlist <- union(oldlist, levels(second$species))
    nspec[i] <- nlevels(as.factor(newlist))
    newspec[i] <- nspec[i] - nspec[(i-1)]
    oldlist <- newlist
    }

out <- data.frame(as.numeric(levels(tabl$segment)), nrecord, newspec, cumsum(nrecord), nspec)
colnames(out) <- c("segment", "newrecord", "newspec", "cumrecord", "cumspec")
return(out)}

