`mapmf` <-
function(xy, z, probs = seq(0, 1, 0.25), type="heat", pch=21, cex=1.5, ...){
plot(xy,type="n",axes=FALSE,xlab="",ylab="",...)
if(is.null(probs)) {
if(type=="heat") for(i in 1:length(z)) points(xy[i,1],xy[i,2],bg=heat.colors(max(z))[(max(z)-z[i]+1)], pch=pch, cex=cex, ...)
if(type=="grey") for(i in 1:length(z)) 
points(xy[i,1],xy[i,2],bg=grey((max(z)-z[i])/max(z)), pch=pch, cex=cex, ...)
    } else {
zq <- as.factor(qdef(z, probs))
nl <- nlevels(zq)
if(type=="heat") for(i in 1:nl) points(xy[which(zq==levels(zq)[i]),],bg=heat.colors(nl)[(nl-i+1)], pch=pch, cex=cex, ...)
if(type=="grey") for(i in 1:nl) points(xy[which(zq==levels(zq)[i]),],bg=grey(as.numeric(levels(zq)[(nl-i+1)])), pch=pch, cex=cex, ...)
}}

