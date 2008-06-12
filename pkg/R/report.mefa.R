report <- 
function(...)
{
    UseMethod("report")
}

`report.mefa` <-
function (mf, filename, n = 1, ordering = "species", biotic.data = 1, species.name = 1, 
    species.order = 1, author = 0, tex = FALSE, binary = FALSE, 
    sep = c(", ", " (", ")", "; ")) 
{
    if(class(table)=="mflist") mf <- as.mefa(mf,n)
    if (class(mf) != "mefa") 
        stop("not 'mefa' class")
    if (sum(is.element(c("species", "samples"), ordering)) == 0) 
        stop("Specify 'ordering' parameter.")
    if (length(biotic.data) > 6) 
        stop("More than 6 columns selected.")
    if (length(sep) != 4) 
        stop("Specify exactly 4 separators.")
loca <- mf$sample.attr[, biotic.data]
loc<-loca[do.call(order, loca), ]
    xcr <- mf$data[do.call(order, loca), order(mf$species.attr[, species.order])]
    nam <- as.vector(mf$species.attr)[, species.name][order(mf$species.attr[, 
        species.order])]
    if (author != 0) 
        autv <- as.vector(mf$species.attr)[, author][order(mf$species.attr[, 
            species.order])]
    ti <- "\textit{"
    if (ordering == "species") {
        zz <- file(filename, "w")
        cat("%Start writing data from 'mefa' object sorted by species into file '", 
            filename, "' on ", date(), ".", file = zz, sep = "")
        for (spec in 1:length(nam)) {
            ifelse(author == 0, aut <- "", aut <- as.vector(autv[spec]))
            loc.sub <- as.matrix(subset(loc, xcr[, spec] > 0))
            xcr.sub <- as.vector(subset(xcr[, spec], xcr[, spec] > 
                0))
            cat("\n\n\n", file = zz, sep = "")
            if (tex == TRUE) {
                cat(gsub("par", "\\\\par", "paragraph{"), file = zz, 
                  sep = "")
                cat(encodeString(ti), file = zz, sep = "")
            }
            cat(as.character(nam[spec]), file = zz, sep = "")
            if (tex == TRUE) 
                cat("}", file = zz, sep = "")
            if (author != 0) 
                cat(" ", as.character(aut), file = zz, sep = "")
            if (tex == TRUE) 
                cat("}", file = zz, sep = "")
            cat("\n\n", file = zz, sep = "")
            for (samp in 1:dim(loc.sub)[1]) {
                locv <- loc.sub[samp, ]
                xcrd <- xcr.sub[samp]
                if (samp < dim(loc.sub)[1]) {
                  cat(t(locv), file = zz, sep = sep[1])
                  if (binary == TRUE) {
                    cat(sep[4], file = zz, sep = "")
                  }
                  else {
                    cat(sep[2], xcrd, sep[3], sep[4], file = zz, 
                      sep = "")
                  }
                  if (tex == TRUE) 
                    cat("\n", file = zz, sep = "")
                }
                else {
                  cat(t(locv), file = zz, sep = sep[1])
                  if (binary == TRUE) {
                    cat(".", file = zz, sep = "")
                  }
                  else {
                    cat(sep[2], xcrd, sep[3], ".", file = zz, 
                      sep = "")
                  }
                }
            }
        }
        cat("\n\n%End of output.", file = zz, sep = "\n")
        close(zz)
    }
    if (ordering == "samples") {
        zz <- file(filename, "w")
        cat("%Start writing data from 'mefa' object sorted by samples into file '", 
            filename, "' on ", date(), ".", file = zz, sep = "")
        xcr <- t(xcr)
        for (samp in 1:dim(loc)[1]) {
            xcr.sub <- as.vector(subset(xcr[, samp], xcr[, samp] > 0))
            nam.sub <- as.vector(subset(nam, xcr[, samp] > 0))
            ifelse(author == 0, aut <- "", aut <- as.vector(subset(autv, 
                xcr[, samp] > 0)))
            locv <- t(loc[samp, ])
            cat("\n\n\n", file = zz, sep = "")
            if (tex == TRUE) 
                cat(gsub("par", "\\\\par", "paragraph{"), file = zz, 
                  sep = "")
            cat(locv, file = zz, sep = sep[1])
            if (tex == TRUE) 
                cat("}\n", file = zz, sep = "")
            cat("\n\n", file = zz, sep = "")
            if (sum(xcr.sub) == 0) {
                cat("No species were found.", file = zz, sep = "")
            }
            else {
                for (spec in 1:length(nam.sub)) {
                  xcrd <- xcr.sub[spec]
                  if (spec < length(nam.sub)) {
                    if (tex == TRUE) 
                      cat(encodeString(ti), file = zz, sep = "")
                    cat(as.character(nam.sub[spec]), file = zz, 
                      sep = "")
                    if (tex == TRUE) 
                      cat("}", file = zz, sep = "")
                    if (author != 0) 
                      cat(" ", as.character(aut[spec]), file = zz, 
                        sep = "")
                    if (binary == TRUE) {
                      cat(sep[4], file = zz, sep = "")
                    }
                    else {
                      cat(sep[2], xcrd, sep[3], sep[4], file = zz, 
                        sep = "")
                    }
                    if (tex == TRUE) 
                      cat("\n", file = zz, sep = "")
                  }
                  else {
                    if (tex == TRUE) 
                      cat(encodeString(ti), file = zz, sep = "")
                    cat(as.character(nam.sub[spec]), file = zz, 
                      sep = "")
                    if (tex == TRUE) 
                      cat("}", file = zz, sep = "")
                    if (author != 0) 
                      cat(" ", as.character(aut[spec]), file = zz, 
                        sep = "")
                    if (binary == TRUE) {
                      cat(".", file = zz, sep = "")
                    }
                    else {
                      cat(sep[2], xcrd, sep[3], ".", file = zz, 
                        sep = "")
                    }
                  }
                }
            }
        }
        cat("\n\n%End of output.", file = zz, sep = "\n")
        close(zz)
    }
}
