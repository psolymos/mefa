jags.dclone <- 
function(data, params, model, n.clones, multiply=NULL, unchanged=NULL, trace=1, ...)
{
    if (any(n.clones < 2))
        stop("provide values > 1 for 'n.clones'")
    k <- n.clones[order(n.clones)]
    times <- length(k)
    crit <- getOption("dclone.crit")
    converged <- FALSE
    ## dctable template
    dctc <- matrix(0, times, 3)
    colnames(dctc) <- c("n.clones", "lambda.max", "p.shapiro")
    dctc[,1] <- k
    dcts <- list()
    quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)
    dcts0 <- matrix(0, times, 3 + length(quantiles))
    dcts0[,1] <- k
    colnames(dcts0) <- c("n.clones", "mean", "sd", names(quantile(0, probs=quantiles)))
    ## internal function to calculate statistics
    dctsfun <- function(x) {
        y <- report(x, array)
        rval <- rbind(mean = apply(y, 2, mean),
            sd = apply(y, 2, sd),
            apply(y, 2, quantile, probs=quantiles))
        t(rval)
    }
    for (i in 1:times) {
        if (trace)
            cat("\nFitting model with", k[i], "clones\n\n")
        jdat <- dclone(data, k[i], multiply=multiply, unchanged=unchanged)
        mod <- jags.fit(jdat, params, model, ...)
        ## dctable evaluation
        if (i == 1) {
            vn <- varnames(mod)
            for (j in 1:length(vn))
                dcts[[vn[j]]] <- dcts0
        }
        dctmp <- dctsfun(mod)
        for (j in 1:length(vn)) {
            dcts[[j]][i,-1] <- dctmp[j,]
        }
        lmax <- lambdamax.diag(mod)
        dctc[i,2] <- lmax
        pshw <- shapiro.diag(mod)$p.value
        dctc[i,3] <- pshw
        if (trace > 1) {
            tmp1 <- paste(ifelse(lmax < crit[1], "<", ">="), "critical", crit[1])
            tmp2 <- paste(ifelse(pshw > crit[2], ">", "<="), "critical", crit[2])
            cat("\nlambda.max", format(lmax), tmp1)
            cat("\nShapiro-Wilk p-value", format(pshw), tmp2, "\n")
        }
        if (lmax < crit[1] && pshw > crit[2]) {
            converged <- TRUE
            if (trace)
                cat("\nConvergence reached with", k[i], "clones\n\n")
        } else cat("\nNo convergence reached with", k[i], "clones\n\n")
        if (converged)
            break
    }
    dcts <- lapply(dcts, function(z) as.data.frame(z[1:i,]))
    dctable <- list(convergence = as.data.frame(dctc[1:i,]), statistics = dcts)
    class(dctable) <- "dctable"
    attr(mod, "converged") <- converged
    attr(mod, "dctable") <- dctable
    mod
}
