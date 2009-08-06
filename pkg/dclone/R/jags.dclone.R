jags.dclone <- 
function(data, params, model, n.clones, multiply=NULL, unchanged=NULL, 
update=NULL, updatefun=NULL,
trace=1, stop.if.converged=TRUE, ...)
{
    if (identical(n.clones, 1))
        stop("'n.clones = 1' gives the Bayesian answer, no need for DC")
    if (!is.null(update) != !is.null(updatefun))
        stop("both 'update' and 'updatefun' must be provided")
    k <- n.clones[order(n.clones)]
    times <- length(k)
    crit <- getOption("dclone.crit")
    converged <- FALSE
    ## dctable template
    dctc <- matrix(0, times, 3)
    colnames(dctc) <- c("n.clones", "lambda.max", "p.shapiro")
    dctc[,1] <- k
    ## internal function to calculate statistics
    dctsfun <- function(x) {
        y <- report(x, array)
        if (nch > 1) {
            rhat <- gelman.diag(x)$psrf[,1]
            rval <- rbind(mean = apply(y, 2, mean),
                sd = apply(y, 2, sd),
                apply(y, 2, quantile, probs=quantiles),
                r.hat=rhat)
        } else {
            rval <- rbind(mean = apply(y, 2, mean),
                sd = apply(y, 2, sd),
                apply(y, 2, quantile, probs=quantiles))
        }
        t(rval)
    }
    ## evaluate updating
    if (!is.null(update)) {
        unchanged <- c(unchanged, update)
        updatefun <- match.fun(updatefun)
    }
    for (i in 1:times) {
        if (trace)
            cat("\nFitting model with", k[i], "clones\n\n")
        jdat <- dclone(data, k[i], multiply=multiply, unchanged=unchanged)
        mod <- jags.fit(jdat, params, model, ...)
        ## dctable evaluation
        if (i == 1) {
            vn <- varnames(mod)
            nch <- nchain(mod)
            dcts <- list()
            quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)
            extracol <- if (nch > 1)
                4 else 3
            dcts0 <- matrix(0, times, extracol + length(quantiles))
            dcts0[,1] <- k
            colntmp <- c("n.clones", "mean", "sd", names(quantile(0, probs=quantiles)))
            colnames(dcts0) <- if (nch > 1)
                c(colntmp, "r.hat") else colntmp
            for (j in 1:length(vn))
                dcts[[vn[j]]] <- dcts0
        } else {
            if (!is.null(update))
                jdat[[update]] <- updatefun(mod)
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
#            tmp1 <- paste(ifelse(lmax < crit[1], "<", ">="), "critical", crit[1])
#            tmp2 <- paste(ifelse(pshw > crit[2], ">", "<="), "critical", crit[2])
#            cat("\nlambda.max", format(lmax), tmp1)
#            cat("\nShapiro-Wilk p-value =", format(pshw), tmp2, "\n")
            cat("\nlambda.max", format(lmax))
            cat("\nShapiro-Wilk p-value =", format(pshw), "\n")
            if (nch > 1)
                cat("R.hat =", format(dctmp[,"r.hat"]), "\n")
        }
        if (lmax < crit[1] && pshw > crit[2]) {
            converged <- TRUE
            if (trace)
                cat("\nConvergence reached with", k[i], "clones\n\n")
        } else cat("\nNo convergence reached with", k[i], "clones\n\n")
        if (converged && stop.if.converged)
            break
    }
    if (nch > 1 && any(dctmp[,"r.hat"] >= crit[3]))
        warning("chains convergence problem, see R.hat values")
    if (times > 1) {
        dctc <- as.data.frame(dctc[1:i,])
        dcts <- lapply(dcts, function(z) as.data.frame(z[1:i,]))
    } else {
        dctc <- as.data.frame(dctc)
        dcts <- lapply(dcts, function(z) as.data.frame(z))
    }
    dctable <- list(convergence = dctc, statistics = dcts)
    class(dctable) <- "dctable"
    attr(mod, "converged") <- converged
    attr(mod, "dctable") <- dctable
    mod
}
