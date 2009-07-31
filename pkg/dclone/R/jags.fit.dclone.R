jags.fit.dclone <- 
function(data, params, model, n.clones, multiply=NULL, unchanged=NULL, trace=1, ...)
{
    if (any(n.clones < 2))
        stop("provide values > 1 for 'n.clones'")
    k <- n.clones[order(n.clones)]
    times <- length(k)
    dctable <- matrix(0, times, 3)
    colnames(dctable) <- c("n.clones", "lambda.max", "p.shapiro")
    dctable[,1] <- k
    crit <- getOption("dclone.crit")
    converged <- FALSE
    for (i in 1:times) {
        if (trace)
            cat("\nFitting model with", k[i], "clones\n\n")
        jdat <- dclone(data, k[i], multiply=multiply, unchanged=unchanged)
        mod <- jags.fit(jdat, params, model, ...)
        lmax <- lambdamax.diag(mod)
        dctable[i,2] <- lmax
        pshw <- shapiro.diag(mod)$p.value
        dctable[i,3] <- pshw
        if (trace) {
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
    attr(mod, "converged") <- converged
    attr(mod, "dctable") <- dctable
    mod
}
