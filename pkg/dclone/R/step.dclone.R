step.dclone <- 
function(data, params, model, k, multiply=NULL, unchanged=NULL, trace=1, ...)
{
    if (length(k) < 2 || any(k < 2))
        stop("length of 'k' should be > 2")
    if (any(k < 2))
        stop("values for 'k' < 2 are not adequate")
    k <- k[order(k)]
    times <- length(k)
    crit <- getOption("dclone.crit")
    converged <- FALSE
    for (i in 1:times) {
        if (trace)
            cat("\nFitting model with", k[i], "clones\n\n")
        jdat <- dclone(data, k[i], multiply=multiply, unchanged=unchanged)
        mod <- jags.fit(jdat, params, model, ...)
        lmax <- lambdamax.diag(mod)
        mstp <- mshapiro.diag(mod)$p.value
        if (trace) {
            tmp1 <- paste(ifelse(lmax < crit[1], "<", ">="), "critical", crit[1])
            tmp2 <- paste(ifelse(mstp > crit[2], ">", "<="), "critical", crit[2])
            cat("\nlambda.max", round(lmax, 4), tmp1)
            cat("\nmshapiro p-value", round(mstp, 4), tmp2, "\n")
        }
        if (lmax < crit[1] && mstp > crit[2]) {
            converged <- TRUE
            if (trace)
                cat("\nConvergence reached with", k[i], "clones\n\n")
        } else cat("\nNo convergence reached with", k[i], "clones\n\n")
        if (converged)
            break
    }
    attr(mod, "converged") <- converged
    mod
}
