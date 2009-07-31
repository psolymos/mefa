.First.lib <- function(lib, pkg){
    cat("This is dclone ", utils::packageDescription("dclone", field="Version"),
    " (", utils::packageDescription("dclone", field="Date"), ")\n", sep="")
    if (is.null(getOption("dclone.crit"))) {
        options("dclone.crit"=c(lmax=0.05, pshw=0.05))
    }
}
## todo
##
## * 1 param case: shapiro.test, unscaled SD
##   (plus rename mshapiro and mstp) -- done
## * rename report???
## * rename jags.fit.dclone to dcjags or jags.dclone
##   (similarly dcbugs/bugs.dclone in future)
## * develop spts class for space-time series
##   and dclone.spts, dclone.ts (??? mspts, mts)
## * inits function
## * data$priors and priors function
## * dctable: $convergence & $statistics
##   within $statistics list by parameters
##   (k * stats matrix)