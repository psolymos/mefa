nestedboard <-
function(comm, board = "B")
{
    board <- match.arg(toupper(board), c("B", "T", "W", "C", "N"))
    bb <- switch(board,
        ## 1111 case -- o.k.
        "B" = sum(designdist(comm, "(J*J-J)/2", "binary")),
        ## 1010 and 0101 cases
        ## be careful, does not give the same for comm & t(comm)
        ## because not invariant to transposing
        "T" = sum(designdist(t(comm), "J*(P-A-B+J)", "binary")),
        ## 0000 case -- o.k.
        "W" = sum(designdist(comm, "(P-A-B+J)*(P-A-B+J-1)/2", "binary")),
        ## checkerboard
        "C" = sum(designdist(comm, "(A-J)*(B-J)", "binary")),
        ## all possible 2x2 -- o.k.
        "N" = (nrow(comm)*(nrow(comm)-1)/2) * ncol(comm)*(ncol(comm)-1)/2
    )
    sppairs <- ncol(comm) * (ncol(comm) - 1)/2
    out <- list(score = bb/sppairs, statistic = bb)
    attr(out, "board") <- board
    class(out) <- c("nestedboard")
    out
}

print.nestedboard <-
function (x, ...) 
{
    short <- attr(x, "board")
    long <- switch(short,
        "B" = "Blackboard Units:",
        "T" = "Togetherness Units:",
        "W" = "Whiteboard Units:",
        "C" = "Checkerboard Units:",
        "N" = "All 2x2 Submatrices:"
    )
    cat(long, format(x$statistic), "\n")
    cat(short, "-score (species mean): ", format(x$score), "\n", sep="")
    invisible(x)
}

#Example
#a <- matrix(c(1,1,0,0,1,0, 1,1,0,1,0,0, 0,0,0,0,1,1), 6, 3)
#nestedboard(a, "N")
#nestedboard(a, "B")
#nestedboard(a, "T")
#nestedboard(a, "W")
#stra <- c(1,1,1,2,2,2)

oddsboard <-
function(comm, strat, board = "C")
{
    ## evaluation of args
    if (missing(strat))
        stop("'strat' argument is missing")
    if (any(dim(comm) < 2))
        stop("dimensions not appropriate")
    if (length(strat) != nrow(comm))
        stop("'comm' and 'strat' must conform")
    if (board == "N")
        stop("'board' should not be 'N'")
    ## internal functions
    csc <- function(x) nestedboard(x, board)$statistic
    asc <- function(x) {
        nestedboard(x, "N")$statistic - nestedboard(x, board)$statistic
    }
    ## within strata C and N scores
    levs <- unique(strat)
    names(levs) <- levs
    if (any(sapply(levs, function(x) length(strat[strat==x])) == 1))
        stop("stratum with 1 observation is not allowed")
    nlevs <- length(levs)
    c.w <- sum(sapply(levs, function(x) csc(comm[strat==x,])))
    a.w <- sum(sapply(levs, function(x) asc(comm[strat==x,])))
    ## within strata C and N scores
    c.b <- csc(comm) - c.w
    a.b <- asc(comm) - a.w
    ## contingency table
    m <- c(c.b, c.w, a.b, a.w)
    Strata <- as.factor(rep(c("Bettween", "Within"), 2))
    Statistics <- as.factor(rep(1:2, each = 2))
    levels(Statistics) <- c(board, paste("N", board, sep="-"))
    out <- list(
        table = xtabs(m ~ Strata + Statistics),
        statistic = or <- (m[1] * m[4]) / (m[3] * m[2]))
    class(out) <- "oddsboard"
    attr(out, "call") <- match.call()
    attr(out, "board") <- board
    out
}

print.oddsboard <-
function(x, ...)
{
    board <- switch(attr(x, "board"),
        "B" = "Blackboard Units:",
        "T" = "Togetherness Units:",
        "W" = "Whiteboard Units:",
        "C" = "Checkerboard Units:"
    )
    cat("Partitioning of", board, "\n\n")
    print(x$table)
    cat("\nOdds ratio:", format(x$statistic), "\n")
    if (any(x$table == 0))
        cat("Result contain zero values, odds ratio may be incorrect\n")
    invisible(x)
}

oddsboardbyspec <-
function(comm, strat, board = "C")
{
    spp <- ncol(comm)
    id <- cbind(unlist(lapply(2:spp, function(z) z:spp)),
        rep(1:(spp-1), (spp-1):1))
    out <- sapply(1:nrow(id), function(z)
        oddsboard(comm[,id[z,]], strat, board)$statistic)
    class(out) <- "dist"
    attr(out, "Size") <- spp
    attr(out, "Diag") <- FALSE
    attr(out, "Upper") <- FALSE
    attr(out, "call") <- match.call()
    attr(out, "method") <- "oddsboardbyspec"
    out
}



