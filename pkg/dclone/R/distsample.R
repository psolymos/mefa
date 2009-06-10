distsample <-
function(xy, probs = seq(0, 1, 0.2), size = round(nrow(xy) / 10))
{
#    require(mefa)
    n <- nrow(xy)
    d <- dist(xy)
    attributes(d) <- NULL
    qd <- qvector(d, probs = probs)
    if (identical(probs, c(0, 1)) && size == n) {
        id <- rep(1, n)
    } else {
        id <- numeric(length(d))
        for (i in 1:length(unique(qd))) {
            id[sample((1:length(d))[qd == unique(qd)[i]], size = size)] <- 1
        }
    }
    did <- vec2dist(id, n)
    didm <- as.matrix(did)
    didm[upper.tri(didm)] <- 0
    diag(didm) <- 0
    rm <- row(didm)
    cm <- col(didm)
    rm <- array(rm)[array(didm) == 1]
    cm <- array(cm)[array(didm) == 1]
    out <- cbind(point1=cm, point2=rm, dist=d[id==1])
    rownames(out) <- rownames(xy)
    attr(out, "probs") <- probs
    attr(out, "size") <- size
    as.data.frame(out)
}

