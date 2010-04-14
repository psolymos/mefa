# draw ellipse
draw1ellipse <-
function(x, y, a = 1, b = 1, angle = 0, segment=c(0, 360), n.points = 100, deg = TRUE, method = c("lines", "polygon"), ...)
{
    if (deg) {# if input is in degrees
        angle <- angle * pi/180
        segment <- segment * pi/180
    }
    z <- seq(segment[1], segment[2], length = n.points + 1)
    xx <- a * cos(z)
    yy <- b * sin(z)
    alpha <- xyangle(xx, yy, directed = TRUE, deg = FALSE)
    rad <- sqrt(xx^2 + yy^2)
    xp <- rad * cos(alpha + angle) + x
    yp <- rad * sin(alpha + angle) + y
    drawfun <- switch(match.arg(method),
        "lines"=lines, "polygon"=polygon)
    drawfun(xp, yp, ...)
    invisible(NULL)
}
drawellipse <-
function(x, y, a = 1, b = 1, angle = 0, n.points = 100, deg = TRUE, method = c("lines", "polygon"), col=1, ...)
{
    n <- length(x)
    if (length(a) < n)
        a <- rep(a, n)[1:n]
    if (length(b) < n)
        b <- rep(b, n)[1:n]
    if (length(angle) < n)
        angle <- rep(angle, n)[1:n]
    if (length(col) < n)
        col <- rep(col, n)[1:n]
    lapply(1:n, function(i) draw1ellipse(x[i], y[i], a[i], b[i], angle[i], n.points, deg, method, col=col[i], segment=c(0, 360), ...))
    invisible(NULL)
}

polarellipse <-
function(a, b, angle, deg=TRUE)
{
    if (deg) # if input is in degrees
        angle <- angle * pi/180
    b*a / sqrt(b^2*cos(angle)^2 + a^2*sin(angle)^2)
}

xyangle <-
function(x, y, directed = FALSE, deg = TRUE)
{
    if (missing(y)) {
        y <- x[,2]
        x <- x[,1]
    }
    out <- atan2(y, x)
    if (!directed)
        out <- out %% pi   
    if (deg) # if output is desired in degrees
        out <- out * 180 / pi
    out
}

