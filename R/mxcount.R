`mxcount` <-
function(xc1, xc2, segment="unspecified"){

if(xc1$segment == xc2$segment) segment <- xc1$segment
if(is.null(xc1$digits) & is.null(xc2$digits)) {
	digits <- NULL} else {
	digits <- max(xc1$digits, xc2$digits)}

out <- 
	xcount(
		msscount(
			ttsscount(as.data.frame(xc1$data), segment=segment, digits=digits), 
			ttsscount(as.data.frame(xc2$data), segment=segment, digits=digits)
		)
	, segment=segment)

if(xc1$segment == xc2$segment) out$segment <- xc1$segment else out$segment <- segment

return(out)}

