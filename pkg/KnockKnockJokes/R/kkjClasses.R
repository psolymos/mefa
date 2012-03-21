## class for jokes
setClass("KnockKnockJoke", 
    representation(
        name = "character",
        punchline = "character"),
    prototype = list(
        name = character(0),
        punchline = character(0)))

## creator function for jokes
KnockKnockJoke <- function(name, punchline) {
    x <- new("KnockKnockJoke")
    x@name <- name
    x@punchline <- punchline
    x
}

## show method for jokes
setMethod("show", "KnockKnockJoke", function(object) {
    cat("Knock-knock!\n\tWho's there?\n")
    cat(object@name, ".\n\t", sep="")
    cat(object@name, "who?\n")
    joke <- object@punchline
    if (length(joke) > 2)
        joke <- paste(joke, collapse=" ")
    cat(strsplit(joke, " ")[[1]],
        fill=getOption("width")-5)
    invisible(object)
})

## function to access jokes
KnockKnock <- function(x, data) {
  if (missing(data)) {
    e <- new.env()
    utils:::data(kkjokes, envir=e)
    data <- get("kkjokes", envir=e)
  }
  if (missing(x))
    x <- sample.int(length(data), 1)
  kkj <- data[[x[1]]]
  KnockKnockJoke(kkj[1], kkj[2])
}
