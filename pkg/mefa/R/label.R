'label<-' <-
function(x, value)
{
    attr(x, "label") <- value
    return(x)
}

label <-
function(x)
{
    attr(x, "label")
}
