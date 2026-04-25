# Inflate (repeat) a Data Frame

The function inflates a data frame based on count values in a vector
corresponding the rows.

## Usage

``` r
# S3 method for class 'data.frame'
rep(x, ...)
```

## Arguments

- x:

  a matrix or data frame, but can be a vector.

- ...:

  arguments passed to the function
  [`rep`](https://rdrr.io/r/base/rep.html), i.e. `times`, `length.out`
  and `each` (see explanation there).

## Details

Each rows of the data frame `x` will be repeated as many times as the
corresponding value in e.g. the vector `times`. This function is called
internally by the function [`stcs`](stcs.md).

## Value

A data frame with columns as in the original (`x`) and e.g. `sum(times)`
number of rows.

## References

S\\olymos P. (2008) mefa: an R package for handling and reporting count
data. *Community Ecology* **9**, 125–127.

S\\olymos P. (2009) Processing ecological data in R with the mefa
package. *Journal of Statistical Software* **29(8)**, 1–28.
[doi:10.18637/jss.v029.i08](https://doi.org/10.18637/jss.v029.i08)

<http://mefa.r-forge.r-project.org/>

## Author

P\\eter S\\olymos, <solymos@ualberta.ca>

## See also

[`stcs`](stcs.md)

## Examples

``` r
x <- data.frame(sample = LETTERS[c(1,1,2,2,3)],
    species = letters[c(5,5,5,6,7)],
    count = c(1,2,10,3,4), segment = letters[c(8,9,8,9,8)])
x
#>   sample species count segment
#> 1      A       e     1       h
#> 2      A       e     2       i
#> 3      B       e    10       h
#> 4      B       f     3       i
#> 5      C       g     4       h
rep(x[,c(1,2,4)], times = x[,3])
#>    sample species segment
#> 1       A       e       h
#> 2       A       e       i
#> 3       A       e       i
#> 4       B       e       h
#> 5       B       e       h
#> 6       B       e       h
#> 7       B       e       h
#> 8       B       e       h
#> 9       B       e       h
#> 10      B       e       h
#> 11      B       e       h
#> 12      B       e       h
#> 13      B       e       h
#> 14      B       f       i
#> 15      B       f       i
#> 16      B       f       i
#> 17      C       g       h
#> 18      C       g       h
#> 19      C       g       h
#> 20      C       g       h
rep(x[,c(1,2,4)], each = 2)
#>    sample species segment
#> 1       A       e       h
#> 2       A       e       h
#> 3       A       e       i
#> 4       A       e       i
#> 5       B       e       h
#> 6       B       e       h
#> 7       B       f       i
#> 8       B       f       i
#> 9       C       g       h
#> 10      C       g       h
```
