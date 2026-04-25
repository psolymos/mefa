# Check and Compare Object(s) of Class 'mefa'

The functions evaluates the internal structute of a 'mefa' object.

## Usage

``` r
mefaCheck(x)
mefaCompare(x1, x2, strict = FALSE)
```

## Arguments

- x, x1, x2:

  an object of class 'mefa'.

- strict:

  logical, if `TRUE` the function checks if ordering of dimnames are the
  same, otherwise it compares only matching.

## Value

`mefaCheck` returns a list, first element is logical with `TRUE`
indicating that all requirements are met. If this is `FALSE`, following
elements of the list describe the encountered problems as characters.
`mefaCheck` is called by the [`is.mefa`](mefa.md) function.

`mefaCompare` compares two mefa objects. If all dimnames, and the
corresponding values in the `xtab` and `segm` parts are identical, the
result is `TRUE`, and otherwise `FALSE`. Different ordering of dimnames
allowed, the comparison is based on the numeric values determined by
dimnames.

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

[`mefa`](mefa.md), [`is.mefa`](mefa.md),
[`all.equal.list`](https://rdrr.io/r/base/all.equal.html)

## Examples

``` r
x <- matrix(rpois(20,1), 5, 4)
m <- mefa(x)
mefaCheck(m)
#> [[1]]
#> [1] TRUE
#> 
data(dol.count)
x1 <- mefa(stcs(dol.count))
x2 <- mefa(melt(x1))
mefaCompare(x1, x2)
#> [1] TRUE
mefaCompare(x1, x2, strict = TRUE)
#> [1] FALSE
```
