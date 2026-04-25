# Cross Tabulate an Object of Class 'stcs'

The function makes cross tabulation of an object of class 'stcs'.
Similar to the [`table`](https://rdrr.io/r/base/table.html) function but
handles specialities of the 'stcs' object class (i.e. removes 'pseudo
species' created to indicate empty samples). This is used internally by
the function [`mefa`](mefa.md) to make 3 dimensional representation of
the data.

## Usage

``` r
mefaCrosstab(x, segment = FALSE, nested = FALSE, drop.zero = FALSE)
```

## Arguments

- x:

  an object of class 'stcs'.

- segment:

  logical, if `TRUE`, the crosstabulation is made in 2 dimensions by the
  `samp` (sample) and `taxa` columns of the `x`. Otherwise,
  crosstabulation is made in 3 dimensions by the `samp`, `taxa` and
  `segm` (segment) columns.

- nested:

  logical, if `FALSE` count values of each segments are independent, if
  `TRUE` crosstabulated matrices are summed up in a nested fashion from
  segment level 1 to the number of segments (it is indicated in the
  segment levels as well).

- drop.zero:

  argument passed to the function [`stcs`](stcs.md), indicating whether
  empty samples (with zero sum) should be removed prior to cross
  tabulation (`TRUE`), or not (`FALSE`).

## Details

The cross tabulation of an object of class 'stcs' is made by the
function [`xtabs`](https://rdrr.io/r/stats/xtabs.html), whic accepts
non-integer values too.

## Value

It returs a matrix if `segment = FALSE` or a list with matrices for the
segments if `segment = TRUE`.

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

[`stcs`](stcs.md), [`mefa`](mefa.md),
[`rep.data.frame`](rep.data.frame.md)

## Examples

``` r
x <- data.frame(sample = LETTERS[c(1,1,2,2,3,4)],
    species = c(letters[c(5,5,5,6,7)], "zero.pseudo"),
    count = c(1,2,10,3,4,0), segment = letters[c(8,9,8,9,8,8)])
y <- stcs(x)
mefaCrosstab(y)
#>     taxa
#> samp  e f g
#>    A  3 0 0
#>    B 10 3 0
#>    C  0 0 4
#>    D  0 0 0
mefaCrosstab(y, segment = TRUE)
#> $h
#>     taxa
#> samp  e f g
#>    A  1 0 0
#>    B 10 0 0
#>    C  0 0 4
#>    D  0 0 0
#> 
#> $i
#>     taxa
#> samp e f g
#>    A 2 0 0
#>    B 0 3 0
#>    C 0 0 0
#>    D 0 0 0
#> 
mefaCrosstab(y, segment = TRUE, nested = TRUE)
#> $h
#>     taxa
#> samp  e f g
#>    A  1 0 0
#>    B 10 0 0
#>    C  0 0 4
#>    D  0 0 0
#> 
#> $`h-i`
#>     taxa
#> samp  e f g
#>    A  3 0 0
#>    B 10 3 0
#>    C  0 0 4
#>    D  0 0 0
#> 
## Examples for non-integer data
y[1:5, 3] <- y[1:5, 3] + 0.123
mefaCrosstab(y)
#>     taxa
#> samp      e     f     g
#>    A  3.246 0.000 0.000
#>    B 10.123 3.123 0.000
#>    C  0.000 0.000 4.123
#>    D  0.000 0.000 0.000
mefaCrosstab(y, segment = TRUE)
#> $h
#>     taxa
#> samp      e f     g
#>    A  1.123 0 0.000
#>    B 10.123 0 0.000
#>    C  0.000 0 4.123
#>    D  0.000 0 0.000
#> 
#> $i
#>     taxa
#> samp     e     f g
#>    A 2.123 0.000 0
#>    B 0.000 3.123 0
#>    C 0.000 0.000 0
#>    D 0.000 0.000 0
#> 
```
