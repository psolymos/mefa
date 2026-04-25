# Ordinal Transformation of a Vector

Converts a numeric vector into an ordinal one based on quantiles.

## Usage

``` r
qvector(x, probs = seq(0, 1, 0.25), na.rm = FALSE, ...)
```

## Arguments

- x:

  a numeric vector.

- probs:

  numeric vector of probabilities with values in \[0, 1\] used by
  [`quantile`](https://rdrr.io/r/stats/quantile.html).

- na.rm:

  logical, if true, any `NA` and `NaN`s are removed from `x` before the
  quantiles are computed.

- ...:

  further arguments passed to the function
  [`quantile`](https://rdrr.io/r/stats/quantile.html).

## Value

A numeric vector of length the same as the input vector, containing the
respective (upper) quantile values.

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

[`quantile`](https://rdrr.io/r/stats/quantile.html)

## Examples

``` r
x <- rnorm(20)
x
#>  [1]  0.60674805 -0.10993567  0.17218172 -0.09032729  1.92434334  1.29839276
#>  [7]  0.74879127  0.55622433 -0.54825726  1.11053489 -2.61233433 -0.15569378
#> [13]  0.43388979 -0.38195111  0.42418757  1.06310200  1.04871262 -0.03810289
#> [19]  0.48614892  1.67288261
y <- qvector(x)
y
#>  [1] 0.75 0.25 0.50 0.50 1.00 1.00 0.75 0.75 0.25 1.00 0.25 0.25 0.50 0.25 0.50
#> [16] 1.00 0.75 0.50 0.75 1.00
rbind(x, y)
#>       [,1]       [,2]      [,3]        [,4]     [,5]     [,6]      [,7]
#> x 0.606748 -0.1099357 0.1721817 -0.09032729 1.924343 1.298393 0.7487913
#> y 0.750000  0.2500000 0.5000000  0.50000000 1.000000 1.000000 0.7500000
#>        [,8]       [,9]    [,10]     [,11]      [,12]     [,13]      [,14]
#> x 0.5562243 -0.5482573 1.110535 -2.612334 -0.1556938 0.4338898 -0.3819511
#> y 0.7500000  0.2500000 1.000000  0.250000  0.2500000 0.5000000  0.2500000
#>       [,15]    [,16]    [,17]       [,18]     [,19]    [,20]
#> x 0.4241876 1.063102 1.048713 -0.03810289 0.4861489 1.672883
#> y 0.5000000 1.000000 0.750000  0.50000000 0.7500000 1.000000
plot(x, y, type="h")
```
