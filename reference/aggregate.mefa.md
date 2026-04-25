# Extract and Aggregate Objects of Class 'mefa'

These methods are useful when extracting or aggregating 'mefa' objects.
The count data matrix, the matrices for segments and the linked tables
of samples and taxa are extracted or aggregated at the same time.

## Usage

``` r
# S3 method for class 'mefa'
aggregate(x, by.samp = NULL, by.taxa = NULL, ...)
# S3 method for class 'mefa'
x[i = 1:dim(x)[1], j = 1:dim(x)[2], k = 1:dim(x)[3], drop = FALSE]
xtab(x, ...)
samp(x, ...)
taxa(x, ...)
segm(x, ...)
```

## Arguments

- x:

  an object of class 'mefa'.

- by.samp:

  a vector with length as the number of samples and more than one unique
  values to use aggregating samples.

- by.taxa:

  a vector with length as the number of taxa and more than one unique
  values to use aggregating taxa.

- i, j, k:

  index value or a vector of indices for samples, taxa and segments,
  respectively. Can be numeric or character. Negative numeric values
  indicate items to be excluded.

- drop:

  logical, if `TRUE` the unused factor leves in the linked tables for
  samples and species will be dropped after extraction. If `FALSE`
  (default) unused factor levels are left intact.

- ...:

  other arguments passed to the generic function.

## Details

If a 'mefa' object is aggregated, either or both tables for samples and
taxa (depending on if the aggregation was made by rows, columns or both)
are set to `NULL`. Because aggregation is not straightforward for data
frames with mixed type of data (e.g. factors, character vectors), and
the applied function (mean, sum, etc.) depends on the type of the
numeric variables. Consequently, if new data table is needed, that must
be defined again by the function [`mefa`](mefa.md). If the original data
frame contained only numeric columns, it can be easily aggregated if the
function to apply is known. For factors and numeric columns, however,
such aggregation is not defined.

The ordering of dimensions follow the ordering of the indices in the
extraction call, except for nested subsets, when sequence of nested
segments is not allowed to be affected by the extraction (error message
is given). The extracting method retains samples and taxa tables as
well. If non-nested segments are extracted, the `x$xtab` matrix will
contain the sum of segments, and not the original values of `x$xtab`.
Thus if a resulting 'mefa' object has only one segment, it will be
identical to the `x$xtab` matrix. If nested segments are extracted,
`x$xtab` will be identical to the last segment and not the sum of
segments. See the examples for these differences. If the original object
had nested segments and the first is excluded, segment names change as a
result. If only one segment remains in the result, the nested attribute
is turned off.

If the aggregation is done over a vector that is not part of the tables
inside the 'mefa' object, it must have a class attribute to be
recognised, use e.g. the
[`as.factor`](https://rdrr.io/r/base/factor.html) function (see
'Examples').

The resulting 'mefa' object must contain at least 2 samples and 2 taxa.

Extractor functions `xtab`, `samp`, `taxa`, and `segm` extract the
corresponding element from the 'mefa' object.

## Value

An object of class 'mefa'.

## References

S\\olymos P. (2008) mefa: an R package for handling and reporting count
data. *Community Ecology* **9**, 125–127.

S\\olymos P. (2009) Processing ecological data in R with the mefa
package. *Journal of Statistical Software* **29(8)**, 1–28.
[doi:10.18637/jss.v029.i08](https://doi.org/10.18637/jss.v029.i08)

<http://mefa.r-forge.r-project.org/>

## Author

P\\eter S\\olymos, <solymos@ualberta.ca>

## Note

The `aggregate` method replaces the function `strify`, the `[` method
replaces the function `exclmf` of the old (\< 2.0) versions of the
**mefa** package.

## See also

[`mefa`](mefa.md)

## Examples

``` r
data(dol.count, dol.samp, dol.taxa)
x <- mefa(stcs(dol.count), dol.samp, dol.taxa)
## These two are identical
aggregate(x, "microhab")
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 731 individuals of 28 taxa in 4 samples,
#>  $ segm: 2 (non-nested) segments:
#>          fresh, broken,
#>  $ samp: table for samples not provided,
#>  $ taxa: table for taxa provided (4 variables).
#> 
aggregate(x, x$samp$microhab, inside = FALSE)
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 731 individuals of 28 taxa in 4 samples,
#>  $ segm: 2 (non-nested) segments:
#>          fresh, broken,
#>  $ samp: table for samples not provided,
#>  $ taxa: table for taxa provided (4 variables).
#> 
## Interaction
aggregate(x, c("method", "microhab"))
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 731 individuals of 28 taxa in 8 samples,
#>  $ segm: 2 (non-nested) segments:
#>          fresh, broken,
#>  $ samp: table for samples not provided,
#>  $ taxa: table for taxa provided (4 variables).
#> 
## Both samples and taxa tables
aggregate(x, "microhab", "familia")
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 731 individuals of 9 taxa in 4 samples,
#>  $ segm: 2 (non-nested) segments:
#>          fresh, broken,
#>  $ samp: table for samples not provided,
#>  $ taxa: table for taxa not provided.
#> 
## To use a factor
aggregate(x, "microhab", as.factor(rbinom(dim(x)[2], 1, 0.5)))
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 731 individuals of 2 taxa in 4 samples,
#>  $ segm: 2 (non-nested) segments:
#>          fresh, broken,
#>  $ samp: table for samples not provided,
#>  $ taxa: table for taxa not provided.
#> 
## Extraction
x[1:10, ]
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 236 individuals of 28 taxa in 10 samples,
#>  $ segm: 2 (non-nested) segments:
#>          fresh, broken,
#>  $ samp: table for samples provided (2 variables),
#>  $ taxa: table for taxa provided (4 variables).
#> 
x[, 1:5]
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 126 individuals of 5 taxa in 24 samples,
#>  $ segm: 2 (non-nested) segments:
#>          fresh, broken,
#>  $ samp: table for samples provided (2 variables),
#>  $ taxa: table for taxa provided (4 variables).
#> 
x[, , "broken"]
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 338 individuals of 28 taxa in 24 samples,
#>  $ segm: 1 (non-nested) segments:
#>          broken,
#>  $ samp: table for samples provided (2 variables),
#>  $ taxa: table for taxa provided (4 variables).
#> 
x[1:10, 1:5]
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 33 individuals of 5 taxa in 10 samples,
#>  $ segm: 2 (non-nested) segments:
#>          fresh, broken,
#>  $ samp: table for samples provided (2 variables),
#>  $ taxa: table for taxa provided (4 variables).
#> 
x[1:10, 1:5, 2]
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 12 individuals of 5 taxa in 10 samples,
#>  $ segm: 1 (non-nested) segments:
#>          broken,
#>  $ samp: table for samples provided (2 variables),
#>  $ taxa: table for taxa provided (4 variables).
#> 
## Note factor levels
y <- x[which(x$samp$method == "time"), ]
levels(y$samp$method)
#> [1] "time"    "quadrat"
z <- x[which(x$samp$method == "time"), drop = TRUE]
levels(z$samp$method)
#> [1] "time"
## Note the differences in total sums
y <- mefa(stcs(dol.count), dol.samp, dol.taxa, nested = TRUE)
x[ , , 2]
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 338 individuals of 28 taxa in 24 samples,
#>  $ segm: 1 (non-nested) segments:
#>          broken,
#>  $ samp: table for samples provided (2 variables),
#>  $ taxa: table for taxa provided (4 variables).
#> 
y[ , , 2]
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 731 individuals of 28 taxa in 24 samples,
#>  $ segm: 1 (non-nested) segments:
#>          fresh-broken,
#>  $ samp: table for samples provided (2 variables),
#>  $ taxa: table for taxa provided (4 variables).
#> 
```
