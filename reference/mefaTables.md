# Subsetting and Ordering of Related Data Tables

This function is called by the [`mefa`](mefa.md) function to subset the
community data matrix and the related data frames, but can be applied
more generally for subsetting and ordering.

## Usage

``` r
mefaTables(xtab, dframe, margin, index = NULL,
drop.index = FALSE, xtab.fixed = TRUE)
```

## Arguments

- xtab:

  a data matrix.

- dframe:

  a data frame.

- margin:

  margin of the `xtab` matrix to use row (`margin = 1`) or column
  (`margin = 2`) names in comparison with the rownames of `dframe`.

- index:

  if `NULL` rownames are used directly, else it can be used to set which
  column of `dframe` should be used as rowname and for comparison.

- drop.index:

  logical, should the `index` column of `dframe` be deleted (if
  `index != NULL`).

- xtab.fixed:

  logical, if `TRUE` the `xtab` matrix is left intact, and `dframe` is
  subsetted accordingly. If `FALSE`, both `xtab` and `dframe` are
  subsetted, and returned values are based on the the intersect of the
  compared names.

## Value

Returns a list with elements `xtab` (matrix) and `dtab` (data frame),
corresponding to the subsetted `xtab` and `dframe` data sets,
respectively. Original column and row orderings in the input matrix
`xtab` are preserved.

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

[`mefa`](mefa.md)

## Examples

``` r
x <- matrix(rpois(20,1), 5, 4)
## Note the reverse alphabetical names
rownames(x) <- letters[5:1]
x
#>   [,1] [,2] [,3] [,4]
#> e    3    1    0    0
#> d    1    1    0    1
#> c    1    2    2    0
#> b    0    0    0    3
#> a    0    1    1    1
f <- data.frame(matrix(rnorm(30), 10, 3))
rownames(f) <- letters[1:10]
f
#>            X1          X2          X3
#> a  0.11203808  0.44479712  0.02956075
#> b -0.13399701  2.75541758  0.54982754
#> c -1.91008747  0.04653138 -2.27411486
#> d -0.27923724  0.57770907  2.68255718
#> e -0.31344598  0.11819487 -0.36122126
#> f  1.06730788 -1.91172049  0.21335575
#> g  0.07003485  0.86208648  1.07434588
#> h -0.63912332 -0.24323674 -0.66508825
#> i -0.04996490 -0.20608719  1.11395242
#> j -0.25148344  0.01917759 -0.24589641
## Reverse alphabetical names preserved
mefaTables(x, f, 1)
#> $xtab
#>   [,1] [,2] [,3] [,4]
#> e    3    1    0    0
#> d    1    1    0    1
#> c    1    2    2    0
#> b    0    0    0    3
#> a    0    1    1    1
#> 
#> $dtab
#>           X1         X2          X3
#> e -0.3134460 0.11819487 -0.36122126
#> d -0.2792372 0.57770907  2.68255718
#> c -1.9100875 0.04653138 -2.27411486
#> b -0.1339970 2.75541758  0.54982754
#> a  0.1120381 0.44479712  0.02956075
#> 
## Now result is the intersect
rownames(f) <- letters[3:12]
mefaTables(x, f, 1, xtab.fixed = FALSE)
#> $xtab
#>   [,1] [,2] [,3] [,4]
#> e    3    1    0    0
#> d    1    1    0    1
#> c    1    2    2    0
#> 
#> $dtab
#>           X1         X2          X3
#> e -1.9100875 0.04653138 -2.27411486
#> d -0.1339970 2.75541758  0.54982754
#> c  0.1120381 0.44479712  0.02956075
#> 
```
