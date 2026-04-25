# Matrix Manipulation Tools

Converts a matrix to a list, calculates matrix fill, or exclude
rows/columns with certain rox/column sums.

## Usage

``` r
mat2list(x, MARGIN = 1)
mfill(x)
rdrop(x, cutoff = 0, attrib = FALSE)
cdrop(x, cutoff = 0, attrib = FALSE)
```

## Arguments

- x:

  A vector or matrix to be converted. `cdrop` and `rdrop` can take mefa
  objects as argument.

- MARGIN:

  Margin in `c(1, 2)`, indicating respectively rows or colums to be
  listed.

- cutoff:

  Numeric, exclude rows/columns with sums \<= `cutoff`.

- attrib:

  Logical, if a vector of excluded rows/columns should be attached to
  the return value as an attribute.

## Value

`mat2list` returns a list of length `nrow(x)` or `ncol(x)` depending on
`MARGIN`. Elements are rows/columns of `x`. Dimnames are retained as
names.

`mfill` returns a single number with the matrix fill value.

`rdrop` and `cdrop` returns the original object but excluding rows
(`rdrop`), or columns (`cdrop`) that has sum \<= `cutoff`. Excluded
row/colum numbers and specifications are stored in the `"exclude"`
attribute of `attrib = TRUE`.

## References

S\\olymos P. (2008) mefa: an R package for handling and reporting count
data. *Community Ecology* **9**, 125–127.

S\\olymos P. (2009) Processing ecological data in R with the mefa
package. *Journal of Statistical Software* **29(8)**, 1–28.
[doi:10.18637/jss.v029.i08](https://doi.org/10.18637/jss.v029.i08)

<http://mefa.r-forge.r-project.org/>

## Author

P\\eter S\\olymos, <solymos@ualberta.ca>

## Examples

``` r
x <- matrix(1:12, 3, 4)
rownames(x) <- letters[1:3]
colnames(x) <- LETTERS[1:4]
mat2list(x, 1)
#> $a
#>  A  B  C  D 
#>  1  4  7 10 
#> 
#> $b
#>  A  B  C  D 
#>  2  5  8 11 
#> 
#> $c
#>  A  B  C  D 
#>  3  6  9 12 
#> 
mat2list(x, 2)
#> $A
#> a b c 
#> 1 2 3 
#> 
#> $B
#> a b c 
#> 4 5 6 
#> 
#> $C
#> a b c 
#> 7 8 9 
#> 
#> $D
#>  a  b  c 
#> 10 11 12 
#> 
x[2, ] <- 0
x[, 2] <- 0
mfill(x)
#> [1] 0.5
mfill(x) == sum(x > 0) / prod(dim(x))
#> [1] TRUE
rdrop(x)
#>   A B C  D
#> a 1 0 7 10
#> c 3 0 9 12
cdrop(x)
#>   A C  D
#> a 1 7 10
#> b 0 0  0
#> c 3 9 12
cdrop(x, cutoff = 5)
#>   C  D
#> a 7 10
#> b 0  0
#> c 9 12
cdrop(x, cutoff = 22) # be careful
#>  
#> a
#> b
#> c
```
