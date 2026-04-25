# Replace \<NA\> Values with Nearest Above Values

The function replaces `<NA>` values with the nearest values above them
in the same column.

## Usage

``` r
fill.na(x)
```

## Arguments

- x:

  a vector, a matrix or a data frame.

## Details

This function is useful when converting a 'notebook style' data into an
'stcs' object. The 'notebook style' data means that observations are
registered in a non-redundant way, e.g. when typing in a spreadsheet.

## Value

A matrix or a data frame.

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

This function is not vectorized, so can be a bit slow for large data
sets.

## See also

[`stcs`](stcs.md)

## Examples

``` r
x <- data.frame(
    samp = c("s1",NA,"s2",NA,"s3","s4"),
    taxa = c("t1",NA,"s1","s2","s3",NA),
    count = c(1, 2, 10, 3, 4, 1),
    segm = c("male","female","male","female","male","male"))
x
#>   samp taxa count   segm
#> 1   s1   t1     1   male
#> 2 <NA> <NA>     2 female
#> 3   s2   s1    10   male
#> 4 <NA>   s2     3 female
#> 5   s3   s3     4   male
#> 6   s4 <NA>     1   male
fill.na(x)
#>   samp taxa count   segm
#> 1   s1   t1     1   male
#> 2   s1   t1     2 female
#> 3   s2   s1    10   male
#> 4   s2   s2     3 female
#> 5   s3   s3     4   male
#> 6   s4   s3     1   male
fill.na(c(1:4, NA))
#> [1] 1 2 3 4 4
```
