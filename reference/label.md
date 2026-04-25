# Assign Label to an Object and Return It

This function assigns a label to any kind of object. The label later can
be retrieved and reused.

## Usage

``` r
label(x)
label(x) <- value
```

## Arguments

- value:

  any expression.

- x:

  any object.

## Details

The label can be any expression assigned to any object. It is stored as
a character attribute. Inheritance is not implemented, so by any
transformation of the object into another one, the label is lost.

## Value

Print out the label value.

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
x <- 1:10
x
#>  [1]  1  2  3  4  5  6  7  8  9 10
## Assign a character
label(x) <- "x is a vector"
x
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"label")
#> [1] "x is a vector"
## Get back the label
label(x)
#> [1] "x is a vector"
## Note the effect of deparsing
label(x) <- 1:10
label(x)
#> [1] "1:10"
```
