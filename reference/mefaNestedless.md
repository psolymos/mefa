# Make Non-Nested 'mefa' Object

The functions makes a non-nested 'mefa' object.

## Usage

``` r
mefaNestedless(x)
```

## Arguments

- x:

  an objects of class 'mefa' with nested segments.

## Value

Returns an object of class 'mefa', with non-nested segments. This
function is internally used by the [`melt.mefa`](melt.mefa.md) function.

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

[`mefa`](mefa.md), [`is.mefa`](mefa.md), [`melt.mefa`](melt.mefa.md)

## Examples

``` r
data(dol.count)
x <- mefa(stcs(dol.count), nested = TRUE)
x
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 731 individuals of 28 taxa in 24 samples,
#>  $ segm: 2 nested segments:
#>          fresh, fresh-broken,
#>  $ samp: table for samples not provided,
#>  $ taxa: table for taxa not provided.
#> 
y <- mefaNestedless(x)
y
#> 
#> An object of class 'mefa' containing
#> 
#>  $ xtab: 731 individuals of 28 taxa in 24 samples,
#>  $ segm: 2 (non-nested) segments:
#>          fresh, fresh-broken,
#>  $ samp: table for samples not provided,
#>  $ taxa: table for taxa not provided.
#> 
```
