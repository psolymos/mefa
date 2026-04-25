# Manipulation Tools for Class 'dist'

Converts a 'dist' object into vector and vice versa.

## Usage

``` r
vec2dist(x, size, labels = NULL, diag = FALSE, upper = FALSE, 
call = FALSE, method = NULL)
is.dist(x)
# S3 method for class 'dist'
stack(x, dim.names = FALSE, ...)
```

## Arguments

- x:

  A vector or a 'dist' object.

- size:

  Integer, the number of observations in the dataset (see
  [`dist`](https://rdrr.io/r/stats/dist.html)).

- labels, diag, upper, call, method:

  Logical, setting attributes of the distance object (see
  [`dist`](https://rdrr.io/r/stats/dist.html)).

- dim.names:

  Logical, if `TRUE`, names stored in the `Labels` attribute of the
  `dist` object are returned as factors instead of row/column indices.

- ...:

  Other arguments passed to functions.

## Value

`vec2dist` returns an object of class `dist`.

`is.dist` returns a logical statement about the class of the object.

`stack.dist` returns a data frame with comuns `row` for row id's, `col`
for column id's and `dist` for the distance values.

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
vec2dist(1:3, 3)
#>   1 2
#> 2 1  
#> 3 2 3
vec2dist(1:3, 3, diag = TRUE)
#>   1 2 3
#> 1 0    
#> 2 1 0  
#> 3 2 3 0
vec2dist(1:3, 3, diag = TRUE, upper = TRUE)
#>   1 2 3
#> 1 0 1 2
#> 2 1 0 3
#> 3 2 3 0
vec2dist(1:3, 3, labels=letters[1:3])
#>   a b
#> b 1  
#> c 2 3
x <- dist(1:5)
attr(x, "Labels") <- LETTERS[1:5]
x
#>   A B C D
#> B 1      
#> C 2 1    
#> D 3 2 1  
#> E 4 3 2 1
is.dist(x)
#> [1] TRUE
stack(x)
#>    row col dist
#> 1    2   1    1
#> 2    3   1    2
#> 3    4   1    3
#> 4    5   1    4
#> 5    3   2    1
#> 6    4   2    2
#> 7    5   2    3
#> 8    4   3    1
#> 9    5   3    2
#> 10   5   4    1
stack(x, dim.names = TRUE)
#>    row col dist
#> 1    B   A    1
#> 2    C   A    2
#> 3    D   A    3
#> 4    E   A    4
#> 5    C   B    1
#> 6    D   B    2
#> 7    E   B    3
#> 8    D   C    1
#> 9    E   C    2
#> 10   E   D    1
```
