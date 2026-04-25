# Image Plot of a 'dist' Object

Image plot of a 'dist' object.

## Usage

``` r
# S3 method for class 'dist'
image(x, grad, lab = TRUE, ...)
```

## Arguments

- x:

  An object of class 'dist', not a similarity matrix.

- grad:

  Gradient to use for ordering the observations in `x`.

- lab:

  Labels to use at the left axis, can be a character vector.

- ...:

  Other graphical parameters passed to
  [`image`](https://rdrr.io/r/graphics/image.html).

## Value

Creates an image plot as a side effect.

Invisibly returns plotted values.

## Author

Peter Solymos \<solymos@ualberta.ca\>

## Examples

``` r
data(dol.count, dol.samp)
m <- mefa(stcs(dol.count), dol.samp)
d <- dist(m$xtab)
g <- with(m$samp, interaction(microhab, method))
image(d)
```
