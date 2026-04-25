# Abundance-Occupancy Plot

Abundance-occupancy plot.

## Usage

``` r
aoplot(x, ...)
# Default S3 method
aoplot(x, log = TRUE, xlab, ylab, ...)
```

## Arguments

- x:

  An object, possibly a matrix.

- log:

  Logical, if abundance values should be log10 transformed.

- xlab, ylab:

  Axis labels, can be missing for default labels.

- ...:

  Other arguments passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html) function.

## Details

Column sums of `x` (species abundances) are plotted against column sums
of the binarized `x` (species occupancy).

## Value

Creates a plot as a side effect.

Invisibly returns abundance and occurrence values in a matrix.

## Author

Peter Solymos \<solymos@ualberta.ca\>

## Examples

``` r
data(dol.count)
x <- mefa(stcs(dol.count))
aoplot(x)
```
