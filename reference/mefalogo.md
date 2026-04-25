# Schematic Plot of an Object of Class 'mefa'

Plots a shematic representation of and object of class 'mefa', or
alternatively plots a colored logo of the **mefa** package.

## Usage

``` r
mefalogo(type = c("cont", "fill"), labels = c("str", "r"),
adj=c(0,0), scale=c(1,1), new = TRUE, cex = 1, ann=FALSE, axes=FALSE,
xlim=c(0,10)*scale[1]+adj[1], ylim=c(0,10)*scale[2]+adj[2], ...)
```

## Arguments

- type:

  character, `"cont"` is for drawing only black contours, `"fill"` is
  for filled polygons.

- labels:

  character, `"str"` for labelling according to the general structure of
  a 'mefa' object, `"r"` places a big `R` letter in the middle (for
  logo).

- adj:

  numeric values to adjust the dimensions of the polygons.

- scale:

  a numeric value to rescale the dimensions of the polygons.

- new:

  logical, if a new plot (`TRUE`) should be drawn, or just adding the
  polygons and labels to an existing one (`FALSE`).

- cex:

  numeric, argument for scaling the labels.

- ann:

  logical, annotations of the plot (see
  [`par`](https://rdrr.io/r/graphics/par.html)).

- axes:

  logical, drawing axes or not (see
  [`par`](https://rdrr.io/r/graphics/par.html)).

- xlim, ylim:

  extreme values for the x and y axes.

- ...:

  other arguments passed to the function `plot`, see
  [`par`](https://rdrr.io/r/graphics/par.html). Only effective if
  `new = TRUE`.

## Details

This function was developed for depicting the general structure of the
'mefa' objects. The colored logo version can be used in presentations as
well.

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
mefalogo()

mefalogo("fill", "r")
```
