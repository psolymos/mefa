# The Dolina Dataset

Data were collected in dolines (sinkholes, karstic depressions) of
Northeast Hungary.

## Usage

``` r
data(dol.count)
data(dol.samp)
data(dol.taxa)
```

## Format

The `dol.count` data set is a data frame with 297 observations on the
following 4 variables.

- `samp`:

  a factor with levels for samples.

- `taxa`:

  a factor with levels for the species.

- `count`:

  a numeric vector with count data.

- `segm`:

  a factor with levels for segments.

The `dol.samp` data set is a data frame with 24 observations on the
following 2 variables.

- `microhab`:

  a factor with levels `dead.wood` `litter` `live.wood` `rock` for
  microhabitats.

- `method`:

  a factor with levels `time` `quadrat` for the sampling method applied.

The `dol.taxa` data set is a data frame with 121 observations on the
following 4 variables.

- `species`:

  a factor with levels the species names.

- `author`:

  a factor with levels for authors of the species and description dates
  (accents are removed).

- `familia`:

  a factor with levels for family level classification.

- `size`:

  a numeric vector for major shell dimension of the adult specimen.

## Details

The data sets are results of the 'DOLINA 2007' expedition (Vilisics et
al. 2008). Species characteristics are based on Kerney et al. (1983).
The full dataset (S\\olymos & Kemencei 2008) from which this example was
derived is available at the Dataverse Network (hdl:1902.1/12060) with
the code to make the subsetting.

## Source

S\\olymos, P., Kemencei, Z. (2008) Methodological study data set of land
snails from the Dolina 2007 project. Available at the Dataverse Network:
hdl:1902.1/12060.

S\\olymos, P., Kemencei, Z., P\\all-Gergely, B., Farkas, R., Vilisics,
F., Hornung, E. (2009) Does shell accumulation matter in micro scale
land snail surveys? *Malacologia*, **51**, 389–393.

Vilisics, F., Nagy, A., S\\olymos, P., Farkas, R., Kemencei, Z.,
P\\all-Gergely, B., Kisfali, M., Hornung, E. (2008) Data on the
terrestrial isopoda fauna of the Als\\o-hegy, Aggtelek National Park,
Hungary. *Folia Faunistica Slovaca* **13**, 9–12.

Kerney, M. P., Cameron, R. A. D., Jungbluth, J. H. (1983) *Die
Landschnecken Nord- und Mitteleuropas*. P. Parey, Hamburg-Berlin, 384 p.

## Examples

``` r
data(dol.count)
str(dol.count)
#> 'data.frame':    297 obs. of  4 variables:
#>  $ samp : Factor w/ 24 levels "DQ1","DQ2","DQ3",..: 7 7 7 7 7 7 7 10 8 8 ...
#>  $ taxa : Factor w/ 32 levels "aacu","amin",..: 1 2 2 13 17 25 25 32 1 2 ...
#>  $ count: num  1 1 2 1 1 13 15 0 1 1 ...
#>  $ segm : Factor w/ 3 levels "fresh","broken",..: 1 2 1 2 2 1 2 3 1 2 ...
data(dol.samp)
str(dol.samp)
#> 'data.frame':    24 obs. of  2 variables:
#>  $ microhab: Factor w/ 4 levels "dead.wood","litter",..: 2 2 2 2 2 1 1 1 1 1 ...
#>  $ method  : Factor w/ 2 levels "time","quadrat": 2 2 1 2 1 2 1 2 1 2 ...
data(dol.taxa)
str(dol.taxa)
#> 'data.frame':    121 obs. of  4 variables:
#>  $ species: Factor w/ 121 levels "Acanthinula aculeata",..: 3 2 81 82 16 17 92 93 72 32 ...
#>  $ author : Factor w/ 65 levels "(Alder, 1830)",..: 26 53 42 19 44 47 31 14 47 43 ...
#>  $ familia: Factor w/ 21 levels "Aciculidae","Bradybaenidae",..: 1 1 14 14 6 6 17 17 17 5 ...
#>  $ size   : num  3.4 5.5 16 16 2.2 2.3 17 8 12 7.5 ...
```
