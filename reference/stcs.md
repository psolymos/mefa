# Create an Object of Class 'stcs'

This function creates object of class 'stcs' from a data frame. This is
the primary format for long (database style) data sets in the **mefa**
package. The 'stcs' object can be converted into an object of class
'mefa' with the function `mefa`. The function name 'stcs' refers to
first letters of column names of the resulting object: **s**amples,
**t**axa, **c**ounts, **s**egments.

## Usage

``` r
stcs(dframe, expand = FALSE, drop.zero = FALSE, zero.pseudo = "zero.pseudo")
# S3 method for class 'stcs'
is(x)
# S3 method for class 'stcs'
summary(object, ...)
```

## Arguments

- dframe:

  a data frame with 2-4 columns. If 2 columns are provided, it is
  assumed that first column contains sample, while second taxa names. If
  3 columns are provided, the first two is treated as sample and taxa
  names, while the third is treated as count if numeric (either integer
  or non-integer values can be supplied), and segment if character or
  factor. If 4 columns are provided, those are assumed to be in the
  samples, taxa, count, segment order.

- expand:

  logical, whether the object should be (`TRUE`) expanded (via the
  function [`rep.data.frame`](rep.data.frame.md)) or not (`FALSE`,
  default). If expanded, each rows correspond to one individual
  observation (value of the `count` column is 1) or a zero sample.
  `expand = TRUE` works only with integer values.

- drop.zero:

  logical, whether samples with zero total count should be left out
  (`TRUE`) or not (`FALSE`, default).

- zero.pseudo:

  character, value in the `taxa` column that indicates zero samples. If
  it is a vector of length 2, first element is used for taxa names,
  second for segment names to indicate zero samples.

- x, object:

  an object of class 'stcs'.

- ...:

  further arguments passed to the function `stcs`.

## Details

If the data are in a long (database style) format, the `stcs` function
prepares the ground for the `mefa` function to make the cross
tabulation. But if only a crosstabulated matrix is needed, the
[`table`](https://rdrr.io/r/base/table.html) function can be applies as
well (in this case be sure to set `expand = TRUE`, and this is only for
integer data), or alternatively see the
[`xtabs`](https://rdrr.io/r/stats/xtabs.html) or
[`mefaCrosstab`](mefaCrosstab.md) functions (these accepts non-integer
data). For subsetting, simply use extraction methods available for data
frames
([`[.data.frame`](https://rdrr.io/r/base/Extract.data.frame.html)).

The summary contains statistics calculated after coercing the object
into the class 'mefa'.

Methods for extracting, subsetting are the same as for data frame
objects. Plotting 'stcs' objects are discussed elsewhere (see links in
'See also' section).

## Value

Returns an object of class 'stcs' with 4 columns. The column names are
converted consistently into `samp` for samples, `taxa` for taxa names,
`count` for counts and `segm` for segment names.

The class has its own methods (i.e. `is`, `as.stcs`, `summary` and
`plot`), but also has a `data.frame` class attribute. Thus other methods
that available for data frames are applicable to an object of class
'stcs' (e.g. `print`, `str`).

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

The `stcs` function nearly equivalent to the `sscount` function in older
(\< 2.0) versions of the **mefa** package.

## See also

[`data.frame`](https://rdrr.io/r/base/data.frame.html),
[`summary.data.frame`](https://rdrr.io/r/base/summary.html),
[`str`](https://rdrr.io/r/utils/str.html),
[`table`](https://rdrr.io/r/base/table.html)

See [`plot.stcs`](plot.mefa.md) [`boxplot.stcs`](plot.mefa.md) and
[`image.stcs`](plot.mefa.md) for graphical display options.

See [`as.stcs`](as.mefa.md) for coercion methods.

## Examples

``` r
## General long format data
x <- data.frame(
    sample = paste("Sample", c(1,1,2,2,3,4), sep="."),
    species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),
    count = c(1,2,10,3,4,0),
    segment = letters[c(6,13,6,13,6,6)])
x
#>     sample     species count segment
#> 1 Sample.1   Species.1     1       f
#> 2 Sample.1   Species.1     2       m
#> 3 Sample.2   Species.1    10       f
#> 4 Sample.2   Species.2     3       m
#> 5 Sample.3   Species.3     4       f
#> 6 Sample.4 zero.pseudo     0       f
## Long format as stcs
y <- stcs(x)
y
#>       samp        taxa count        segm
#> 1 Sample.1   Species.1     1           f
#> 2 Sample.1   Species.1     2           m
#> 3 Sample.2   Species.1    10           f
#> 4 Sample.2   Species.2     3           m
#> 5 Sample.3   Species.3     4           f
#> 6 Sample.4 zero.pseudo     0 zero.pseudo
## Methods
as.stcs(x)
#>       samp        taxa count        segm
#> 1 Sample.1   Species.1     1           f
#> 2 Sample.1   Species.1     2           m
#> 3 Sample.2   Species.1    10           f
#> 4 Sample.2   Species.2     3           m
#> 5 Sample.3   Species.3     4           f
#> 6 Sample.4 zero.pseudo     0 zero.pseudo
is(y, "stcs")
#> [1] TRUE
is(y, "data.frame")
#> [1] TRUE
## Effects of arguments
stcs(x, expand = TRUE)
#>        samp        taxa count        segm
#> 1  Sample.1   Species.1     1           f
#> 2  Sample.1   Species.1     1           m
#> 3  Sample.1   Species.1     1           m
#> 4  Sample.2   Species.1     1           f
#> 5  Sample.2   Species.1     1           f
#> 6  Sample.2   Species.1     1           f
#> 7  Sample.2   Species.1     1           f
#> 8  Sample.2   Species.1     1           f
#> 9  Sample.2   Species.1     1           f
#> 10 Sample.2   Species.1     1           f
#> 11 Sample.2   Species.1     1           f
#> 12 Sample.2   Species.1     1           f
#> 13 Sample.2   Species.1     1           f
#> 14 Sample.2   Species.2     1           m
#> 15 Sample.2   Species.2     1           m
#> 16 Sample.2   Species.2     1           m
#> 17 Sample.3   Species.3     1           f
#> 18 Sample.3   Species.3     1           f
#> 19 Sample.3   Species.3     1           f
#> 20 Sample.3   Species.3     1           f
#> 21 Sample.4 zero.pseudo     0 zero.pseudo
stcs(x, drop.zero = TRUE)
#>       samp      taxa count segm
#> 1 Sample.1 Species.1     1    f
#> 2 Sample.1 Species.1     2    m
#> 3 Sample.2 Species.1    10    f
#> 4 Sample.2 Species.2     3    m
#> 5 Sample.3 Species.3     4    f
stcs(x, zero.pseudo = "pseudo.secies")
#>       samp          taxa count          segm
#> 1 Sample.1     Species.1     1             f
#> 2 Sample.1     Species.1     2             m
#> 3 Sample.2     Species.1    10             f
#> 4 Sample.2     Species.2     3             m
#> 5 Sample.3     Species.3     4             f
#> 6 Sample.4 pseudo.secies     0 pseudo.secies
## Input options
stcs(x[,1:2])
#>       samp        taxa count      segm
#> 1 Sample.1   Species.1     1 undefined
#> 2 Sample.1   Species.1     1 undefined
#> 3 Sample.2   Species.1     1 undefined
#> 4 Sample.2   Species.2     1 undefined
#> 5 Sample.3   Species.3     1 undefined
#> 6 Sample.4 zero.pseudo     1 undefined
stcs(x[,1:3])
#>       samp        taxa count        segm
#> 1 Sample.1   Species.1     1   undefined
#> 2 Sample.1   Species.1     2   undefined
#> 3 Sample.2   Species.1    10   undefined
#> 4 Sample.2   Species.2     3   undefined
#> 5 Sample.3   Species.3     4   undefined
#> 6 Sample.4 zero.pseudo     0 zero.pseudo
stcs(x[,c(1:2,4)])
#>       samp        taxa count segm
#> 1 Sample.1   Species.1     1    f
#> 2 Sample.1   Species.1     1    m
#> 3 Sample.2   Species.1     1    f
#> 4 Sample.2   Species.2     1    m
#> 5 Sample.3   Species.3     1    f
#> 6 Sample.4 zero.pseudo     1    f
## The Dolina dataset
data(dol.count)
dol <- stcs(dol.count)
dol
#>     samp        taxa count        segm
#> 1    DQ1        amin     2       fresh
#> 2    DQ1        amin     2      broken
#> 3    DQ1        ctri     2       fresh
#> 4    DQ1        dper     2       fresh
#> 5    DQ1        estr     1       fresh
#> 6    DQ1        vidi     2      broken
#> 7    DQ1        vidi     7       fresh
#> 8    DQ2        amin     1      broken
#> 9    DQ2        amin     3       fresh
#> 10   DQ2        apur     1      broken
#> 11   DQ2        ctri     7      broken
#> 12   DQ2        ctri    13       fresh
#> 13   DQ2        dper     1       fresh
#> 14   DQ2        dper     1      broken
#> 15   DQ2        druf     1       fresh
#> 16   DQ2        druf     1      broken
#> 17   DQ2        ppyg     1      broken
#> 18   DQ2        vidi     4      broken
#> 19   DQ2        vidi     5       fresh
#> 20   DQ3        amin     1      broken
#> 21   DQ3        amin     2       fresh
#> 22   DQ3        apur     1       fresh
#> 23   DQ3        hobv     3      broken
#> 24   DQ3        vidi     2      broken
#> 25   DT1        amin     1       fresh
#> 26   DT1        bcan     1      broken
#> 27   DT1        dper     1       fresh
#> 28   DT1        estr     1      broken
#> 29   DT1        hobv     1       fresh
#> 30   DT1        mbor     5       fresh
#> 31   DT1        pvic     1       fresh
#> 32   DT2        amin     1       fresh
#> 33   DT2        amin     1       fresh
#> 34   DT2        apur     1       fresh
#> 35   DT2        bbip     1       fresh
#> 36   DT2        clam     2       fresh
#> 37   DT2        dper     1       fresh
#> 38   DT2        druf     1       fresh
#> 39   DT2        estr     1      broken
#> 40   DT2        hobv     1       fresh
#> 41   DT2        tuni     1       fresh
#> 42   DT2        vidi     2       fresh
#> 43   DT3        amin     1       fresh
#> 44   DT3        amin     2       fresh
#> 45   DT3        bbip     1      broken
#> 46   DT3        hobv     1       fresh
#> 47   DT3        hobv     1      broken
#> 48   DT3        hobv     4       fresh
#> 49   DT3        pvic     1       fresh
#> 50   DT3        vidi     2       fresh
#> 51   LQ1        aacu     1       fresh
#> 52   LQ1        amin     1      broken
#> 53   LQ1        amin     2       fresh
#> 54   LQ1        dper     1      broken
#> 55   LQ1        ffau     1      broken
#> 56   LQ1        ppyg    13       fresh
#> 57   LQ1        ppyg    15      broken
#> 58   LQ2        aacu     1       fresh
#> 59   LQ2        amin     1      broken
#> 60   LQ2        dper     1       fresh
#> 61   LQ2        ppyg     5       fresh
#> 62   LQ2        ppyg     8      broken
#> 63   LQ2        pvic     1      broken
#> 64   LQ3        amin     2      broken
#> 65   LQ3        apur     1       fresh
#> 66   LQ3        bbip     1      broken
#> 67   LQ3        ccer     1      broken
#> 68   LQ3        dper     1       fresh
#> 69   LQ3        dper     1      broken
#> 70   LQ3        dper     2       fresh
#> 71   LQ3        dper     3       fresh
#> 72   LQ3        ppyg     1       fresh
#> 73   LQ3        ppyg    10       fresh
#> 74   LQ3        ppyg    16      broken
#> 75   LQ3        ppyg    17      broken
#> 76   LQ3        ppyg    25       fresh
#> 77   LQ3        pvic     1      broken
#> 78   LT1 zero.pseudo     0 zero.pseudo
#> 79   LT2        amin     1       fresh
#> 80   LT2        pvic     1      broken
#> 81   LT3        amin     1       fresh
#> 82   LT3        amin     1       fresh
#> 83   LT3        amin     3       fresh
#> 84   LT3        dper     1       fresh
#> 85   LT3        pinc     1       fresh
#> 86   LT3        pvic     1       fresh
#> 87   LT3        tuni     1       fresh
#> 88   LT3        tuni     1      broken
#> 89   RQ1        amin     1       fresh
#> 90   RQ1        amin     1       fresh
#> 91   RQ1        amin     1      broken
#> 92   RQ1        ccer     1      broken
#> 93   RQ1        clam     1       fresh
#> 94   RQ1        cort     1       fresh
#> 95   RQ1        dper     1       fresh
#> 96   RQ1        dper     1       fresh
#> 97   RQ1        dper     1      broken
#> 98   RQ1        ffau     1      broken
#> 99   RQ1        ffau     1      broken
#> 100  RQ1        hobv     1       fresh
#> 101  RQ1        hobv     1       fresh
#> 102  RQ1        ogla     1      broken
#> 103  RQ1        ppyg     1       fresh
#> 104  RQ1        ppyg     1       fresh
#> 105  RQ1        ppyg     1      broken
#> 106  RQ1        ppyg     2       fresh
#> 107  RQ1        vidi     1       fresh
#> 108  RQ1        vidi     1      broken
#> 109  RQ1        vidi     1      broken
#> 110  RQ1        vidi     1      broken
#> 111  RQ2        amin     1       fresh
#> 112  RQ2        amin     6       fresh
#> 113  RQ2        bbip     1       fresh
#> 114  RQ2        bbip     1       fresh
#> 115  RQ2        bbip     1      broken
#> 116  RQ2        bbip     1      broken
#> 117  RQ2        ctri     1       fresh
#> 118  RQ2        ctri     1      broken
#> 119  RQ2        ctri     2      broken
#> 120  RQ2        ctri     3       fresh
#> 121  RQ2        dbre     1      broken
#> 122  RQ2        dper     1       fresh
#> 123  RQ2        dper     1       fresh
#> 124  RQ2        dper     1      broken
#> 125  RQ2        dper     1      broken
#> 126  RQ2        dper     2       fresh
#> 127  RQ2        estr     3       fresh
#> 128  RQ2        ffau     1       fresh
#> 129  RQ2        ffau     1      broken
#> 130  RQ2        hobv     9       fresh
#> 131  RQ2        mobs     2       fresh
#> 132  RQ2        odol     4      broken
#> 133  RQ2        odol     9       fresh
#> 134  RQ2        ogla     1      broken
#> 135  RQ2        ogla     2       fresh
#> 136  RQ2        ppyg     9      broken
#> 137  RQ2        ppyg    19       fresh
#> 138  RQ2        pvic     1      broken
#> 139  RQ2        tuni     2       fresh
#> 140  RQ2        vcos     2       fresh
#> 141  RQ2        vidi     1       fresh
#> 142  RQ2        vidi     3       fresh
#> 143  RQ2        vidi     3       fresh
#> 144  RQ3        aacu     2      broken
#> 145  RQ3        amin     2      broken
#> 146  RQ3        amin     3       fresh
#> 147  RQ3        amin     4       fresh
#> 148  RQ3        amin     4      broken
#> 149  RQ3        bbip     1      broken
#> 150  RQ3        ctri     1       fresh
#> 151  RQ3        ctri     2      broken
#> 152  RQ3        ctri     7       fresh
#> 153  RQ3        dbre     1       fresh
#> 154  RQ3        dbre     2       fresh
#> 155  RQ3        dper     1       fresh
#> 156  RQ3        dper     1      broken
#> 157  RQ3        dper     2       fresh
#> 158  RQ3        dper     2      broken
#> 159  RQ3        eful     1       fresh
#> 160  RQ3        eful     1      broken
#> 161  RQ3        ffau     1      broken
#> 162  RQ3        ffau     1      broken
#> 163  RQ3        hobv     1       fresh
#> 164  RQ3        hobv     1      broken
#> 165  RQ3        mobs     1      broken
#> 166  RQ3        ppyg     1       fresh
#> 167  RQ3        ppyg     1      broken
#> 168  RQ3        ppyg     3       fresh
#> 169  RQ3        ppyg     4      broken
#> 170  RQ3        pvic     1      broken
#> 171  RQ3        vidi     1       fresh
#> 172  RQ3        vidi     1      broken
#> 173  RQ3        vidi     1      broken
#> 174  RQ3        vidi     2      broken
#> 175  RQ3        vidi     6       fresh
#> 176  RT1        amin     1       fresh
#> 177  RT1        amin     3      broken
#> 178  RT1        bbip     3      broken
#> 179  RT1        ccer     1       fresh
#> 180  RT1        ccer     1      broken
#> 181  RT1        cort     1      broken
#> 182  RT1        dper     1       fresh
#> 183  RT1        ffau     2       fresh
#> 184  RT1        ffau     5      broken
#> 185  RT1        hobv     1       fresh
#> 186  RT1        hobv     7      broken
#> 187  RT1        iiso     6      broken
#> 188  RT1        pinc     1       fresh
#> 189  RT1        pinc     2      broken
#> 190  RT1        pvic     1       fresh
#> 191  RT1        pvic     3      broken
#> 192  RT1        tuni     1       fresh
#> 193  RT1        tuni     4      broken
#> 194  RT2        amin     1      broken
#> 195  RT2        apur     2      broken
#> 196  RT2        bbip     1       fresh
#> 197  RT2        bbip     2      broken
#> 198  RT2        bcan     1      broken
#> 199  RT2        cort     1      broken
#> 200  RT2        ctri     1       fresh
#> 201  RT2        dper     1       fresh
#> 202  RT2        dper     1       fresh
#> 203  RT2        estr     1       fresh
#> 204  RT2        estr     3      broken
#> 205  RT2        ffau     1       fresh
#> 206  RT2        ffau     5      broken
#> 207  RT2        ffau     6      broken
#> 208  RT2        hobv     1      broken
#> 209  RT2        iiso     4      broken
#> 210  RT2        iiso     4      broken
#> 211  RT2        pinc     1       fresh
#> 212  RT2        pinc     1      broken
#> 213  RT2        pinc     3      broken
#> 214  RT2        pvic     1      broken
#> 215  RT2        tuni     1       fresh
#> 216  RT2        tuni     2      broken
#> 217  RT2        vidi     1       fresh
#> 218  RT2        vidi     1      broken
#> 219  RT3        amin     1      broken
#> 220  RT3        amin     4       fresh
#> 221  RT3        apur     1      broken
#> 222  RT3        bbip     5       fresh
#> 223  RT3        clam     1      broken
#> 224  RT3        ffau     1       fresh
#> 225  RT3        ffau     2       fresh
#> 226  RT3        ffau     3      broken
#> 227  RT3        hobv     1       fresh
#> 228  RT3        hobv     1      broken
#> 229  RT3        hobv     2       fresh
#> 230  RT3        iiso     1       fresh
#> 231  RT3        pinc     3      broken
#> 232  RT3        pvic     1       fresh
#> 233  RT3        pvic     2      broken
#> 234  RT3        tuni     1      broken
#> 235  WQ1        amin     1       fresh
#> 236  WQ1        amin     1       fresh
#> 237  WQ1        amin     1      broken
#> 238  WQ1        bbip     1      broken
#> 239  WQ1        bcan     1      broken
#> 240  WQ1        cort     1      broken
#> 241  WQ1        cort     2      broken
#> 242  WQ1        iiso     1       fresh
#> 243  WQ1        iiso     1       fresh
#> 244  WQ1        pinc     1       fresh
#> 245  WQ1        pinc     1       fresh
#> 246  WQ1        ppyg     1      broken
#> 247  WQ1        ppyg     2      broken
#> 248  WQ1        ppyg     4       fresh
#> 249  WQ1        ppyg     7       fresh
#> 250  WQ1        ppyg    17      broken
#> 251  WQ1        pvic     1      broken
#> 252  WQ1        vidi     1       fresh
#> 253  WQ1        vidi     1       fresh
#> 254  WQ1        vidi     1      broken
#> 255  WQ1        vidi     1      broken
#> 256  WQ2        amin     1       fresh
#> 257  WQ2        amin     1      broken
#> 258  WQ2        amin     2      broken
#> 259  WQ2        bbip     1      broken
#> 260  WQ2        ccer     1       fresh
#> 261  WQ2        dper     1       fresh
#> 262  WQ2        dper     1      broken
#> 263  WQ2        dper     2       fresh
#> 264  WQ2        dper     4      broken
#> 265  WQ2        dper    12       fresh
#> 266  WQ2        estr     2      broken
#> 267  WQ2        mobs     2      broken
#> 268  WQ2        pinc     1       fresh
#> 269  WQ2        ppyg     3      broken
#> 270  WQ2        ppyg     5       fresh
#> 271  WQ2        ppyg    22       fresh
#> 272  WQ2        ppyg    26      broken
#> 273  WQ2        pvic     1       fresh
#> 274  WQ2        tuni     2       fresh
#> 275  WQ3        amin     3      broken
#> 276  WQ3        amin     7       fresh
#> 277  WQ3        bbip     1      broken
#> 278  WQ3        bbip     2       fresh
#> 279  WQ3        estr     1      broken
#> 280  WQ3        ffau     2      broken
#> 281  WQ3        pinc     1       fresh
#> 282  WQ3        ppyg     2      broken
#> 283  WQ3        ppyg     3       fresh
#> 284  WQ3        vicr     1       fresh
#> 285  WQ3        vidi     1      broken
#> 286  WT1        amin     1      broken
#> 287  WT1        cort     1       fresh
#> 288  WT1        estr     1      broken
#> 289  WT1        ffau     1       fresh
#> 290  WT1        tuni     1       fresh
#> 291  WT2        amin     2       fresh
#> 292  WT2        apur     1       fresh
#> 293  WT2        bbip     1      broken
#> 294  WT2        dper     2       fresh
#> 295  WT3        amin     4      broken
#> 296  WT3        ffau     1      broken
#> 297  WT3        vidi     1       fresh
plot(dol)
```
