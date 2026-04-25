# Conversion Among Various Object Classes

These functions coerce into class 'mefa' or 'stcs'; or converts 'mefa'
or 'stcs' obects into a 'data.frame', 'matrix', 'array', 'list'.

## Usage

``` r
as.stcs(x, ...)
# Default S3 method
as.stcs(x, ...)
as.mefa(x, ...)
as.Mefa(x, ...)
as.Xtab(x, ...)
# Default S3 method
as.mefa(x, samp, taxa, ...)
# S3 method for class 'array'
as.mefa(x, ...)
# S3 method for class 'list'
as.mefa(x, ...)
# S3 method for class 'mefa'
as.matrix(x, ...)
# S3 method for class 'mefa'
as.array(x, ...)
# S3 method for class 'mefa'
as.list(x, ...)
# S3 method for class 'stcs'
as.data.frame(x, ...)
# S3 method for class 'mefa'
as.data.frame(x, ..., fun, name, make.unique = FALSE)
mss(x, name, make.unique = FALSE, ...)
msr(x, name, make.unique = FALSE, ...)
mts(x, name, make.unique = FALSE, ...)
mtr(x, name, make.unique = FALSE, ...)
```

## Arguments

- x:

  an object of class 'mefa'.

- samp:

  a data frame containing rows for samples, or `NULL`. See argument
  `xtab.fixed` and details for sample matching options.

- taxa:

  a data frame containing rows for taxa, or `NULL`. See argument
  `xtab.fixed` and details for taxa matching options.

- fun:

  a function to determine what to return, see details. It has no
  default, because the choice is not trivial.

- name:

  optional character vector, names to return if not all possible
  elements are needed, see details.

- make.unique:

  logical, useful to use `make.unique = TRUE` if names to return are
  non-unique, see details.

- ...:

  other arguments passed to the generic function
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

## Details

The usage of most of these coercion methods is trivial. Arrays and lists
can be coerced into 'mefa' objects as if those were the `segm` element
of it (either nested, or non nested). The reverse is true, when coercing
an object of class 'mefa' into list or array, the `segm` element is
used.

Coercing among object classes 'matrix' and 'mefa' is done via using the
`xtab` element.

By `as.data.frame.mefa`, four kinds of data frames can be returned,
depending on the function used as the `fun` argument: `mss` returns
summary statistics for samples (richness and abundance) and `x$samp`;
`msr` returns raw data (`x$xtab`) and `x$samp`; `mts` returns summary
statistics for taxa (occurrence and abundance) and `x$taxa`; `mtr`
returns raw data (`t(x$xtab)`) and `x$taxa`.

The `name` can be used if only a subset of the summary statistics, or
raw data should be returned. The character vector should contain names
of elements to return (see examples).

It might (rarely) occur, that names of the summary statistics, or the
raw data and the column names of the sample/taxa table overlap (contains
names that are common in the two set). In this case, the
`make.unique = TRUE` can be used to resolve non-uniqueness and avoid the
error message produced otherwise.

The functions supplied as the `fun` argument can be used separately.
Although the usage of the `as.data.frame` method is more transparent.

## Value

An object of class 'stcs', 'mefa', 'matrix', 'array', 'list' or
'data.frame', depending on the method used.

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

[`mefa`](mefa.md), [`stcs`](stcs.md),
[`as.matrix`](https://rdrr.io/r/base/matrix.html),
[`as.list`](https://rdrr.io/r/base/list.html),
[`as.array`](https://rdrr.io/r/base/array.html),
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)

## Examples

``` r
data(dol.count, dol.samp, dol.taxa)
x <- mefa(stcs(dol.count), dol.samp, dol.taxa)
## These two are equivalent
as.data.frame(x, fun = mss)
#>     s.rich s.abu  microhab  method
#> DQ1      5    18 dead.wood quadrat
#> DQ2      7    39 dead.wood quadrat
#> DQ3      4     9 dead.wood quadrat
#> DT1      7    11 dead.wood    time
#> DT2     10    13 dead.wood    time
#> DT3      5    13 dead.wood    time
#> LQ1      5    34    litter quadrat
#> LQ2      5    17    litter quadrat
#> LQ3      7    82    litter quadrat
#> LT1      0     0    litter    time
#> LT2      2     2    litter    time
#> LT3      5    10    litter    time
#> RQ1     10    23      rock quadrat
#> RQ2     16    97      rock quadrat
#> RQ3     13    63      rock quadrat
#> RT1     11    44      rock    time
#> RT2     15    47      rock    time
#> RT3     10    30      rock    time
#> WQ1      9    48 live.wood quadrat
#> WQ2     10    90 live.wood quadrat
#> WQ3      8    24 live.wood quadrat
#> WT1      5     5 live.wood    time
#> WT2      4     6 live.wood    time
#> WT3      3     6 live.wood    time
mss(x)
#>     s.rich s.abu  microhab  method
#> DQ1      5    18 dead.wood quadrat
#> DQ2      7    39 dead.wood quadrat
#> DQ3      4     9 dead.wood quadrat
#> DT1      7    11 dead.wood    time
#> DT2     10    13 dead.wood    time
#> DT3      5    13 dead.wood    time
#> LQ1      5    34    litter quadrat
#> LQ2      5    17    litter quadrat
#> LQ3      7    82    litter quadrat
#> LT1      0     0    litter    time
#> LT2      2     2    litter    time
#> LT3      5    10    litter    time
#> RQ1     10    23      rock quadrat
#> RQ2     16    97      rock quadrat
#> RQ3     13    63      rock quadrat
#> RT1     11    44      rock    time
#> RT2     15    47      rock    time
#> RT3     10    30      rock    time
#> WQ1      9    48 live.wood quadrat
#> WQ2     10    90 live.wood quadrat
#> WQ3      8    24 live.wood quadrat
#> WT1      5     5 live.wood    time
#> WT2      4     6 live.wood    time
#> WT3      3     6 live.wood    time
## Return only two species
as.data.frame(x, fun = msr, c("iiso", "ppyg"))
#>     aacu amin apur bbip bcan ccer clam cort ctri dbre dper druf eful estr ffau
#> DQ1    0    4    0    0    0    0    0    0    2    0    2    0    0    1    0
#> DQ2    0    4    1    0    0    0    0    0   20    0    2    2    0    0    0
#> DQ3    0    3    1    0    0    0    0    0    0    0    0    0    0    0    0
#> DT1    0    1    0    0    1    0    0    0    0    0    1    0    0    1    0
#> DT2    0    2    1    1    0    0    2    0    0    0    1    1    0    1    0
#> DT3    0    3    0    1    0    0    0    0    0    0    0    0    0    0    0
#> LQ1    1    3    0    0    0    0    0    0    0    0    1    0    0    0    1
#> LQ2    1    1    0    0    0    0    0    0    0    0    1    0    0    0    0
#> LQ3    0    2    1    1    0    1    0    0    0    0    7    0    0    0    0
#> LT1    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#> LT2    0    1    0    0    0    0    0    0    0    0    0    0    0    0    0
#> LT3    0    5    0    0    0    0    0    0    0    0    1    0    0    0    0
#> RQ1    0    3    0    0    0    1    1    1    0    0    3    0    0    0    2
#> RQ2    0    7    0    4    0    0    0    0    7    1    6    0    0    3    2
#> RQ3    2   13    0    1    0    0    0    0   10    3    6    0    2    0    2
#> RT1    0    4    0    3    0    2    0    1    0    0    1    0    0    0    7
#> RT2    0    1    2    3    1    0    0    1    1    0    2    0    0    4   12
#> RT3    0    5    1    5    0    0    1    0    0    0    0    0    0    0    6
#> WQ1    0    3    0    1    1    0    0    3    0    0    0    0    0    0    0
#> WQ2    0    4    0    1    0    1    0    0    0    0   20    0    0    2    0
#> WQ3    0   10    0    3    0    0    0    0    0    0    0    0    0    1    2
#> WT1    0    1    0    0    0    0    0    1    0    0    0    0    0    1    1
#> WT2    0    2    1    1    0    0    0    0    0    0    2    0    0    0    0
#> WT3    0    4    0    0    0    0    0    0    0    0    0    0    0    0    1
#>     hobv iiso mbor mobs odol ogla pinc ppyg pvic tuni vcos vicr vidi  microhab
#> DQ1    0    0    0    0    0    0    0    0    0    0    0    0    9 dead.wood
#> DQ2    0    0    0    0    0    0    0    1    0    0    0    0    9 dead.wood
#> DQ3    3    0    0    0    0    0    0    0    0    0    0    0    2 dead.wood
#> DT1    1    0    5    0    0    0    0    0    1    0    0    0    0 dead.wood
#> DT2    1    0    0    0    0    0    0    0    0    1    0    0    2 dead.wood
#> DT3    6    0    0    0    0    0    0    0    1    0    0    0    2 dead.wood
#> LQ1    0    0    0    0    0    0    0   28    0    0    0    0    0    litter
#> LQ2    0    0    0    0    0    0    0   13    1    0    0    0    0    litter
#> LQ3    0    0    0    0    0    0    0   69    1    0    0    0    0    litter
#> LT1    0    0    0    0    0    0    0    0    0    0    0    0    0    litter
#> LT2    0    0    0    0    0    0    0    0    1    0    0    0    0    litter
#> LT3    0    0    0    0    0    0    1    0    1    2    0    0    0    litter
#> RQ1    2    0    0    0    0    1    0    5    0    0    0    0    4      rock
#> RQ2    9    0    0    2   13    3    0   28    1    2    2    0    7      rock
#> RQ3    2    0    0    1    0    0    0    9    1    0    0    0   11      rock
#> RT1    8    6    0    0    0    0    3    0    4    5    0    0    0      rock
#> RT2    1    8    0    0    0    0    5    0    1    3    0    0    2      rock
#> RT3    4    1    0    0    0    0    3    0    3    1    0    0    0      rock
#> WQ1    0    2    0    0    0    0    2   31    1    0    0    0    4 live.wood
#> WQ2    0    0    0    2    0    0    1   56    1    2    0    0    0 live.wood
#> WQ3    0    0    0    0    0    0    1    5    0    0    0    1    1 live.wood
#> WT1    0    0    0    0    0    0    0    0    0    1    0    0    0 live.wood
#> WT2    0    0    0    0    0    0    0    0    0    0    0    0    0 live.wood
#> WT3    0    0    0    0    0    0    0    0    0    0    0    0    1 live.wood
#>      method <NA>
#> DQ1 quadrat iiso
#> DQ2 quadrat ppyg
#> DQ3 quadrat iiso
#> DT1    time ppyg
#> DT2    time iiso
#> DT3    time ppyg
#> LQ1 quadrat iiso
#> LQ2 quadrat ppyg
#> LQ3 quadrat iiso
#> LT1    time ppyg
#> LT2    time iiso
#> LT3    time ppyg
#> RQ1 quadrat iiso
#> RQ2 quadrat ppyg
#> RQ3 quadrat iiso
#> RT1    time ppyg
#> RT2    time iiso
#> RT3    time ppyg
#> WQ1 quadrat iiso
#> WQ2 quadrat ppyg
#> WQ3 quadrat iiso
#> WT1    time ppyg
#> WT2    time iiso
#> WT3    time ppyg
## Taxa table and summary
as.data.frame(x, fun = mts)
#>      t.occ t.abu                      species                       author
#> aacu     3     4         Acanthinula aculeata         (O. F. Muller, 1774)
#> amin    23    86            Aegopinella minor              (Stabile, 1864)
#> apur     7     8             Aegopinella pura                (Alder, 1830)
#> bbip    12    25              Balea biplicata              (Montagu, 1803)
#> bcan     3     3               Bulgarica cana                 (Held, 1836)
#> ccer     4     5            Cochlodina cerata          (Rossmassler, 1836)
#> clam     3     4          Cochlodina laminata              (Montagu, 1803)
#> cort     5     7        Cochlodina orthostoma                (Menke, 1830)
#> ctri     5    40        Carychium tridentatum                (Risso, 1826)
#> dbre     2     4         Daudebardia brevipes           (Draparnaud, 1805)
#> dper    15    56          Discus perspectivus (Megerle von Muhlfeld, 1816)
#> druf     2     3             Daudebardia rufa           (Draparnaud, 1805)
#> eful     1     2             Euconulus fulvus         (O. F. Muller, 1774)
#> estr     8    14         Euomphalia strigella           (Draparnaud, 1801)
#> ffau    10    36         Chilostoma faustinum          (Rossmassler, 1835)
#> hobv    10    37         Helicodonta obvoluta         (O. F. Muller, 1774)
#> iiso     4    17 Isognomostoma isognomostomos             (Schroter, 1784)
#> mbor     1     5      Macrogastra latestriata           (A. Schmidt, 1857)
#> mobs     3     5                  Ena obscura         (O. F. Muller, 1774)
#> odol     1    13                Orcula dolium           (Draparnaud, 1801)
#> ogla     2     4             Oxychilus glaber          (Rossmassler, 1838)
#> pinc     7    16       Perforatella incarnata         (O. F. Muller, 1774)
#> ppyg    10   245             Punctum pygmaeum           (Draparnaud, 1801)
#> pvic    13    18          Perforatella vicina          (Rossmassler, 1842)
#> tuni     8    17           Trichia unidentata           (Draparnaud, 1805)
#> vcos     1     2             Vallonia costata         (O. F. Muller, 1774)
#> vicr     1     1           Vitrea crystallina         (O. F. Muller, 1774)
#> vidi    12    54              Vitrea diaphana               (Studer, 1820)
#>           familia size
#> aacu  Valloniidae  2.0
#> amin    Zonitidae  9.0
#> apur    Zonitidae  5.0
#> bbip Clausiliidae 18.0
#> bcan Clausiliidae 18.0
#> ccer Clausiliidae 18.0
#> clam Clausiliidae 17.0
#> cort Clausiliidae 13.0
#> ctri   Ellobiidae  2.3
#> dbre    Zonitidae   NA
#> dper Endodontidae  6.5
#> druf    Zonitidae   NA
#> eful  Euconulidae  3.5
#> estr    Helicidae 18.0
#> ffau    Helicidae 20.0
#> hobv    Helicidae 15.0
#> iiso    Helicidae 11.0
#> mbor Clausiliidae 15.0
#> mobs       Enidae  9.0
#> odol    Orculidae  9.0
#> ogla    Zonitidae 14.0
#> pinc    Helicidae 16.0
#> ppyg Endodontidae  1.5
#> pvic    Helicidae 15.5
#> tuni    Helicidae  8.0
#> vcos  Valloniidae  2.7
#> vicr    Zonitidae  4.0
#> vidi    Zonitidae  4.2
## Taxa table and raw data transpose
as.data.frame(x, fun = mtr)
#>      DQ1 DQ2 DQ3 DT1 DT2 DT3 LQ1 LQ2 LQ3 LT1 LT2 LT3 RQ1 RQ2 RQ3 RT1 RT2 RT3
#> aacu   0   0   0   0   0   0   1   1   0   0   0   0   0   0   2   0   0   0
#> amin   4   4   3   1   2   3   3   1   2   0   1   5   3   7  13   4   1   5
#> apur   0   1   1   0   1   0   0   0   1   0   0   0   0   0   0   0   2   1
#> bbip   0   0   0   0   1   1   0   0   1   0   0   0   0   4   1   3   3   5
#> bcan   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   1   0
#> ccer   0   0   0   0   0   0   0   0   1   0   0   0   1   0   0   2   0   0
#> clam   0   0   0   0   2   0   0   0   0   0   0   0   1   0   0   0   0   1
#> cort   0   0   0   0   0   0   0   0   0   0   0   0   1   0   0   1   1   0
#> ctri   2  20   0   0   0   0   0   0   0   0   0   0   0   7  10   0   1   0
#> dbre   0   0   0   0   0   0   0   0   0   0   0   0   0   1   3   0   0   0
#> dper   2   2   0   1   1   0   1   1   7   0   0   1   3   6   6   1   2   0
#> druf   0   2   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0
#> eful   0   0   0   0   0   0   0   0   0   0   0   0   0   0   2   0   0   0
#> estr   1   0   0   1   1   0   0   0   0   0   0   0   0   3   0   0   4   0
#> ffau   0   0   0   0   0   0   1   0   0   0   0   0   2   2   2   7  12   6
#> hobv   0   0   3   1   1   6   0   0   0   0   0   0   2   9   2   8   1   4
#> iiso   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   6   8   1
#> mbor   0   0   0   5   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> mobs   0   0   0   0   0   0   0   0   0   0   0   0   0   2   1   0   0   0
#> odol   0   0   0   0   0   0   0   0   0   0   0   0   0  13   0   0   0   0
#> ogla   0   0   0   0   0   0   0   0   0   0   0   0   1   3   0   0   0   0
#> pinc   0   0   0   0   0   0   0   0   0   0   0   1   0   0   0   3   5   3
#> ppyg   0   1   0   0   0   0  28  13  69   0   0   0   5  28   9   0   0   0
#> pvic   0   0   0   1   0   1   0   1   1   0   1   1   0   1   1   4   1   3
#> tuni   0   0   0   0   1   0   0   0   0   0   0   2   0   2   0   5   3   1
#> vcos   0   0   0   0   0   0   0   0   0   0   0   0   0   2   0   0   0   0
#> vicr   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> vidi   9   9   2   0   2   2   0   0   0   0   0   0   4   7  11   0   2   0
#>      WQ1 WQ2 WQ3 WT1 WT2 WT3                      species
#> aacu   0   0   0   0   0   0         Acanthinula aculeata
#> amin   3   4  10   1   2   4            Aegopinella minor
#> apur   0   0   0   0   1   0             Aegopinella pura
#> bbip   1   1   3   0   1   0              Balea biplicata
#> bcan   1   0   0   0   0   0               Bulgarica cana
#> ccer   0   1   0   0   0   0            Cochlodina cerata
#> clam   0   0   0   0   0   0          Cochlodina laminata
#> cort   3   0   0   1   0   0        Cochlodina orthostoma
#> ctri   0   0   0   0   0   0        Carychium tridentatum
#> dbre   0   0   0   0   0   0         Daudebardia brevipes
#> dper   0  20   0   0   2   0          Discus perspectivus
#> druf   0   0   0   0   0   0             Daudebardia rufa
#> eful   0   0   0   0   0   0             Euconulus fulvus
#> estr   0   2   1   1   0   0         Euomphalia strigella
#> ffau   0   0   2   1   0   1         Chilostoma faustinum
#> hobv   0   0   0   0   0   0         Helicodonta obvoluta
#> iiso   2   0   0   0   0   0 Isognomostoma isognomostomos
#> mbor   0   0   0   0   0   0      Macrogastra latestriata
#> mobs   0   2   0   0   0   0                  Ena obscura
#> odol   0   0   0   0   0   0                Orcula dolium
#> ogla   0   0   0   0   0   0             Oxychilus glaber
#> pinc   2   1   1   0   0   0       Perforatella incarnata
#> ppyg  31  56   5   0   0   0             Punctum pygmaeum
#> pvic   1   1   0   0   0   0          Perforatella vicina
#> tuni   0   2   0   1   0   0           Trichia unidentata
#> vcos   0   0   0   0   0   0             Vallonia costata
#> vicr   0   0   1   0   0   0           Vitrea crystallina
#> vidi   4   0   1   0   0   1              Vitrea diaphana
#>                            author      familia size
#> aacu         (O. F. Muller, 1774)  Valloniidae  2.0
#> amin              (Stabile, 1864)    Zonitidae  9.0
#> apur                (Alder, 1830)    Zonitidae  5.0
#> bbip              (Montagu, 1803) Clausiliidae 18.0
#> bcan                 (Held, 1836) Clausiliidae 18.0
#> ccer          (Rossmassler, 1836) Clausiliidae 18.0
#> clam              (Montagu, 1803) Clausiliidae 17.0
#> cort                (Menke, 1830) Clausiliidae 13.0
#> ctri                (Risso, 1826)   Ellobiidae  2.3
#> dbre           (Draparnaud, 1805)    Zonitidae   NA
#> dper (Megerle von Muhlfeld, 1816) Endodontidae  6.5
#> druf           (Draparnaud, 1805)    Zonitidae   NA
#> eful         (O. F. Muller, 1774)  Euconulidae  3.5
#> estr           (Draparnaud, 1801)    Helicidae 18.0
#> ffau          (Rossmassler, 1835)    Helicidae 20.0
#> hobv         (O. F. Muller, 1774)    Helicidae 15.0
#> iiso             (Schroter, 1784)    Helicidae 11.0
#> mbor           (A. Schmidt, 1857) Clausiliidae 15.0
#> mobs         (O. F. Muller, 1774)       Enidae  9.0
#> odol           (Draparnaud, 1801)    Orculidae  9.0
#> ogla          (Rossmassler, 1838)    Zonitidae 14.0
#> pinc         (O. F. Muller, 1774)    Helicidae 16.0
#> ppyg           (Draparnaud, 1801) Endodontidae  1.5
#> pvic          (Rossmassler, 1842)    Helicidae 15.5
#> tuni           (Draparnaud, 1805)    Helicidae  8.0
#> vcos         (O. F. Muller, 1774)  Valloniidae  2.7
#> vicr         (O. F. Muller, 1774)    Zonitidae  4.0
#> vidi               (Studer, 1820)    Zonitidae  4.2
## Why is it useful?
## Instead of
glm(x$xtab[,"amin"] ~ microhab + method, data = x$samp, family = poisson)
#> 
#> Call:  glm(formula = x$xtab[, "amin"] ~ microhab + method, family = poisson, 
#>     data = x$samp)
#> 
#> Coefficients:
#>       (Intercept)     microhablitter  microhablive.wood       microhabrock  
#>            0.6475            -0.3483             0.3448             0.6633  
#>     methodquadrat  
#>            0.6758  
#> 
#> Degrees of Freedom: 23 Total (i.e. Null);  19 Residual
#> Null Deviance:       49.38 
#> Residual Deviance: 28.51     AIC: 107
## it is more convenient to use
glm(amin ~ microhab + method, data = msr(x), family = poisson)
#> 
#> Call:  glm(formula = amin ~ microhab + method, family = poisson, data = msr(x))
#> 
#> Coefficients:
#>       (Intercept)     microhablitter  microhablive.wood       microhabrock  
#>            0.6475            -0.3483             0.3448             0.6633  
#>     methodquadrat  
#>            0.6758  
#> 
#> Degrees of Freedom: 23 Total (i.e. Null);  19 Residual
#> Null Deviance:       49.38 
#> Residual Deviance: 28.51     AIC: 107
```
