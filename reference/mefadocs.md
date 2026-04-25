# Display Package Documentation

Display package documentation using `pager` or `pdfviewer` defined in
[`options`](https://rdrr.io/r/base/options.html).

## Usage

``` r
mefadocs(doc = c("ChangeLog","SampleReport"))
```

## Arguments

- doc:

  The name of the document (partial match, case sensitive).

## Details

`"ChangeLog"` contains version history, `"SampleReport"` shows an
example LaTeX document how to use [`report.mefa`](report.mefa.md) to
prepare [`Sweave`](https://rdrr.io/r/utils/Sweave.html) reports based on
faunistic data.

## Author

P\\eter S\\olymos (<solymos@ualberta.ca>) adapted Jari Oksanen's
[`vegandocs`](https://vegandevs.github.io/vegan/reference/vegan-defunct.html)
function in the **vegan** package.

## Examples

``` r
if (FALSE) { # \dontrun{
mefadocs("Change")
mefadocs("Sample")
} # }
```
