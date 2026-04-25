# Write Report of an Object of Class 'mefa' into File

The data of the 'mefa' object is written into a file in a standard
format used by most local journals for publishing distribution data of
organisms. The aim is to provide an exporting facility that can directly
be used for generating reports, parts of research papers, or
supplementary materials.

## Usage

``` r
report(x, ...)
# S3 method for class 'mefa'
report(x, filename, segment = FALSE, n = NULL,
    by.taxa = TRUE, samp.var = NULL, drop.redundant = NULL,
    collapse = TRUE, taxa.name = NULL, author.name = NULL,
    taxa.order = NULL, grouping = FALSE, tex = FALSE, binary = FALSE,
    tex.control = list(ital.taxa = TRUE, noindent = TRUE, 
    bold.sect = TRUE, bold.1st = TRUE, vspace1 = 0.5, vspace2 = 0.2),
    sep = c(",", ":", "(", ":", ",", ")", ";"), dir = getwd(), ...)
```

## Arguments

- x:

  an object of class 'mefa'.

- filename:

  character, the name of the file to be written into the working
  directory or a directory defined by the `outdir` argument.

- segment:

  logical, if `NULL` (default) segments are not distinguished in the
  output. If `TRUE` all (`n = NULL`) or some of the segments are used
  and distinguished.

- n:

  segments to be used. If `NULL` all segments are used, if not `NULL`,
  give names of the segments to be used (only if `segment = TRUE`).

- by.taxa:

  logical, whether the main sectioning should be made according to taxa
  (`TRUE`, default) or samples (`FALSE`, currently not implemented).

- samp.var:

  variables in the samples table (`x$samp`) to be used in the report.
  Variables appear in the order given here.

- drop.redundant:

  logical, whether redundant elements should be dropped (an integer up
  to `length(samp.var)-1`) or not (`NULL`, default).

- collapse:

  logical, if `TRUE` samples with identical values in `samp.var` for a
  given species are collapsed to avoid redundancy.

- taxa.name:

  column in the taxa table (`x$taxa`) where the taxa names are stored.
  By default (`NULL`) `dimnames(x)$taxa` is used.

- author.name:

  column in the taxa table (`x$taxa`) where the authors' names and dates
  of descriptions are stored. By default it is not used (`NULL`). If
  specified, these follow the species names in the report.

- taxa.order:

  column in the taxa table (`x$taxa`) where the variable for ordering is
  stored. By default (`NULL`) `taxa.name` is used for ordering.

- grouping:

  logical, whether all records of a given species should be collapsed
  into single paragraph without grouping (`FALSE`, default), or grouped
  into separate paragraphs (`TRUE`).

- tex:

  logical, whether to generate formatted LaTeX output (`TRUE`) or plain
  text file (`FALSE`).

- binary:

  logical, if `TRUE` the result will contains count data.

- tex.control:

  a list with LaTex formatting options (if `tex = TRUE`). The
  `ital.taxa` controls italicised taxa names, `noindent` controls
  indentation of paragraphs, `bold.sect` controls boldface type of
  paragraph openings, `bold.1st` controls the boldface type of first
  elements of each record of a given species, `vspace1` controls the
  space between species blocks, `vspace2` controls the space between
  paragraphs of the same species (if `gouping = FALSE`).

- sep:

  character vector of length 7. The 1st separates values in `samp.var`,
  2nd is placed between values of `samp.var` if redundant elements were
  dropped, 3rd is the opening character before count data, 4th stands
  after the segment name, 5th separates values of segments, 6th is the
  closing character after the count data, 7th separates the records.

- dir:

  character, full path of the directory where the file should be written
  (working directory is then restored). If `NULL`, the current working
  directory is used.

- ...:

  further arguments potentially passed (currently there are none).

## Details

The resulting file can be recycled by copy-and-pasting into a document
(if `tex = FALSE`) or be included into a LaTeX report (e.g. via the
package `Sweave`, see `mefadocs("SampleReport")`). Contrary to the many
arguments, the default values do not need much modifications in most of
the cases (see examples).

## Value

Writes a file into the working (or a specified) directory.

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

The `by.taxa = FALSE` option is not yet implemented.

## See also

[`mefa`](mefa.md), [`write`](https://rdrr.io/r/base/write.html)

## Examples

``` r
data(dol.count, dol.samp, dol.taxa)
x <- mefa(stcs(dol.count), dol.samp, dol.taxa)
if (FALSE) { # \dontrun{
## Plain text
## Count values from x$xtab
report(x, "report-all.txt")
## Count values for each segments
report(x, "report-segm-all.txt",
    samp.var = c("method","microhab"), segment = TRUE)
## LaTeX formatting
report(x, "report-all.tex", tex=TRUE)
## For how to include into a TeX or Rnw file, see:
mefadocs("SampleReport")
} # }
```
