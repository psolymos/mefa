# mefa: multivariate data handling in ecology and biogeography

[![CRAN version](https://www.r-pkg.org/badges/version/mefa)](https://cran.rstudio.com/web/packages/mefa/index.html)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/mefa)](https://cran.rstudio.com/web/packages/mefa/index.html)
[![check](https://github.com/psolymos/mefa/actions/workflows/check.yml/badge.svg)](https://github.com/psolymos/mefa/actions/workflows/check.yml)

This is the imported version of the mefa R package from the [mefa project on R-Forge](https://r-forge.r-project.org/projects/mefa/) (rev 619).

Mefa is a framework package aimed to provide standardized computational environment for specialist work via object classes to represent the data coded by samples, taxa and segments (i.e. subpopulations, repeated measures). It supports easy processing of the data along with cross tabulation and relational data tables for samples and taxa. An object of class mefa is a project specific compendium of the data and can be easily used in further analyses. Methods are provided for extraction, aggregation, conversion, plotting, summary and reporting of `mefa' objects. Reports can be generated in plain text or LaTeX format. Vignette contains worked examples.

## Versions

Install CRAN version as:

```R
install.packages("mefa")
```

Install development version from GitHub:

```R
remotes::install_github("psolymos/mefa")
```

## Report a problem

Use the [issue tracker](https://github.com/psolymos/mefa/issues)
to report a problem.

## References

Solymos P. 2008. mefa: an R package for handling and reporting count data.
_Community Ecology_ **9**, 125-127. ([PDF](https://drive.google.com/file/d/0B-q59n6LIwYPdWVkWlQ1ZzFMS3c/view?usp=sharing))

Solymos P. 2009. Processing ecological data in R with the mefa package.
_Journal of Statistical Software_ **29(8)**, 1-28.
DOI: [10.18637/jss.v029.i08](https://doi.org/10.18637/jss.v029.i08)
