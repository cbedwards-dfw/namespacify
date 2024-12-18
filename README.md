
<!-- README.md is generated from README.Rmd. Please edit that file -->

# namespacify

<!-- badges: start -->
<!-- badges: end -->

The goal of namespacify is to automate the addition of namespace
prefixes (e.g. “dplyr::”) to code to facilitate converting scripts to
packages.

## Installation

You can install the development version of namespacify like so:

``` r
remotes::install_github("cbedwards-dfw/namespacify")
```

or

``` r
pak::pkg_install("cbedwards-dfw/namespacify")
```

## Example

When developing the `CreelEstimateR` package, we already had functions
written, but as is common for non-package scripts, the R code did not
include the package prefixes. We were able to rapidly add prefixes for
the packages we used by first adding all the functions to the `R/`
folder, and then running the following code:

``` r
namespacify(packages = c("dplyr", "ggplot2", "purrr", "tidyr", "DBI", "readr", "stringr", "tidyselect"))
```

To apply package prefixes to all R, Qmd, and Rmd files in a specific
folder (instead of to `here::here("R")` as is the default), you can
provide the path to the folder with the `folder` argument.

``` r
namespacify(packages = c("dplyr", "ggplot2", "purrr", "tidyr", "DBI", "readr", "stringr", "tidyselect"),
folder = "analysis/scripts")
```

To add namespace prefixes to just a specific file or collection of
files, provide the filepath or a vector of filepaths in the `folder`
argument

``` r
## single file
namespacify(packages = c("dplyr", "ggplot2", "purrr", "tidyr", "DBI", "readr", "stringr", "tidyselect"),
folder = "analysis/scripts/cleaning_data.R")

namespacify(packages = c("dplyr", "ggplot2", "purrr", "tidyr", "DBI", "readr", "stringr", "tidyselect"),
folder = c("analysis/scripts/cleaning_data.R","analysis/scripts/fitting_models.R"))
```

### Identifying packages

If you do not know the list of appropriate packages already,
`namespacify::package_detect()` will search the provided `folder`
argument for any use of `library()` and `require()`, and return the
associated packages. It will also suggest adding those packages to your
package’s namespace with `usethis::use_package()`, and will load an
appropriate function call to your clipboard.
