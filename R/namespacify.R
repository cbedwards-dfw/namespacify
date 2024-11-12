#' Apply namespace prefixes to one or more files
#'
#' Workhorse function. There is special handling for "function collisions" -- cases when two or more specified
#' packages have one or more functions with the same name. If `error.on.collision` is set to TRUE,
#' collisions will cause `namspacify` to identify collisions, and then stop.
#' Users can then specify individual functions to ignore with the `funs.ignore` argument.
#' This is the safest approach to ambiguity.
#'
#' For example, \{dplyr\} and \{stats\} both have a `filter()` function. `namespacify()`
#' doesn't know whether to replace `filter(` with `dplyr::filter(` or `stats::filter(`.
#' For this reason, `namespacify()` will error unless a user included "filter" in
#' the `funs.ignore` argument. Then the user can manually add the appropriate prefix within their scripts,
#' identifying the specific package in each case.
#'
#' In cases where collisions are common and anticipated, users can set `error.on.collision`
#' to FALSE. In this case, `namespacify()` will instead provide a warning, and then in cases
#' of collisions will use the first relevant package in the `packages` argument. This may be useful
#' when including packages that reexport functions. In that case, two packages
#' may have identical functions with the same name, and the choice of package doesn't matter; setting `error.on.collision` to
#' FALSE will allow the assignment of a relevant namespace prefix to all functions. (Note that reexporting among
#' the tidyverse packages is already addressed; see details see details).
#'
#' @details Dev note:  The Tidyverse packages frequently re-export functions (e.g., dplyr includes functions from the tibble package).
#' This means that including multiple tidyverse packages will automatically lead to "function collisions",
#' even though they are identical versions of the function, so the choice of namespace doesn't matter.
#' Right now I have a workaround that skips these internal-to-tidyverse collisions -- these within-tidyverse
#' collisions are not counted as "function collisions" by `namespace`. However, my workaround
#' is janky -- in cases of within-tidyverse collisions it uses the first relevant tidyverse package listed
#' in the `packages` argument.A cleaner solution would be to identify which package is being rexported *from*, and use that.
#' I could not identify an easy way to do so.
#'
#' @param packages character string of packages to apply namespace prefixes for. In the specified files (below), all functions used by these packages will be the package prefix (e.g. "dplyr::")
#' @param error.on.collision If two or more packages share a function name (e.g., \{stats\} and \{dplyr\}), should the function error (TRUE) or warn (FALSE)
#' @param funs.ignore Character vector of functions to skip when adding namespace prefixes as a solution to function collisions. Defaults to NULL.
#' @param verbose Print details to console? Defaults to TRUE.
#' @param folder Identify the folder or files to apply namespace prefixes to. If Defaults to the "./R/" folder, which is where the R files of an R package live.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' namespacify(packages = c("dplyr", "ggplot2", "purrr", "tidyr",
#' "DBI", "readr", "stringr", "tidyselect"))
#' }
namespacify = function(
    packages,
    error.on.collision = TRUE,
    funs.ignore = NULL,
    verbose = TRUE,
    folder = NULL
){
  if(!is.na(folder) | is.character(folder)){
    cli::cli_abort("`folder` argument must be NULL, a character string of a filepath to a directory, or a character string of one or more files.")
  }
  vec.replacement = make_prefix_vector(packages = packages,
                                       error.on.collision = error.on.collision,
                                       funs.ignore = funs.ignore,
                                       verbose = verbose
  )
  if(is.null(folder)){
    r_files <- fs::dir_ls(here::here("R"))
  }else{
    if(length(folder == 1) & dir.exists(folder)){
      if(verbose){
        cli::cli_alert("`folder` argument appears to be folder. Namespacifying all R, Rmd, and Qmd files in folder")
      }
      r_files <- fs::dir_ls(folder)
      r_files = grep(".R$|.Rmd$|.Qmd", r_files, value = TRUE, ignore.case = TRUE)
    }else{
      if(verbose){
        cli::cli_alert("`folder` argument is not a folder. Assuming it's a filename or vector of filenames. Namespacifying this/these file(s), constraining to only .R, .Rmd, and .Qmd filetypes.")
      }
      r_files = folder
      r_files = grep(".R$|.Rmd$|.Qmd", r_files, value = TRUE, ignore.case = TRUE)
    }
  }
  purrr::walk(r_files, \(x) namespacify_file(file = x, vec.replacement = vec.replacement, verbose = verbose))
}
## updates:
##  maybe have the replacement vector run a single time as a separate function
##  add collision detection and either warning or errors (optional arg to change that)

#' Title
#'
#' @inheritParams namespacify
#'
#' @return Vector of patterns to replace; vector names are what should be replaced. Designed to work with `stringr::string_replace_all()`.
#' @export
#'
#' @examples
#' \dontrun{
#' make_prefix_vector(packages = c("dplyr", "ggplot2"))
#' }
make_prefix_vector = function(packages,
                              error.on.collision = TRUE,
                              funs.ignore = NULL,
                              verbose = TRUE){
  if(verbose){
    if(is.null(funs.ignore)){
      cli::cli_alert("Not skipping any functions. To specify functions not to assign namespace prefixes to, use `funs.ignore` argument.")
    }else{
      cli::cli_alert("Set to ignore the skip namespace prefix addition for the following functions: {funs.ignore}")
    }
  }

  df.initial = NULL
  for(cur.package in packages){
    vec.funs = sort(getNamespaceExports(cur.package))
    if(!is.null(funs.ignore)){
      vec.funs = vec.funs[!vec.funs %in% funs.ignore]
    }
    ## remove functions with funky chars
    vec.funs = vec.funs[!stringr::str_detect(vec.funs, "[%]|[<]")]
    ## remove .data object types
    vec.funs = vec.funs[!stringr::str_detect(vec.funs, "^[.]")]
    ##handle functions with periods in them
    df.initial = rbind(df.initial,
                       data.frame(package = cur.package,
                                  fun = vec.funs))
  }
  df.initial = df.initial |>
    dplyr::group_by(.data$fun) |>
    dplyr::summarize(packages = list(.data$package)) |>
    dplyr::relocate(.data$packages, .data$fun)

  ## Address tidyverse duplication
  tidyverse_helper = function(x){
    packages.tidyverse = c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats", "tidyselect")
    tidy.exclude = x[x %in% packages.tidyverse][-1]
    x = x[! x %in% tidy.exclude]
  }
  df.initial = df.initial |>
    dplyr::mutate(packages = purrr::map(.data$packages, tidyverse_helper)) |>
    dplyr::mutate(num.packages = purrr::map_dbl(.data$packages, length))

  fun.collisions = df.initial |>
    dplyr::filter(.data$num.packages>1)

  if(nrow(fun.collisions)>0){
    if(error.on.collision){
      cli::cli_alert_danger("Function collisions dectected: multiple packages have functions with same name!")
      purrr::walk2(fun.collisions$packages, fun.collisions$fun, \(x, y) cli::cli_alert_danger(("{c(x)} both have function {y}()")))
      cli::cli_abort("Canceling due to function collisions. Remove one of the packages,
                     use `funs.ignore` or set `error.on.collisions = FALSE`.")
    }else{
      cli::cli_alert_info("Function collisions dectected: multiple packages have functions with same name.")
      cli::cli_alert_info("errors.on.collision is FALSE. Choosing namespace prefixes of in case of collisions by order of packages in `packages` argument.")
      if(verbose){
        purrr::walk2(fun.collisions$packages, fun.collisions$fun, \(x, y) cli::cli_alert_info(("{c(x)} both have function {y}(). Using {x[1]}.")))
      }
    }
  }
  df.cleaned = df.initial |>
    dplyr::mutate(package = purrr::map_chr(.data$packages, \(x) x[1]))





  df.final = df.cleaned |> dplyr::mutate(
    fun.safe = gsub("[.]", "[.]", .data$fun),
    pattern = glue::glue("([^A-z:]){.data$fun.safe}[(]"),
    replacement = glue::glue("\\{.data$package}::{.data$fun.safe}("))


  res = df.final$pattern
  names(res) = df.final$replacement
  return(res)
}
