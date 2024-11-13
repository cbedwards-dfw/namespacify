#' Detect packages likely to be relevant for namespacify
#'
#' This function can be run before `namespacify()` to identify packages that
#' are being loaded in the specified folder or files. This function (invisibly)
#' returns a vector of package names, and loads a corresponding `usethis::use_package()`
#' call into the clipboard. Before adding the packages to the namespace with use_package()
#' or running namespacify, consider reviewing the package list to make sure that it
#' makes sense: it might include calls to meta packages like `tidyverse`, or it might include
#' outdated library calls for packages that are no longer in use.
#'
#' @inheritParams namespacify
#'
#' @return Vector of packages in use, which can be used as the `packages` argument for `namespacify()`.
#' @export
#'
package_detect = function(
    folder = NULL
){
  r_files = identify_files(folder = folder, verbose = TRUE)
  all.text = unlist(purrr::map(r_files, readLines, warn = FALSE))

  package.calls = c(stringr::str_extract_all(all.text, "library(.*?)",
                                             simplify = TRUE),
                    stringr::str_extract_all(all.text, "require(.*?)",
                                             simplify = TRUE)
  )
  package.calls = package.calls[package.calls != ""]
  package.calls = gsub(".*[(]", "", package.calls)
  package.calls = gsub("[)]", "", package.calls)
  package.calls = gsub(" ", "", package.calls)
  packages.vec = unlist(strsplit(package.calls, ","))
  cli::cli_alert_info("The following packages were loaded in one or more scripts: {packages.vec}. Returning vector of these packages.")
  cli::cli_alert_info("If developing a package, recommend confirming that these packages are indeed needed, and then adding them to the namespace with `usethis::use_package(packagename)`.")
  cli::cli_alert_info("Loading appropriate `use_package()` call to clipboard")
  clipr::write_clip(paste0("usethis::use_package(", paste0(paste0('"', packages.vec, '"'), collapse = ", "), ")"))
  invisible(packages.vec)
}
