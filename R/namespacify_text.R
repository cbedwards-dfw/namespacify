#' Apply namespace prefixes to text.
#'
#' @param original.text character vector with code to be updated
#' @param vec.replacement vector of replacements generated from make_prefix_vector
#'
#' @return Replaced text
namespacify_text = function(original.text,
                            vec.replacement){
  return(stringr::str_replace_all(original.text, vec.replacement))
}
