#' Look up standards
#' @param ... arguments passed to `dplyr::filter`
#' @importFrom dplyr filter
#' @export
standards <- function(...){
  tdwg$standards_versions |>
    filter(...)
}

#' look up vocabularies
#' @param .x a single-row `tibble` giving the required standard, created using
#' `standard()`
#' @param standard (optional) string giving the standard for which to display 
#' valid vocabularies. Not required if `.x` is given
#' @param version (optional) string giving the version for which to display 
#' valid vocabularies. Not required if `.x` is given
#' @param ... arguments passed to `dplyr::filter`
vocabularies <- function(.x, standard, version, ...){
  
}