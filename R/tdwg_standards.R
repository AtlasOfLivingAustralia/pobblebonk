#' Amend a `tdwg` query
#' 
#' These functions drill down through the TDWG information hierarchy
#' 
#' @param .query An object of class `tdwg`, created with `tdwg()`
#' @param standard (string) Either a valid `label` (or part thereof, e.g. 
#' `"Audibon"` will return `"Audibon Core"`), or a TDWG standard number (e.g.
#' `"450"`). See [valid_standards()] for details.
#' #' @param version (string) Either `"recommended"` (the default), or a valid
#' date string in `"YYYY-MM-DD"` format. See [valid_versions()] for examples. 
#' Only used if not already specified by an earlier function.
#' @details
#' The sequence of `tdwg` objects is `standards` > `vocabularies` > `term lists` 
#' (> `classes`) > `terms`.
#' @importFrom dplyr filter
#' @rdname tdwg_
#' @order 2
#' @export
tdwg_standards <- function(.query,
                           standard = "Darwin Core",
                           version = "recommended"
                           ){
  check_tdwg_object(.query)
  df <- show_standards() |>
    filter_text(text = standard,
                field1 = "label",
                field2 = "standard") |>
    filter_version(version = version,
                   text_field = "standard_status", 
                   date_field = "version_issued")
  update_tdwg(.query, standards = df)
}