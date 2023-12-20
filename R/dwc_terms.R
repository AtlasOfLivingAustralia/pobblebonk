#' Get DwC terms that match user-specified arguments
#' 
#' Basically a wrapper function to the piped syntax given by `tdwg()` et al.
#' @param date (string) A valid date on which a DwC standard was released.
#' @returns A `tibble` listing the requested terms, along with appropriate 
#' metadata.
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @export
dwc_terms <- function(date = "2023-09-18"){
  tdwg() |>
    tdwg_standards(code = 450, date = date) |>
    tdwg_vocabularies() |>
    tdwg_termlists(code = "dwc/terms") |>
    tdwg_terms(type = "term") |>
    pluck(terms) |>
    select(parent_class, code, description) |>
    arrange(parent_class, code)
}