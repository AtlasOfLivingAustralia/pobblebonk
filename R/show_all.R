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
#' @param ... arguments passed to `dplyr::filter`
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom rlang abort
#' @importFrom rlang enquos
#' @export
vocabularies <- function(.x, ...){
  .x <- check_standards_tibble(.x)
  # look up `standards-parts` for the selected standard
  selected_standard <- .x$version
  selected_parts <- tdwg$standards_versions_parts |>
    filter(.data$standard_version %in% selected_standard) |>
    pull("part")
  tdwg$vocabularies_versions |>
    filter(...,
           .data$version %in% selected_parts)
}

# Look up documents
# not yet defined

#' look up term lists
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export
term_lists <- function(.x, ...){
  requested_versions <- .x$version
  # parse via `vocabularies_versions_members`
  selected_term_lists <- tdwg$vocabularies_versions_members |>
    filter(.data$vocabulary %in% requested_versions) |>
    pull("termList")
  tdwg$term_lists_versions |>
    filter(...,
           .data$version %in% selected_term_lists)
}

#' look up terms
#' @importFrom dplyr arrange
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @export
terms <- function(.x, ...){
  selected_terms_lists <- .x$version
  # # parse via `term_lists_versions_members`
  selected_terms <- tdwg$term_lists_versions_members |>
    filter(.data$termListVersion %in% selected_terms_lists) |>
    pull("termVersion")
  tdwg$terms_versions |>
    filter(...,
           .data$version %in% selected_terms) |>
    select("term_localName", "label", "rdfs_comment", "version", "version_status") |>
    rename("term" = "term_localName",
                  "description" = "rdfs_comment") |>
    arrange(term)
}