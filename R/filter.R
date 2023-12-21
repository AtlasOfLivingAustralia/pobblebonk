#' Internal function to extract valid `vocabularies`, given information on 
#' user-selected `standards`. Called by `tdwg_vocabularies()`
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
filter_vocabularies <- function(.query){
  # reconstruct the `version` field that links `standards` to `vocabularies`
  selected_keys <- .query |>
    pluck("standards") |>
    pull("key")
  # lookup joining table
  selected_parts <- tdwg_data$standards_vocabularies_keys |>
    filter(.data$key_standards %in% selected_keys) |>
    pull("key_vocabularies")
  # reconstruct matching vocabularies
  show_vocabularies() |>
    # mutate(key = construct_key(df, type = "vocabularies")) |>
    filter(.data$key %in% selected_parts)
}

#' Internal function to 'back-fill' `standards` based on supplied `vocabularies.`
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
expand_vocabularies <- function(.query){
  selected_keys <- .query |> 
    pluck("vocabularies") |>
    pull("key")
  # lookup joining table
  selected_parts <- tdwg_data$standards_vocabularies_keys |>
    filter(.data$key_vocabularies %in% selected_keys) |>
    pull("key_standards") |>
    unique()
  # construct matching standards
  show_standards() |>
    filter(.data$key %in% selected_parts)
}

#' Internal function to extract valid `termlists`, given information on 
#' user-selected `vocabularies` Called by `tdwg_termlists()`
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
filter_termlists <- function(.query){
  # reconstruct the `version` field that links `vocabularies` to `termlists`
  selected_keys <- .query |>
    pluck("vocabularies") |>
    pull("key")
  # lookup joining table
  selected_parts <- tdwg_data$vocabularies_termlists_keys |>
    filter(.data$key_vocabularies %in% selected_keys) |>
    pull("key_termlists")
  # construct joins
  show_termlists() |>
    filter(.data$key %in% selected_parts)
}

#' Internal function to 'back-fill' `vocabularies` based on supplied `termlists`
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
expand_termlists <- function(.query){
  selected_keys <- .query |> 
    pluck("termlists") |>
    pull("key")
  # lookup joining table
  selected_parts <- tdwg_data$vocabularies_termlists_keys |>
    filter(.data$key_termlists %in% selected_keys) |>
    pull("key_vocabularies") |>
    unique()
  # construct matching standards
  .query |>
    tdwg_vocabularies(.data$key %in% selected_parts) # note recursion here
}

#' Internal function to extract valid `terms`, given information on 
#' user-selected `termlists` Called by `tdwg_terms()`
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
filter_terms <- function(.query){
  # reconstruct the `version` field that links `termlists` to `terms`
  selected_keys <- .query |>
    pluck("termlists") |>
    pull("key")
  # lookup joining table
  selected_parts <- tdwg_data$termlists_terms_keys |>
    filter(.data$key_termlists %in% selected_keys) |>
    pull("key_terms")
  # reconstruct matching vocabularies
  show_terms() |>
    filter(.data$key %in% selected_parts) 
}

#' Internal function to 'back-fill' `termlists` based on supplied `terms`
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
expand_terms <- function(.query){
  selected_keys <- .query |> 
    pluck("terms") |>
    pull("key")
  # lookup joining table
  selected_parts <- tdwg_data$termlists_terms_keys |>
    filter(.data$key_terms %in% selected_keys) |>
    pull("key_termlists") |>
    unique()
  # construct matching standards
  .query |>
    tdwg_termlists(.data$key %in% selected_parts)
}