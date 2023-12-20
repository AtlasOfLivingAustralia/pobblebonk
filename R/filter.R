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
    construct_key(type = "standards")
  # lookup joining table
  selected_parts <- tdwg_data$standards_versions_parts |>
    filter(.data$standard_version %in% selected_keys) |>
    pull("part")
  # reconstruct matching vocabularies
  df <- show_vocabularies()
  df |>
    mutate(key = construct_key(df, type = "vocabularies")) |>
    filter(.data$key %in% selected_parts) |>
    select(- "key")
}

#' Internal function to 'back-fill' standards based on supplied vocabularies.
#' Inverse of `filter_vocabularies()`
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
expand_vocabularies <- function(.query){
  selected_keys <- .query |> 
    pluck("vocabularies") |>
    construct_key(type = "vocabularies")
  # lookup joining table
  selected_versions <- tdwg_data$standards_versions_parts |>
    filter(.data$part %in% selected_keys) |>
    pull("standard_version")
  # construct matching standards
  df <- show_standards()
  df |>
    mutate(key = construct_key(df, type = "standards")) |>
    filter(.data$key %in% selected_versions) |>
    select(- "key")
}

#' Internal function to extract valid `termlists`, given information on 
#' user-selected `vocabularies` Called by `tdwg_termlists()`
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
filter_termlists <- function(.query){
  # reconstruct the `version` field that links `vocabularies` to `termlists`
  selected_keys <- .query |>
    pluck("vocabularies") |>
    construct_key(type = "vocabularies")
  # lookup joining table
  selected_parts <- tdwg_data$vocabularies_versions_members |>
    filter(.data$vocabulary %in% selected_keys) |>
    pull("termList")
  # construct joins
  df <- show_termlists()
  df |>
    mutate(key = construct_key(df, type = "termlists")) |>
    filter(.data$key %in% selected_parts) |>
    select(- "key")
}

# TODO: `expand_termlists()`

#' Internal function to extract valid `terms`, given information on 
#' user-selected `termlists` Called by `tdwg_terms()`
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
filter_terms <- function(.query){
  # reconstruct the `version` field that links `termlists` to `terms`
  selected_keys <- .query |>
    pluck("termlists") |>
    construct_key(type = "termlists")
  # lookup joining table
  selected_parts <- tdwg_data$term_lists_versions_members |>
    filter(.data$termListVersion %in% selected_keys) |>
    pull("termVersion")
  # reconstruct matching vocabularies
  df <- show_terms()
  df |>
    mutate(key = construct_key(df, type = "terms")) |>
    filter(.data$key %in% selected_parts) |>
    select(- "key")
}

# TODO: `expand_terms()`

#' Internal function to filter if entries are not null
#' This powers filtering in `show_` functions, and, by extension, `tdwg_`
#' functions as well
#' @importFrom dplyr filter
#' @noRd
#' @keywords Internal
conditional_filter <- function(df, ...){
  args <- list(...)
  null_test <- lapply(args, is.null) |> 
    unlist()
  args <- args[!null_test]
  if(length(args) > 0){
    for(i in seq_along(args)){
      arg <- names(args)[i]
      value <- args[[i]]
      df <- filter(df, .data[[arg]] == value)
    }
    df
  }else{
    df
  }
}

#' Internal function to build fields that act as keys between tables.
#' This adds processing time relative to retaining keys in the tables;
#' but makes for cleaner UX
#' @importFrom glue glue_data
#' @noRd
#' @keywords Internal
construct_key <- function(df, 
                          type = c("standards", 
                                   "vocabularies",
                                   "termlists",
                                   "terms")
){
  type <- match.arg(type)
  switch(type, 
         "standards" = glue_data(df, "http://www.tdwg.org/standards/{code}/version/{date}"),
         "vocabularies" = glue_data(df, "http://rs.tdwg.org/version/{code}/{date}"),
         "termlists" = {case_when(grepl("/", df$code) ~ {
           df |> mutate(
             code1 = sub("/[[:graph:]]+$", "", code),
             code2 = sub("^[[:alpha:]]+/", "", code)
           ) |>
             glue_data("http://rs.tdwg.org/{code1}/version/{code2}/{date}")
         },
         .default = glue_data(df, "http://rs.tdwg.org/version/{code}/{date}"))},
        "terms" = glue_data(df, "http://rs.tdwg.org/dwc/terms/version/{code}-{date}"))
}