#' Internal function to filter by version
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
filter_version <- function(df, version, text_field, date_field){
  if(!inherits(version, "character")){
    version <- as.character(version)
  }
  if(version == "recommended"){
    df <- df |> filter({{text_field}} == "recommended")
  }else{
    version_check <- df[["date_field"]] == version
    if(any(version_check)){
      df <- df[version_check, ]
    }else{
      bullets <- glue("no valid `versions` found that match `{version}`")
      abort(bullets)    
    }
  }
  df
}

#' Internal function to filter by a string
#' In most cases these have a primary and secondary field to check against
#' @importFrom glue glue
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
filter_text <- function(df, text, field1, field2){
  # subset by standard
  # ensure a string is given
  if(!inherits(text, "character")){
    text <- as.character(text)
  }
  # check column 1
  check1 <- grepl(text, df[[field1]]) 
  if(any(check1)){
    df <- df[check1, ]
  }else{
    # if missing, look for field2
    check2 <- grepl(text, df[[field2]])
    if(any(check2)){
      df <- df[check2,]
    }else{
      bullets <- glue("no valid entries found that match `{text}`")
      abort(bullets)
    }
  }
}

#' Internal function to extract valid vocabularies, given information on 
#' user-selected standards. Called by `tdwg_vocabularies()`
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @noRd
#' @keywords Internal
filter_vocabularies <- function(.query){
  standards <- check_standards_tibble(.query$standards)
  selected_standard <- standards$version
  selected_parts <- tdwg_data$standards_versions_parts |>
    filter(.data$standard_version %in% selected_standard) |>
    pull("part")
  show_vocabularies() |>
    filter(.data$version %in% selected_parts)
}

#' Internal function to extract valid termlists, given information on 
#' user-selected vocabularies Called by `tdwg_termlists()`
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @noRd
#' @keywords Internal
filter_termlists <- function(.query){
  requested_versions <- .query$vocabularies$version
  # parse via `vocabularies_versions_members`
  selected_term_lists <- tdwg_data$vocabularies_versions_members |>
    filter(.data$vocabulary %in% requested_versions) |>
    pull("termList")
  df <- show_termlists() |>
    filter(.data$version %in% selected_term_lists) 
}