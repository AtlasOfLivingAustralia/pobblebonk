#' Get raw data from TDWG
#' 
#' Rough function to populate a list of data from TDWG, with (all?) information
#' needed to look up terms, versions etc.
#' 
#' Later on, it might be useful to 1. cache this information somewhere, and 
#' 2. add checks for whether information is up to date
#' @returns list of tibbles
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @noRd
#' @keywords Internal
build_tdwg_list <- function(){
  base_url <- "https://raw.githubusercontent.com/tdwg/rs.tdwg.org/master/"
  suffixes <- c(
                # level 1: standards
                "standards-versions/standards-versions.csv", # key column: `standard`
                "standards-versions/standards-versions-parts.csv", # links `standard` (level 1) to `part`
                # level 2: documents and vocabularies 
                "vocabularies-versions/vocabularies-versions.csv", # `part` (above) == `vocabulary` (here)
                "vocabularies-versions/vocabularies-versions-members.csv",
                "docs-versions/docs-versions.csv", # unimplemented yet
                # level 3: term lists
                # "term-lists/term-lists.csv",
                "term-lists-versions/term-lists-versions.csv", #  == `vocabulary` (above) == `vann_preferredNamespaceUri` (here)
                "term-lists-versions/term-lists-versions-members.csv",
                # level 4: terms
                "terms-versions/terms-versions.csv" 
                )
  urls <- paste0(base_url, suffixes)
  result <- map(.x = urls, 
                .f = function(a){read_csv(a, show_col_types = FALSE)},
                .progress = TRUE)
  names(result) <- basename(urls) |>
                   gsub("-", "_", x = _) |>
                   sub(".csv$", "", x = _)
  
  # tidying
  result$standards_versions <- tidy_standards(result$standards_versions)
  result$vocabularies_versions <- tidy_vocabularies(result$vocabularies_versions)
  result$term_lists_versions <- tidy_termlists(result$term_lists_versions)
  result$terms_versions <- tidy_terms(result$terms_versions)
  result
}

#' Internal function to re-organise `standards`
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
tidy_standards <- function(df){
  code_field <- df |>
    pull("version") |>
    sub("^http://www.tdwg.org/standards/", "", x = _) |>
    substr(start = 1, stop = 3) |>
    as.character() # for consistency with codes at other levels
  df |>
    mutate(code = code_field,
           date = version_issued,
           status = standard_status) |>
    select(code, date, label, description, status) |>
    arrange(desc(date)) |>
    mutate(id = seq_len(nrow(df)), .before = code)
}

#' Internal function to re-organise `vocabularies`
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
tidy_vocabularies <- function(df){
  code_field <- df |>
    pull("version") |>
    sub("^http://rs.tdwg.org/version/", "", x = _) |>
    sub("/[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$", "", x = _)
  df |>
    mutate(code = code_field,
           date = version_issued,
           status = vocabulary_status) |>
    select(code, date, label, description, status) |>
    arrange(desc(date)) |>
    mutate(id = seq_len(nrow(df)), .before = code)
}

#' Internal function to re-organise `termlists`
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
tidy_termlists <- function(df){
  df |>
    mutate(code = sub("/$", "", x = list_localName),
           date = version_modified,
           status = dplyr::case_when(!is.na(list_deprecated) ~ "deprecated",
                                     .default = status)) |>
    select(code, date, label, description, status) |>
    arrange(desc(date)) |>
    mutate(id = seq_len(nrow(df)), .before = code)
}

#' Internal function to re-organise `terms`
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_extract
#' @noRd
#' @keywords Internal
tidy_terms <- function(df){
  class_vector <- str_extract(df$tdwgutility_organizedInClass, 
                              "[:alpha:]+$")
  identified_classes <- unique(class_vector[!is.na(class_vector)])  
  x <- df |>
    mutate(code = term_localName,
           date = version_issued,
           status = version_status,
           description = rdfs_comment,
           type = case_when(df$rdf_type == "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property" ~ "term",
                            df$rdf_type == "http://www.w3.org/2000/01/rdf-schema#Class" ~ "class"),
           parent_class = class_vector) |>
    filter((term_localName %in% identified_classes)) |>
    select(code, date, parent_class, label, description, examples, type, status) |>
    arrange(desc(date), parent_class, code)
  x |>
    mutate(id = seq_len(nrow(x)), .before = code)
}
