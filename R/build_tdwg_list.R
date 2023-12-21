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
                "standards-versions/standards-versions.csv",
                "standards-versions/standards-versions-parts.csv",
                # level 2: documents and vocabularies 
                "vocabularies-versions/vocabularies-versions.csv",
                "vocabularies-versions/vocabularies-versions-members.csv",
                "docs-versions/docs-versions.csv", # unimplemented yet
                # level 3: term lists
                "term-lists-versions/term-lists-versions.csv",
                "term-lists-versions/term-lists-versions-members.csv",
                # level 4: terms
                "terms-versions/terms-versions.csv" 
                )
  urls <- paste0(base_url, suffixes)
  result <- map(.x = urls, 
                .f = function(a){read_csv(a, show_col_types = FALSE)},
                .progress = TRUE)
  names(result) <- c("standards",
                     "standards_vocabularies_keys",
                     "vocabularies",
                     "vocabularies_termlists_keys",
                     "documents",
                     "termlists",
                     "termlists_terms_keys",
                     "terms")
  result$standards <- tidy_standards(result$standards)
  names(result$standards_vocabularies_keys) <- c("key_standards", "key_vocabularies")
  result$vocabularies <- tidy_vocabularies(result$vocabularies)
  names(result$vocabularies_termlists_keys) <- c("key_vocabularies", "key_termlists")
  result$termlists <- tidy_termlists(result$termlists)
  names(result$termlists_terms_keys) <- c("key_termlists", "key_terms")
  result$terms <- tidy_terms(result$terms)
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
           status = standard_status,
           key = version) |>
    select(code, date, label, description, status, key) |>
    arrange(desc(date))
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
           status = vocabulary_status, 
           key = version) |>
    select(code, date, label, description, status, key) |>
    arrange(desc(date))
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
                                     .default = status),
           key = version) |>
    select(code, date, label, description, status, key) |>
    arrange(desc(date))
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
  type_vector <- case_when(
    df$rdf_type == "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property" ~ "term",
    df$rdf_type == "http://www.w3.org/2000/01/rdf-schema#Class" ~ "class",
    df$rdf_type == "http://purl.org/dc/dcam/VocabularyEncodingScheme" ~ "vocabulary_scheme")
  # type_vector[df$term_localName %in% identified_classes] <- "parent_class"
  df |>
    mutate(code = term_localName,
           date = version_issued,
           status = version_status,
           description = rdfs_comment,
           type = type_vector,
           parent_class = class_vector,
           key = version) |>
    # filter(!(term_localName %in% identified_classes)) |>
    select(code, 
           date, 
           parent_class, 
           label, 
           description, 
           examples, 
           type, 
           status,
           key) |>
    arrange(desc(date), parent_class, code)
}