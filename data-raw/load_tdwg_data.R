# Script to collect and arrange data used in this package.
# All data is downloaded direct from TDWG
library(readr)
library(dplyr)
library(tidyr)
library(httr2)
library(glue)
library(purrr)


## STAGE 1: SORUCE REQUIRED DATA FROM TDWG
# Note: we use httr2 to cache these files, rather than import direct from csv, 
# so that we don't have to then write out to csv at a later time.

# build a source tibble
download_df <- tibble(url = c(
  "https://raw.githubusercontent.com/tdwg/rs.tdwg.org/master/standards-versions/standards-versions.csv",
  "https://raw.githubusercontent.com/tdwg/rs.tdwg.org/master/term-lists/term-lists.csv",
  "https://raw.githubusercontent.com/tdwg/rs.tdwg.org/master/term-lists-versions/term-lists-versions.csv"
)) |>
  mutate(file = glue("./data-raw/{basename(url)}"))

# check whether an update is needed
request("https://github.com/tdwg/rs.tdwg.org/blob/master/standards/standards.csv") |>
  req_perform(file = "./data-raw/standards.csv")
last_date_modified <- "2023-09-18 13:54:21" # this can be overwritten as needed
standards <- read_csv("./data-raw/standards.csv") |>
  filter(label == "Darwin Core")

# if date is 'new', refresh information from GitHub
if(as.character(standards$document_modified) != last_date_modified){
  map(split(download_df, seq_len(nrow(download_df))), 
      function(a){
        request(a$url) |> req_perform(path = a$file)})  
}


#' restrict raw data to that needed for this package
#' 
#' Takes a list from `build_tdwg_list()` and filters it to only relevant information
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom tidyr replace_na
#' @export
subset_tdwg_list <- function(x){
  
  # level 1: standards
  # right now we only want Darwin Core
  x$standards <- x$standards |>
    filter(label == "Darwin Core")
  
  dwc_standard <- x$standards$standard
  
  # 1a. get all versions, narrow to dwc_standard
  x$standards_versions <- x$standards_versions |>
    filter(standard == dwc_standard) |>
    arrange(desc(version))
  
  # get information on which `term-lists` are available 
  x$term_lists <- x$term_lists |>
    filter(standard == dwc_standard) |>
    mutate(list_deprecated = replace_na(list_deprecated, FALSE)) |>
    arrange(desc(list_modified))
  
  dwc_term_lists <- x$term_lists |>
    pull(list) |>
    unique()
  
  # as above, but including previous versions 
  x$term_lists_versions <- x$term_lists_versions |>
    filter(list %in% dwc_term_lists) |>
    mutate(list_deprecated = replace_na(list_deprecated, FALSE)) |>
    arrange(desc(version_modified))
  
  # this can be used to get `terms`
  
}
