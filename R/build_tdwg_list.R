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
#' @export
build_tdwg_list <- function(){
  base_url <- "https://raw.githubusercontent.com/tdwg/rs.tdwg.org/master/"
  suffixes <- c(
                # level 1: standards
                # "standards/standards.csv", # current standard only - ignore for now
                "standards-versions/standards-versions.csv", # key column: `standard`
                "standards/standards-parts.csv", # links `standard` (level 1) to `part`
                # level 2: documents and vocabularies 
                "vocabularies-versions/vocabularies-versions.csv", # `part` (above) == `vocabulary` (here)
                "docs-versions/docs-versions.csv", # unimplemented yet
                # level 3: term lists
                # "term-lists/term-lists.csv",
                "term-lists-versions/term-lists-versions.csv", #  == `vocabulary` == `vann_preferredNamespaceUri` (here)
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
  result
}