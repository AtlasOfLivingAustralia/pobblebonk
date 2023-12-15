#' Get classes
#' 
#' `class`es are a type of `term`. They are nested within `termLists` within the
#' TDWG hierarchy.
#' 
#' Many terms are within classes; for example class `Event` includes an 
#' `eventID`, `eventTime` and many others. Ergo classes are useful for grouping 
#' similar types of `class`. However, some `class`es are nested within terms; 
#' `LivingSpecimen` is best thought of as a valid entry to the `basisOfRecord` 
#' `term`, for example.
#' 
#' @export
tdwg_classes <- function(.x, ...){
  
  df <- tdwg_data$terms_versions
  
  # look up `organisedInClass` entries
  class_links <- df |>
    filter(!is.na(tdwgutility_organizedInClass)) |>
    pull(tdwgutility_organizedInClass) |>
    unique()
  urls <- dirname(class_links) |>
    unique()
  class_names <- paste0("^", urls, "/") |>
    paste(collapse = "|") |>
    sub(replacement = "", x = class_links)
  
  # get terms that correspond to this class
  if(missing(.x)){ # i.e. no prior information
    
  }
  
  df |>
    filter(grepl("Class$", x = rdf_type),
           term_localName %in% class_names,
           # ...
           )
    
}