#' look up terms
#' 
#' terms are the final specificity within the TDWG hierarchy.
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @export
tdwg_terms <- function(.x, ...){
  if(missing(.x)){
    df <- tdwg_data$terms_versions |>
      filter(...)
  }else{
    selected_terms_lists <- .x$version
    # # parse via `term_lists_versions_members`
    selected_terms <- tdwg_data$term_lists_versions_members |>
      filter(.data$termListVersion %in% selected_terms_lists) |>
      pull("termVersion")
    df <- tdwg_data$terms_versions |>
      filter(...,
             .data$version %in% selected_terms)     
  }
  df |>
    select("term_localName", 
           "label", 
           "tdwgutility_organizedInClass", # get last entry
           "rdfs_comment", 
           "version", 
           "version_status") |>
    rename("term" = "term_localName",
           "description" = "rdfs_comment",
           "class" = "tdwgutility_organizedInClass") |>
    mutate(class = sub("http://purl.org/|http://rs.tdwg.org/", 
                       replacement = "",
                       x = class)) |>
    mutate(class = sub("/terms/", "-", x = class)) |>
    arrange(class, term)
}