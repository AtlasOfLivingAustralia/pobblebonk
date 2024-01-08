#' get DwC terms and format them
#' 
#' This was originally in `galaxias`, but is a dependency of `view_terms()`, so
#' is included here. This content probably needs more thought, and to be
#' integrated with other functions. 
#' Also, functions like this should return tibbles, not lists, unless as part of
#' a named class (i.e. `tdwg`).
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom purrr pluck
#' @importFrom tibble tibble
#' @export
get_terms <- function(){
  # get 'master' list of terms
  terms_full <- tdwg() |>
    tdwg_standards(label == "Darwin Core",
                   status == "recommended") |>
    tdwg_terms() |>
    pluck("terms")

  # get parents list
  parents <- terms_full |>
    filter(is.na(parent_class) & 
           code %in% parent_class) |>
    select("code", "description")
  
  # get reduced terms list
  terms <- terms_full |>
    filter(!(code %in% parents$code),
           type == "term") |>
    select("parent_class", 
           "code", 
           "label", 
           "description", 
           "examples", 
           "weight") |>
    arrange(desc(weight))
  terms$parent_class[is.na(terms$parent_class)] <- "No parent class"
  
  # add 'no parents' to parents list
  parents <- bind_rows(parents, 
                       tibble(code = "No parent class", 
                              description = "Terms that lack a specific class."))
  
  # add weights to parents
  # Q: should term weights be in `tawnydragon` or `galaxias`?
  # Might be better here, as it incorporates non-TDWG data
  parents <- terms |>
    group_by(parent_class) |>
    summarize(weight = sum(weight)) |>
    arrange(desc(weight)) |>
    left_join(parents, by = c("parent_class" = "code")) |>
    relocate("weight", .after = "description") |>
    rename("code" = "parent_class")
  parents$description[parents$code == "Location"] <- "Spatial information."
  
  # return a list
  list(parents = parents, terms = terms)
}