#' Piped syntax for navigating TDWG data
#' 
#' This creates an object of class `tdwg`, which can be piped to refine the 
#' information returned at the end. Susequent functions amend the subsequent 
#' query. To investigate complete tibbles without piping, use `show_` functions.
#' @details
#' An unusual feature of this syntax is that requests propagate up the hierarchy,
#' but not down. That is, calling `tdwg_standards()` (level 1) fills only the 
#' `standards` slot, but calls to `tdwg_vocabularies()` (level 2) fills 
#' `vocabularies` and `standards`. The interpretation of 'back-filled' slots
#' is that they show all entries that contain at least one row from the selected
#' level of the hierarchy. It does _not_ answer the question "Which single 
#' standard contains all the terms I asked for?".
#' @returns An object of class `tdwg`, which is a length-4 list containing
#' the requested information; except `summarize()` which returns a `tibble`
#' @name tdwg 
#' @order 1
#' @export
tdwg <- function(){
  x <- vector(mode = "list", length = 4)
  names(x) <- c("standards", 
                "vocabularies",
                "termlists",
                "terms")
  class(x) <- "tdwg"
  x
}

#' #' @rdname tdwg
#' #' @export
#' print.tdwg <- function(x, ...){
#'   str(x)
#' }

#' @rdname tdwg
#' @order 6
#' @export
summarize.tdwg <- function(.data, ...){
  if(is.null(.data$terms)){
    inform(c("No `terms` calculated for the specified query",
             i = "Did you call `tdwg_terms()` yet?"))
  }else{
    .data |>
      pluck("terms") |>
      select("parent_class", "code", "description", "status") |>
      arrange("parent_class", "code")    
  }
}

#' @rdname tdwg
#' @order 7
#' @export
summarise.tdwg <- summarize.tdwg

#' Internal function to update a `tdwg` object
#' @importFrom purrr list_modify
#' @importFrom purrr list_flatten
#' @noRd
#' @keywords Internal
update_tdwg <- function(x, ...){
  y <- list(...) # |> list_flatten()
  # class(x) <- "list"
  # x <- list_flatten(x)
  list_modify(x, !!!y)
  # class(result) <- "tdwg"
  # result
}

#' Check tibble given to `vocabularies` is a tibble with a single row
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_tdwg_object <- function(x){
  if(missing(x)){
    abort(c("`.query` is missing, with no default", 
            i = "`tawnydragon` pipes must start with a call to `tdwg()`"))
  }else if(!inherits(x, "tdwg")){
    abort(c("`.query` is not a `tdwg` object", 
            i = "`tawnydragon` pipes must start with a call to `tdwg()`"))
  }
}

#' @param .query An object of class `tdwg`, created with `tdwg()`
#' @param ... Further arguments passed to `dplyr::filter()`
#' @rdname tdwg
#' @order 2
#' @importFrom dplyr filter
#' @importFrom rlang enquos
#' @export
tdwg_standards <- function(.query, ...){
  check_tdwg_object(.query)
  if(length(enquos(...)) < 1){
    df <- show_standards() |>
      filter(code == "450", status == "recommended")
  }else{
    df <- show_standards() |>
      filter(...)
  }
  update_tdwg(.query, standards = df)
}

#' @rdname tdwg
#' @order 3
#' @export
tdwg_vocabularies <- function(.query, ...){
  check_tdwg_object(.query)
  # if user hasn't supplied `standards`, build `vocabularies` from user-supplied 
  # information, then 'back-fill' `standards`
  if(is.null(.query$standards)){
    df <- show_vocabularies() |>
      filter(...)
    .query <- update_tdwg(.query, vocabularies = df)
    standards_df <- expand_vocabularies(.query)
    update_tdwg(.query, standards = standards_df)
  }else{
    # otherwise, filter `standards` first, then apply user-supplied information
    df <- filter_vocabularies(.query) |>
      filter(...)
    update_tdwg(.query, vocabularies = df)
  }
}

#' @rdname tdwg
#' @order 4
#' @export
tdwg_termlists <- function(.query, ...){
  check_tdwg_object(.query)
  # check 'above' terms in hierarchy to look for relevant information
  empty_check <- lapply(.query[1:2], is.null) |> unlist()
  if(all(empty_check)){ # upward search
    df <- show_termlists() |>
      filter(...)
    .query <- update_tdwg(.query, termlists = df)
    expand_termlists(.query) # note: returns an updated `.query`, *not* a `tibble`
  }else{ # downward search
    search_from <- names(.query)[max(which(!empty_check))]
    .query <- switch(search_from, 
                     "standards" = {.query |>
                         tdwg_vocabularies() |>
                         tdwg_termlists() },
                     .query)
    df <- filter_termlists(.query) |>
      filter(...)
    update_tdwg(.query, termlists = df)
  }
}

#' @rdname tdwg
#' @order 5
#' @export
tdwg_terms <- function(.query, ...){
  check_tdwg_object(.query)
  # check 'above' terms in hierarchy to look for relevant information
  empty_check <- lapply(.query[1:3], is.null) |> unlist()
  if(all(empty_check)){
    df <- show_terms() |>
      filter(...)
    .query <- update_tdwg(.query, terms = df)
    expand_terms(.query)
  }else{
    search_from <- names(.query)[max(which(!empty_check))]
    .query <- switch(search_from, 
                     "standards" = {.query |>
                                      tdwg_vocabularies() |>
                                      tdwg_termlists() },
                     "vocabularies" = {.query |>
                                        tdwg_termlists()},
                     .query)
    df <- filter_terms(.query) |>
      filter(...)
    update_tdwg(.query, terms = df)
  }
}