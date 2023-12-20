#' Piped syntax for navigating TDWG data
#' 
#' This creates an object of class `tdwg`, which can be piped to refine the 
#' information returned at the end. Susequent functions amend the subsequent 
#' query. To investigate complete tibbles without piping, use `show_` functions.
#' @rdname tdwg 
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

#' Internal function to update a `tdwg` object
#' @importFrom purrr list_modify
#' @noRd
#' @keywords Internal
update_tdwg <- function(x, ...){
  y <- list(...)
  list_modify(x, !!!y)
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
#' @param label (string) A complete, valid `label`, such as 
#' `"Audiovisual Core"`. Partial matches are not supported.
#' @param code (string) TDWG code for a given level in the hierarchy, such as
#' `450` for Darwin Core standard.
#' @param date (string) A valid date in `"YYYY-MM-DD"` format, used to 
#' signify the date that the level in question was adopted. Note that dates for
#' `standards` correspond to accepted versions, whereas at other levels in the
#' hierarchy they relate to the most recent update. Ergo it is possible to use
#' different dates at different points in the hierarchy, though care should be
#' taken when doing so.
#' @param status (string) Either `"recommended"` or `"superseded"`. 
#' @details
#' The sequence of `tdwg` objects is `standards` > `vocabularies` > `term lists` 
#' (> `classes`) > `terms`.
#' @rdname tdwg
#' @order 2
#' @export
tdwg_standards <- function(.query,
                           label = "Darwin Core",
                           code = NULL,
                           date = NULL,
                           status = "recommended"){
  check_tdwg_object(.query)
  update_tdwg(.query, 
              standards = show_standards(label = label,
                                         code = code,
                                         date = date,
                                         status = status))
}

#' @rdname tdwg
#' @order 3
#' @export
tdwg_vocabularies <- function(.query, 
                              label = NULL,
                              code = "dwc",
                              date = NULL,
                              status = "recommended"){
  check_tdwg_object(.query)
  # if user hasn't supplied `standards`, build `vocabularies` from user-supplied 
  # information, then 'back-fill' `standards`
  if(is.null(.query$standards)){
    df <- show_vocabularies(label = label,
                            code = code,
                            date = date,
                            status = status)
    .query <- update_tdwg(.query, vocabularies = df)
    standards_df <- expand_vocabularies(.query)
    update_tdwg(.query, standards = standards_df)
  }else{
    # otherwise, filter `standards` first, then apply user-supplied information
    df <- filter_vocabularies(.query) |>
      conditional_filter(label = label,
                         code = code,
                         date = date,
                         status = status)
    update_tdwg(.query, vocabularies = df)
  }
}

#' @rdname tdwg
#' @param type (string) Either `"term"` or `"class"`, corresponding to the 
#' different types of information held in TDWG term lists
#' @param parent_class (string) Some `terms` are nested within a parent `class`
#' such as `Event` or `Occurrence`. Use this argument to select only those
#' terms within a particular class.
#' @order 4
#' @export
tdwg_termlists <- function(.query,
                           label = NULL,
                           code = NULL,
                           date = NULL,
                           status = NULL){
  check_tdwg_object(.query)
  # if user hasn't supplied `standards`, build `vocabularies` from user-supplied 
  # information, then 'back-fill' `standards`
  if(is.null(.query$vocabularies)){
    df <- show_termlists(label = label,
                         code = code,
                         date = date,
                         status = status)
    .query <- update_tdwg(.query, termlists = df)
    vocabularies_df <- expand_termlists(.query) # TODO: write this function
    update_tdwg(.query, vocabularies = vocabularies_df)
  }else{
    # otherwise, filter `vocabularies` first, then apply user-supplied information
    df <- filter_termlists(.query) |>
      conditional_filter(label = label,
                         code = code,
                         date = date,
                         status = status)
    update_tdwg(.query, termlists = df)
  }
}

#' @rdname tdwg
#' @order 5
#' @export
tdwg_terms <- function(.query,
                       label = NULL,
                       code = NULL,
                       date = NULL,
                       status = NULL,
                       type = "term",
                       parent_class = NULL){
  check_tdwg_object(.query)
  # if user hasn't supplied `standards`, build `vocabularies` from user-supplied 
  # information, then 'back-fill' `standards`
  if(is.null(.query$vocabularies)){
    df <- show_terms(label = label,
                     code = code,
                     date = date,
                     status = status,
                     type = type,
                     parent_class = parent_class)
    .query <- update_tdwg(.query, termlists = df)
    termlists_df <- expand_termlists(.query) # TODO: write this function
    update_tdwg(.query, termlists = termlists_df)
  }else{
    # otherwise, filter `terms` first, then apply user-supplied information
    df <- filter_terms(.query) |>
      conditional_filter(label = label,
                         code = code,
                         date = date,
                         status = status,
                         type = type,
                         parent_class = parent_class)
    update_tdwg(.query, terms = df)
  }
}