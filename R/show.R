#' Show raw data from TDWG
#' 
#' low-level functions for calling internal data sourced from TDWG. These 
#' functions are not exact exports of TDWG data structures. For piping, use
#' `tdwg_` functions instead.
#' @rdname show_
#' @param label (string) A complete, valid `label`, such as 
#' `"Audiovisual Core"`. Partial matches are not supported.
#' @param code (integer) TDWG code for a given level in the hierarchy, such as
#' `450` for Darwin Core standard.
#' @param date (string) A valid date in `"YYYY-MM-DD"` format, used to 
#' signify the date that the level in question was adopted.
#' @param status (string) Either `"recommended"`, `"superseded"` or (more 
#' rarely) `"deprecated"`.
#' @order 1
#' @export
show_standards <- function(label = NULL, # Note: might be nicer to use `missing()`
                           code = NULL,  # rather than `NULL`
                           date = NULL,
                           status = NULL
                           ){
  tdwg_data$standards_versions |>
    conditional_filter(label = label,
                       code = code,
                       date = date,
                       status = status)
}

#' @rdname show_
#' @order 2
#' @export
show_vocabularies <- function(label = NULL,
                              code = NULL,
                              date = NULL,
                              status = NULL
                              ){
  tdwg_data$vocabularies_versions  |>
    conditional_filter(label = label,
                       code = code,
                       date = date,
                       status = status)
}

#' @rdname show_
#' @order 3
#' @export
show_termlists <- function(label = NULL,
                           code = NULL,
                           date = NULL,
                           status = NULL
){
  tdwg_data$term_lists_versions  |>
    conditional_filter(label = label,
                       code = code,
                       date = date,
                       status = status)
}

#' @rdname show_
#' @param type (string) Either `"term"` or `"class"`, corresponding to the 
#' different types of information held in TDWG term lists
#' @param parent_class (string) Some `terms` are nested within a parent `class`
#' such as `Event` or `Occurrence`. Use this argument to select only those
#' terms within a particular class.
#' @order 4
#' @export
show_terms <- function(label = NULL,
                       code = NULL,
                       date = NULL,
                       status = NULL,
                       type = NULL,
                       parent_class = NULL
){
  tdwg_data$terms_versions  |>
    conditional_filter(label = label,
                       code = code,
                       date = date,
                       status = status,
                       type = type,
                       parent_class = parent_class)
}