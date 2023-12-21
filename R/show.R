#' Show TDWG data
#' 
#' Low-level functions for calling internal data sourced from TDWG. These 
#' functions are not full exports of TDWG data structures; some columns have 
#' been removed, and others have been renamed. For piping, use `tdwg_` functions 
#' instead.
#' @name show_
#' @format
#' All `tibble`s returned by `show_` functions have the following columns:
#' \describe{
#'   \item{code}{Identifier for the type of data within the column; i.e. 
#'               for `show_standards()` this is the unique code for that 
#'               standard.}
#'  \item{date}{Release date for selected object in `"YYYY-MM-DD"` format. 
#'              For standards, this is effectively synonymous with a version of 
#'              the standard; but this is not the case at lower levels of the 
#'              hierarchy.}
#'  \item{label}{Human-readable tag for this datum.}
#'  \item{description}{A detailed overview of the function of this datum.}
#'  \item{status}{`"recommended"`, `"superseded"` or `"deprecated"`.}
#'  \item{key}{A unique identifier used to navigate the information hierarchy,
#'            formed as a URL.}
#' }
#' 
#' In addition, `terms` data have the following additional columns:
#' \describe{
#'   \item{type}{TDWG terms contain descriptions of the terms themselves 
#'               (type = `terms`), but also the classes that are related to 
#'               those terms (type = `class`). Note that terms can be nested
#'               within classes or vice versa.}
#'   \item{parent_class}{If a `term` is nested within a `class`, the parent 
#'                      class is listed here. Useful for grouping like terms.}
#' }
#' @order 1
#' @importFrom dplyr filter
#' @export
show_standards <- function(){tdwg_data$standards}

#' @rdname show_
#' @order 2
#' @export
show_vocabularies <- function(){tdwg_data$vocabularies}

#' @rdname show_
#' @order 3
#' @export
show_termlists <- function(){tdwg_data$termlists}

#' @rdname show_
#' @order 4
#' @export
show_terms <- function(){tdwg_data$terms}