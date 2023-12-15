#' Show raw data from TDWG
#' 
#' low-level functions for calling internal data sourced from TDWG
#' @rdname show_
#' @order 1
#' @export
show_standards <- function(){tdwg_data$standards_versions}

#' @rdname show_
#' @order 2
#' @export
show_vocabularies <- function(){tdwg_data$vocabularies_versions}

#' @rdname show_
#' @order 3
#' @export
show_termlists <- function(){tdwg_data$term_lists_versions}

#' @rdname show_
#' @order 4
#' @export
show_terms <- function(){tdwg_data$terms_versions}