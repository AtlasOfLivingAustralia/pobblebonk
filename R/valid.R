# NOTE: these are largely redundant given changes to `show_` - consider removing

#' Look up which standards are supported
#' @rdname valid
#' @order 1
#' @importFrom dplyr arrange
#' @importFrom dplyr pull
#' @importFrom dplyr summarize
#' @export
valid_standards<- function(){
  show_standards() |>
    filter(.data$status == "recommended") |>
    arrange(code) |>
    summarize(code, label, description)
  # add standard number to this
}

#' @rdname valid
#' @param standard (optional) if given, a standard for which valid versions will
#' be given. Otherwise, all dates with releases will be shown
#' @order 2
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export
valid_versions <- function(standard){
  if(missing(standard)){
    show_standards() |>
      pull("date") |>
      unique() |>
      sort(decreasing = TRUE)    
  }else{
    show_standards() |>
      filter(.data$code == standard) |>
      pull("date") |>
      unique() |>
      sort(decreasing = TRUE)
  }
}

#' @rdname valid
#' @order 3
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @export
valid_vocabularies <- function(){
  show_vocabularies() |>
    filter(.data$status == "recommended") |>
    arrange(code) |>
    select(code, label, description)
}