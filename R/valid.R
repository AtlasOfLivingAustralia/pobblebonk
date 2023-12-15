#' Look up which standards are supported
#' @rdname valid
#' @order 1
#' @importFrom dplyr group_by
#' @importFrom dplyr pull
#' @importFrom dplyr summarize
#' @export
valid_standards<- function(){
  show_standards() |>
    filter(.data$standard_status == "recommended") |>
    group_by(standard) |>
    summarize(label, description)
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
      pull("version_issued") |>
      unique() |>
      sort(decreasing = TRUE)    
  }else{
    show_standards() |>
      filter(.data$standard == standard) |>
      pull("version_issued") |>
      unique() |>
      sort(decreasing = TRUE)
  }
}

#' @rdname valid
#' @order 3
#' @export
valid_vocabularies <- function(){
  show_vocabularies() |>
    filter(.data$vocabulary_status == "recommended") |>
    group_by(vocabulary) |>
    select(vocabulary, label, description)
}