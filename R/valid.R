#' Look up which standards are supported
#' @rdname valid
#' @order 1
#' @importFrom dplyr group_by
#' @importFrom dplyr pull
#' @importFrom dplyr summarize
#' @export
valid_standards<- function(){
  tdwg$standards_versions |>
    filter(.data$standard_status == "recommended") |>
    group_by(standard) |>
    summarize(label, description)
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
    tdwg$standards_versions |>
      pull("version_issued") |>
      unique() |>
      sort(decreasing = TRUE)    
  }else{
    tdwg$standards_versions |>
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
  tdwg$vocabularies_versions |>
    filter(.data$vocabulary_status == "recommended") |>
    group_by(vocabulary) |>
    select(vocabulary, label, description)
}