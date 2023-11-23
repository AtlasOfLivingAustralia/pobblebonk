#' Check tibble given to `vocabuliaries` is a tibble with a single row
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @noRd
#' @keywords Internal
check_standards_tibble <- function(.x){
  if(missing(.x)){
    abort(".x is missing, with no default")
  }else if(!inherits(.x, "data.frame")){
    abort(".x must be a `data.frame` or `tibble`")
  }else if(nrow(.x) > 1L){
    inform(".x has more than one row; choosing the most recent entry") 
    .x[which.max(.x$version_issued)[1], ]
  }else{
    .x
  }
}