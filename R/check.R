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

#' Check tibble given to `vocabularies` is a tibble with a single row
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @noRd
#' @keywords Internal
check_standards_tibble <- function(x){
  if(!inherits(x, "data.frame")){
    abort("`standards` must be a `data.frame` or `tibble`")
  }else if(nrow(x) > 1L){
    inform("`standards` has more than one row; choosing the most recent entry") 
    x[which.max(x$version_issued)[1], ]
  }else{
    x
  }
}


