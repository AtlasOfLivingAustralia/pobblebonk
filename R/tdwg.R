#' Open a pipe for TDWG data
#' 
#' This creates an object of class `tdwg`, which can be piped to refine the 
#' information returned at the end. This is useful for navigating down the 
#' TDWG information hierarchy
#' @rdname tdwg 
#' @export
tdwg <- function(version = "recommended"){
  x <- list()
  class(x) <- "tdwg"
  x
}


#' @rdname tdwg
print.tdwg <- function(x, ...){
  str(x)
}

#' Internal function to update a `tdwg` object
#' @noRd
#' @keywords Internal
update_tdwg <- function(x, ...){
  y <- list(...)
  class(x) <- "list"
  result <- c(x, y)
  class(result) <- "tdwg"
  result
}