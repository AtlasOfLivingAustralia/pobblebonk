#' @param vacabulary (string) Either a valid `label` (or part thereof, e.g. 
#' `"Audibon"` will return `"Audibon Core"`), or a TDWG vocabulary code (e.g.
#' `"dwc"`). See [valid_vacabularies()] for details.
#' @rdname tdwg_
#' @order 2
#' @export
tdwg_vocabularies <- function(.query, 
                              vocabulary = NULL,
                              version = NULL){
  check_tdwg_object(.query)
  # if `standards` is already set, use that information to filter
  if(any(names(.query) == "standards")){
    df <- filter_vocabularies(.query)
  }else{ # otherwise, apply version
    df <- show_vocabularies()
    if(!is.null(version)){
      df <- df |> filter_version(version = version,
                                 text_field = "vocabulary_status", 
                                 date_field = "version_issued")
    }
  }
  # subset by vocabulary
  if(!is.null(vocabulary)){
    df <- df |>
      filter_text(text = vocabulary,
                  field1 = "label",
                  field2 = "vocabulary")    
  }
  # update .query
  update_tdwg(.query, vocabularies = df)
}