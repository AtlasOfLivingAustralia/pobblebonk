#' @rdname tdwg_
#' @order 3
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export
tdwg_termlists <- function(.query,
                           version = NULL,
                           termlist = NULL
                           ){
  check_tdwg_object(.query)
  available_names <- names(.query)
  if(!any(available_names == "vocabularies")){
    if(any(available_names == "standards")){
      df <- .query |>
        tdwg_vocabularies() |>
        filter_termlists()
    }else{
      # no available data goes here
      df <- show_termlists()
      if(!is.null(version)){
        df <- df |> filter_version(version = version,
                                   text_field = "status", 
                                   date_field = "version_modified")
      }
    }
  }else{ # properly piped solution goes here
     df <- filter_termlists(.query)
  }
  # subset by vocabulary
  if(!is.null(termlist)){
    df <- df |>
      filter_text(text = termlist,
                  field1 = "label",
                  field2 = "vann_preferredNamespacePrefix")    
  }
  # update .query
  update_tdwg(.query, termlists = df)

}
