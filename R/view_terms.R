#' View DwC terms in the browser
#'
#' This is a prototype initially imported from `galaxias`. Requires more care
#' to optimize for `tawnydragon`.
#' @importFrom shiny shinyApp
#' @export
view_terms <- function() {
  shinyApp(view_terms_ui(df), 
           view_terms_server) |>
    print()  
}

#" UI for `view_terms()`
#' @importFrom bslib bs_theme
#' @importFrom bslib page_fillable
#' @importFrom htmltools h4
#' @noRd
#' @keywords Internal
view_terms_ui <- function(df){
  page_fillable(
    title = "galaxias::map_terms",
    theme = bs_theme(bootswatch = "minty"),
    h4("Darwin Core terms"),
    terms_list()
  )
}

#' simple server function. May require more detail later
#' @noRd
#' @keywords Internal
view_terms_server <- function(input, output) {}

#' build a list of terms with navbar
#' @importFrom bslib navset_pill_list
#' @importFrom bslib nav_panel
#' @importFrom dplyr filter
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
terms_list <- function(){
  navset_pill_list(
    !!!map(
      .x = get_terms()$parents$code,
      .f = \(x){
        nav_panel(
          title = x,
          terms_div(
            terms = {get_terms() |>
                pluck("terms") |>
                filter(parent_class == x)},
            group = x)
        )
      }
    ),
    well = FALSE
  )
}

#' Internal function to convert terms to boxes
#' @importFrom htmltools div
#' @importFrom purrr map
#' @importFrom shiny markdown
#' @noRd
#' @keywords Internal
terms_div <- function(terms, group){
  div(
    class = "panel-body",
    id = paste0("sort_terms_", group),
    map(.x = seq_len(nrow(terms)),
        .f = \(x){
          custom_value_box(
            term = terms$code[x],
            markdown(paste0("*", terms$description[x], "*")),
            markdown(paste0("**Example:** ", 
                            strsplit(terms$examples[x], ";")[[1]][1]
            )),
            theme = "success"
          )
          # card(
          #      class = "terms",
          #      card_body(terms$code[x], class = "value-box-title"),
          #      shiny::markdown(terms$description[x])
          #      # title = terms$description[x],
          #      # value = terms$code[x]
          #      # card_footer(terms$examples[x]))
          # ) 
        })
  )
}

#' Internal function to build own boxes - heavily based on `value_box()` code
#' @importFrom bslib card
#' @importFrom bslib as_fill_carrier
#' @importFrom bslib value_box_theme
#' @importFrom htmltools css
#' @importFrom htmltools div
#' @importFrom htmltools p
#' @noRd
#' @keywords Internal
custom_value_box <- function(term, ..., theme){
  dots <- list(...)
  term <- p(term, class = "value-box-value")
  contents <- div(class = "value-box-area", term, !!!dots) |>
    as_fill_carrier()
  theme <- value_box_theme(theme)
  card(class = c("bslib-value-box", theme$class),
       style = css(color = theme$fg, 
                   background_color = theme$bg, 
                   `--bslib-color-fg` = theme$fg, 
                   `--bslib-color-bg` = theme$bg),
       contents)
}