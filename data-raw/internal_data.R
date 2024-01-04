# code to build internal data
library(devtools)
library(purrr)
library(dplyr)
library(galah)

# get info from tdwg
tdwg_data <- build_tdwg_list()

# get term frequencies from ALA
unique_terms <- tdwg_data |>
  pluck(terms) |>
  filter(type == "term") |>
  pull(code) |>
  unique()

# create a tibble
term_counts <- tibble(
  dwc_term = unique_terms,
  count = NA)

## get number of records that have each field populated in the ALA (c/o `galah`)
## this is a proxy for how often each term is used, which is better for ranking
## than e.g. alphabetically
available_terms <- show_all_fields() |>
  pull(id)
available_check <- unique_terms %in% available_terms
count_results <- map(
  .x = unique_terms[available_check],
  .f = \(a){
    Sys.sleep(1) # rate limit to one per second
    galah_call() |>
      filter(!is.na(!!!a)) |>
      count() |>
      collect() |>
      pull(count)
  },
  .progress = TRUE
) |>
  unlist()

# update `term_counts` tibble
term_counts$count[!available_check] <- 0
term_counts$count[available_check] <- count_results

# make a `proportion` column
total_count <- atlas_counts() |> pull(count)
term_counts <- term_counts |>
  mutate(weight = count / total_count)

## check most common terms
# term_counts |>
#   arrange(desc(proportion)) |>
#   print(n = 40)

# join to `terms`
new_terms <- tdwg_data$terms |>
  left_join(select(term_counts, -count), 
            by = c("code" = "dwc_term"))

new_terms$weight[which(is.na(new_terms$weight))] <- 0
tdwg_data$terms <- new_terms

# cache with package
usethis::use_data(tdwg_data,
                  internal = TRUE,
                  overwrite = TRUE)