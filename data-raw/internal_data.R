devtools::load_all()
tdwg_data <- build_tdwg_list()
usethis::use_data(tdwg_data,
                  internal = TRUE,
                  overwrite = TRUE)
