test_that("`vocabularies()` fails with no inputs", {
  expect_error(vocabularies())
})

test_that("`vocabularies()` selects the most recent value when many are given", {
  many_standards <- standards(label == "Darwin Core")
  expect_message({x <- many_standards |> 
                   vocabularies()})
})


test_that("full workflow is functional", {
  # valid_standards() # look up our options
  x <- standards(standard == "http://www.tdwg.org/standards/450", 
                 standard_status == "recommended") |>
    vocabularies() # gives a list of 5 entries
  # add tests here
  
  y <- standards(standard == "http://www.tdwg.org/standards/450", 
                 standard_status == "recommended") |>
       vocabularies(vocabulary == "http://rs.tdwg.org/dwc/") |>
       term_lists(list_localName == "dwc/terms/") |>
       terms()
  expect_equal(names(y),
               c("term", "label", "description", "version", "version_status"))
  expect_gt(nrow(y), 0)
})