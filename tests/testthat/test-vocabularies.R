test_that("`vocabularies()` fails with no inputs", {
  expect_error(vocabularies())
})

test_that("`vocabularies()` selects the most recent value when many are given", {
  many_standards <- get_standards(label == "Darwin Core")
  expect_message({x <- many_standards |> 
                   get_vocabularies()})
})


test_that("full workflow is functional", {
  # valid_standards() # look up our options
  x <- get_standards(standard == "http://www.tdwg.org/standards/450", 
                 standard_status == "recommended") |>
    get_vocabularies() # gives a list of 5 entries
  # add tests here
  
  y <- get_standards(standard == "http://www.tdwg.org/standards/450", 
                 standard_status == "recommended") |>
       get_vocabularies(vocabulary == "http://rs.tdwg.org/dwc/") |>
       get_termlists(list_localName == "dwc/terms/") |>
       get_terms()
  
  # Note
    # `terms` are listed within `classes`; but both classes and terms are within terms_versions.csv
    # the `rdf_type` field distinguishes them by the suffixes `Property` (term) and `Class` (class)
    # this should be an intermediate step between `term_lists()` (whose function is still unclear) 
    # and `terms`
  
  # oddly, while many terms are within classes (e.g. `Event` class has many terms)
  # some classes are within terms (e.g. `LivingSpecimen` is an event within `basisOfRecord`)
  expect_equal(names(y),
               c("term", "label", "description", "version", "version_status"))
  expect_gt(nrow(y), 0)
})