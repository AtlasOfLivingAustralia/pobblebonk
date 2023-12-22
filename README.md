
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tawnydragon <img src="man/figures/logo.png" align="right" style="margin: 0px 10px 0px 10px;" alt="" height="138"/>

**Easy Biodiversity Information Standards**

The [Biodiversity Information Standards](http://www.tdwg.org) Group -
commonly known as TDWG - develops and standards for sharing biodiversity
information. `{tawnydragon}` is a fast and tidy R interface to those
standards.

The package name refers is one common name for *Ctenophorus decresii*, a
species whose common name happens to contain all of the letters in TDWG
(albeit not in order!)

To install from GitHub:

``` r
install.packages("remotes")
remotes::install_github("AtlasOfLivingAustralia/tawnydragon")
```

You can use `tawnydragon` to show data from the TDWG information
hierarchy, which starts with `standards` and moves down through
`vocabularies`, `termlists` and finally `terms`. These functions have
the prefix `show_`. To show available standards (including past
versions), for example, you use:

``` r
library(tawnydragon)
show_standards()
#> # A tibble: 36 × 6
#>    code  date       label            description                    status key  
#>    <chr> <date>     <chr>            <chr>                          <chr>  <chr>
#>  1 450   2023-09-18 Darwin Core      Darwin Core is a standard mai… recom… http…
#>  2 450   2023-09-13 Darwin Core      Darwin Core is a standard mai… super… http…
#>  3 638   2023-09-05 Audiovisual Core The Audiovisual Core (AC) is … recom… http…
#>  4 450   2023-07-07 Darwin Core      Darwin Core is a standard mai… super… http…
#>  5 450   2023-06-28 Darwin Core      Darwin Core is a standard mai… super… http…
#>  6 638   2023-04-26 Audiovisual Core The Audiovisual Core (AC) is … super… http…
#>  7 638   2023-02-24 Audiovisual Core The Audiovisual Core (AC) is … super… http…
#>  8 638   2022-02-23 Audubon Core     The Audubon Core (AC) is a se… super… http…
#>  9 638   2021-10-05 Audubon Core     The Audubon Core (AC) is a se… super… http…
#> 10 450   2021-09-01 Darwin Core      Darwin Core is a standard mai… super… http…
#> # ℹ 26 more rows
```

All functions in `tawnydragon`, and all `show_` functions return
information in reverse chronological order (i.e. most recent first). For
obvious reasons, these tibbles get larger the lower down in the
hierarchy you go.

``` r
show_terms()
#> # A tibble: 839 × 9
#>    code    date       parent_class label description examples type  status key  
#>    <chr>   <date>     <chr>        <chr> <chr>       <chr>    <chr> <chr>  <chr>
#>  1 Event   2023-09-18 <NA>         Event An action … `a spec… class recom… http…
#>  2 Fossil… 2023-09-18 <NA>         Foss… A preserve… `a body… class recom… http…
#>  3 Geolog… 2023-09-18 <NA>         Geol… Geological… `a lith… class recom… http…
#>  4 HumanO… 2023-09-18 <NA>         Huma… An output … `eviden… class recom… http…
#>  5 Identi… 2023-09-18 <NA>         Iden… A taxonomi… `a subs… class recom… http…
#>  6 Living… 2023-09-18 <NA>         Livi… A specimen… `a livi… class recom… http…
#>  7 Machin… 2023-09-18 <NA>         Mach… An output … `a phot… class recom… http…
#>  8 Materi… 2023-09-18 <NA>         Mate… A referenc… `a cita… class recom… http…
#>  9 Occurr… 2023-09-18 <NA>         Occu… An existen… `a wolf… class recom… http…
#> 10 Organi… 2023-09-18 <NA>         Orga… A particul… `a spec… class recom… http…
#> # ℹ 829 more rows
```

Of course, looking at all past and present terms for any standard is not
especially useful; It makes more sense to choose a single standard and
version, and show only those terms within it. To pipe these tibbles
together in the correct order, you start with a call to `tdwg()` then
pipe your levels together with `tdwg_` functions. All functions accept
arguments to pass to `dplyr::filter()`. If you end the pipe with
`summarize()`, only the `terms` slot of the `tdwg` object will be
returned.

``` r
tdwg() |>
  tdwg_standards(label == "Darwin Core",
                 status == "recommended") |>
  tdwg_terms(type == "term") |>
  summarize()
#> # A tibble: 199 × 4
#>    parent_class      code                         description             status
#>    <chr>             <chr>                        <chr>                   <chr> 
#>  1 GeologicalContext bed                          "The full name of the … recom…
#>  2 GeologicalContext earliestAgeOrLowestStage     "The full name of the … recom…
#>  3 GeologicalContext earliestEonOrLowestEonothem  "The full name of the … recom…
#>  4 GeologicalContext earliestEpochOrLowestSeries  "The full name of the … recom…
#>  5 GeologicalContext earliestEraOrLowestErathem   "The full name of the … recom…
#>  6 GeologicalContext earliestPeriodOrLowestSystem "The full name of the … recom…
#>  7 GeologicalContext formation                    "The full name of the … recom…
#>  8 GeologicalContext group                        "The full name of the … recom…
#>  9 GeologicalContext highestBiostratigraphicZone  "The full name of the … recom…
#> 10 GeologicalContext latestAgeOrHighestStage      "The full name of the … recom…
#> # ℹ 189 more rows
```
