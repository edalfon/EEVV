# Regression tests: compare the final compiled datasets against counts
# published in DANE's official EEVV statistics
# (https://www.dane.gov.co/index.php/estadisticas-por-tema/salud/nacimientos-y-defunciones).
#
# To add a new check, just add a row to `official_counts` below --
# no new test code needed. `dataset` must be the name of a {targets}
# target resolving to a data frame with an `ano` column (e.g. "nac",
# "defun_fetal", "defun_nofetal").

official_counts <- tibble::tribble(
  ~dataset , ~ano   , ~n     ,
  "nac"    , "2024" , 453901 ,
  "nac"    , "2023" , 515549 ,
  "nac"    , "2022" , 573625 ,
  "nac"    , "2021" , 616914 ,
  "nac"    , "2020" , 629402 ,
  "nac"    , "2019" , 642660 ,
  "nac"    , "2018" , 649115 ,
  "nac"    , "2017" , 656704 ,
  "nac"    , "2016" , 647521 ,
  "nac"    , "2015" , 660999 ,
  "nac"    , "2014" , 669137 ,
  "nac"    , "2013" , 658835 ,
  "nac"    , "2012" , 676835 ,
  "nac"    , "2011" , 665499 ,
  "nac"    , "2010" , 654627 ,
  "nac"    , "2009" , 699775 ,
  "nac"    , "2008" , 715453 ,
  "nac"    , "2007" , 709253 ,
  "nac"    , "2006" , 714450 ,
  "nac"    , "2005" , 719968 ,
  "nac"    , "2004" , 723099 ,
  "nac"    , "2003" , 710702 ,
  "nac"    , "2002" , 700455 ,
  "nac"    , "2001" , 724319 ,
  "nac"    , "2000" , 752834 ,
  "nac"    , "1999" , 746194 ,
  "nac"    , "1998" , 720984 ,
)

# cache targets so multiple rows for the same dataset only hit disk once
# (tar_read_test() is defined in helper.R)
read_target_cached <- local({
  cache <- new.env(parent = emptyenv())
  function(dataset) {
    if (!exists(dataset, envir = cache, inherits = FALSE)) {
      assign(dataset, tar_read_test(dataset), envir = cache)
    }
    get(dataset, envir = cache, inherits = FALSE)
  }
})

purrr::pwalk(official_counts, function(dataset, ano, n) {
  test_that(paste(dataset, ano, "matches DANE's official count"), {
    actual <- read_target_cached(dataset) |>
      filter(ano == !!ano) |>
      nrow()

    expect_equal(actual, n)
  })
})
