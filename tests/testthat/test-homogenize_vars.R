test_that("homogenize_vars works", {
  nac_raw <- tar_read(nac_raw)

  expect_silent(homologate_vars(nac_raw))


  # apgar1: check specific values, using the rowid #######
  expect_equal(9, nac_raw |>
    filter(sav_file == "d620_1999.sav") |>
    pull(sav_data) |>
    first() |>
    filter(rowid == 539063) |>
    select(apgar1) |>
    as.numeric())

  expect_equal(TRUE, nac |>
    filter(sav_file == "d620_1999.sav") |>
    filter(rowid == 539063) |>
    select(apgar1_old) |>
    is.na() |> all())

  #
  expect_equal(
    object = nac_raw |>
      filter(sav_file == "d620_2004.sav") |>
      pull(sav_data) |>
      first() |>
      pull(apgar1) |>
      vctrs::vec_count() |>
      pull(count) |>
      sort(),
    expected = nac |>
      filter(sav_file == "d620_2004.sav") |>
      pull(apgar1_old) |> # note we want to compare with the renamed data
      vctrs::vec_count() |>
      pull(count) |>
      sort()
  )

  expect_equal(
    object = nac_raw |>
      filter(sav_file == "Nac_2014.sav") |>
      pull(sav_data) |>
      first() |>
      pull(apgar1) |>
      na_if("") |>
      na_if("99") |>
      vctrs::vec_count() |>
      pull(count) |>
      sort(),
    expected = nac |>
      filter(sav_file == "Nac_2014.sav") |>
      pull(apgar1) |>
      vctrs::vec_count() |>
      pull(count) |>
      sort()
  )
})
