# Tests run with the working directory set to tests/testthat/ (both under
# devtools::test() and testthat::test_check()), while the {targets} store
# lives at the project root. testthat::test_path() resolves reliably
# regardless of how the tests are invoked, so route all target reads
# through this helper instead of calling targets::tar_read()/tar_load()
# directly.
tar_read_test <- function(name) {
  targets::tar_read_raw(name, store = testthat::test_path("..", "..", "_targets"))
}
