#' Summarize each variable's class, type, and unique values across years
#'
#' Takes the nested-by-year raw data as returned by `ingest_eevv_nac()` (or
#' the analogous `ingest_eevv_defun_fetal()`/`ingest_eevv_defun_nofetal()`),
#' and produces one row per (bundle/year, variable) summarizing its R class,
#' storage type, and, for low-cardinality variables (fewer than 35 distinct
#' values), the distinct values themselves. This is meant to be inspected
#' (e.g. via `inspect_vars_info*.Rmd`) to spot where a variable's coding
#' changed across years, ahead of homologating it in `homologate_vars()` /
#' `homologate_vars_defun()`.
#'
#' @param nac_raw a data frame with nested `sav_data`, as returned by
#' `ingest_eevv_nac()`, `ingest_eevv_defun_fetal()`, or
#' `ingest_eevv_defun_nofetal()`
#' @return a data frame with one row per (bundle/year, variable), including
#' `var_class`, `var_type`, `var_nuniq`, and `var_unique` columns (the
#' latter two collapsed to a printable string), and without the original
#' `sav_data` column
#' @seealso `homologate_vars`, `homologate_vars_defun`
#' @export
check_vars <- function(nac_raw) {
  vars_info <- nac_raw |>
    # mutate(sav_data = purrr::map(sav_data, function(x) {
    #   mutate(x, across(where(function(x) "haven_labelled" %in% class(x)), haven::as_factor))
    # })) |>
    mutate(var_names = purrr::map(sav_data, names)) |>
    unnest(var_names) |>
    mutate(
      var_class = purrr::map2(sav_data, var_names, function(x, y) {
        class(x[[y]])
      }),
      var_type = purrr::map2(sav_data, var_names, function(x, y) {
        typeof(x[[y]])
      }),
      var_nuniq = purrr::map2_int(sav_data, var_names, function(x, y) {
        vctrs::vec_unique_count(x[[y]])
      }),
      var_unique = ifelse(
        test = var_nuniq < 35,
        yes = purrr::map2(sav_data, var_names, function(x, y) {
          unique(x[[y]])
        }),
        no = NA
      )
    ) |>
    mutate(across(c(var_class, var_type, var_unique), function(column) {
      purrr::map_chr(column, function(x) {
        # as.character() first: some columns (e.g. hms/difftime "hora"
        # variables) error out of paste0() directly, since paste0() doesn't
        # know how to coerce every S3 class on its own
        paste0(
          "[",
          paste0(sort(as.character(unlist(x))), collapse = "], ["),
          "]"
        )
      })
    })) |>
    select(-sav_data)

  vars_info
}
