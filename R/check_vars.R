#' kda fhkjlha sdfjkha sf
#'
#' .. content for \details{} ..
#'
#' @param nac_raw
#' @return
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
