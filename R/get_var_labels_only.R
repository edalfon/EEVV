#' Get only variable name/label pairs
#'
#' DDI data ingested into ddi_nac_vars contains variable labels for
#' every year and every variable. But in the end we want to row-bind
#' all years and hence, we may only want to have one label per variable.
#' This function makes just that, by aggregating the data in ddi_nac_vars
#' to select only one label per variable name and return it as a
#' named character vector, where the name if the variable name and
#' the value is the label. Thus, it can be conveniently queried like
#' `ddi_nac_var_labels[varname]` to obtain the label associated to `varname`.
#'
#' @param ddi_nac_vars A data frame containing DDI metadata as returned by
#' `ingest_eevv_ddi`.
#' @return named character vector
#' @export
get_var_labels_only <- function(ddi_nac_vars) {
  var_labl_dups <- ddi_nac_vars |>
    group_by(name, labl) |>
    summarize(n = n(), years = paste0(sort(year), collapse = ",")) |>
    arrange(name, desc(n))

  DT::datatable(var_labl_dups, plugins = "ellipsis", options = list(
    rowsGroup = list(0),
    # use the ellipsis plugin https://github.com/rstudio/DT/pull/606
    columnDefs = list(list(
      targets = "years",
      render = DT::JS("$.fn.dataTable.render.ellipsis( 10 )")
    ))
  )) |>
    # Style A Full Row https://rstudio.github.io/DT/010-style.html
    DT::formatStyle(
      "name",
      target = "row",
      backgroundColor = DT::styleEqual(
        unique(var_labl_dups$name),
        rep_len(c("#a5d9f7", "#f0e5a8"), length(unique(var_labl_dups$name)))
      )
    )

  # After individually inspecting the table above, it's just fine to take the
  # most frequent label (differences are usually lower/upper case and accent,
  # but the most frequent value tends to be the better formatted one)
  var_labl <- var_labl_dups |>
    group_by(name) |>
    slice_max(order_by = n, n = 1)

  ddi_nac_var_labels <- var_labl$labl |>
    rlang::set_names(var_labl$name)

  ddi_nac_var_labels
}
