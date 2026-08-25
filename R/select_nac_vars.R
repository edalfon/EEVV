#' Select final Nacimientos variables and flatten the nested data
#'
#' Takes the homologated Nacimientos data (still nested by year in the
#' `sav_data` column) and keeps only the variables of interest for
#' downstream analysis. The nested data frames are then unnested into a
#' single flat data frame and, since `tidyr::unnest()` drops variable
#' labels, the labels are re-attached from `ddi_nac_var_labels`.
#' In addition, any invalid UTF-8 characters in character columns are fixed,
#' so that downstream loading does not fail.
#'
#' @param nac_homo a data frame with homologated nacimientos data, including
#' a `sav_data` column with nested data frames, as returned by
#' `homologate_vars`
#' @param ddi_nac_var_labels a named character vector of variable labels,
#' keyed by variable name, as returned by `get_var_labels_only`
#' @return a flat (not nested) data frame with the selected variables,
#' each one carrying its corresponding label (as the `"label"` attribute)
#' @seealso `homologate_vars`, `get_var_labels_only`
#' @export
select_nac_vars <- function(nac_homo, ddi_nac_var_labels) {
  nac <- nac_homo |>
    mutate(
      sav_data = purrr::map(sav_data, \(x) {
        select(
          x,
          rowid,
          ano,
          starts_with("apgar"),
          area_res,
          areanac,
          aten_par,
          cod_dpto,
          cod_munic,
          codmunre,
          codpres,
          codptore,
          starts_with("cod_inst"),
          edad_madre,
          edad_padre,
          starts_with("est_civm"),
          starts_with("gru_san"),
          starts_with("idfactorrh"),
          starts_with("idhemoclas"),
          any_of(c("gru_san_homo", "factorrh_homo", "hemoclas_homo")),
          any_of(c("idpertet", "idpuebloin", "pertet_homo")),
          mes,
          starts_with("niv_edu"),
          any_of(c("nom_inst", "nomclasad", "otro_sit", "otrparatx")),
          numconsul,
          peso_nac,
          any_of(c("profesion_old", "profesion_new")),
          any_of(c("seg_social_old", "seg_social_new")),
          sexo,
          sit_parto,
          t_ges,
          talla_nac,
          tipo_parto,
          any_of(c("ultcurmad", "ultcurpad"))
        )
      })
    ) |>
    tidyr::unnest(sav_data) |>
    mutate(across(everything(), \(x) {
      attr(x, "label") <- ddi_nac_var_labels[cur_column()]
      x
    }))

  # raw saving end's up with file with encoding issues,
  # so we better fix it so downstream loading does not fail
  fix_invalid_utf8 <- function(x) {
    bad <- !validUTF8(x)
    if (any(bad, na.rm = TRUE)) {
      x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
    }
    x
  }

  nac <- dplyr::mutate(
    nac,
    dplyr::across(where(is.character), fix_invalid_utf8)
  )

  nac
}
