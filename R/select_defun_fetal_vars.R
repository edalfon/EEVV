#' Select final Defunciones Fetales variables and flatten the nested data
#'
#' Mirrors `select_nac_vars()`: takes the homologated Defunciones Fetales
#' data (still nested by year in the `sav_data` column), keeps the
#' variables handled by `homologate_vars_defun()`, unnests into a single
#' flat data frame, and re-attaches variable labels (dropped by
#' `tidyr::unnest()`). Unlike `select_nac_vars()`, most variables here are
#' wrapped in `any_of()` rather than referenced bare, because several of
#' them (e.g. `hora`, `causa_667`, `t_ges_agru_cie`) only exist in a subset
#' of years -- a bare reference would error on years missing that column.
#'
#' @param defun_fetal_homo a data frame with homologated defunciones
#' fetales data, as returned by `homologate_vars_defun_fetal`
#' @param ddi_defun_fetal_var_labels a named character vector of variable
#' labels, keyed by variable name, as returned by `get_var_labels_only`
#' @return a flat (not nested) data frame with the selected variables,
#' each one carrying its corresponding label (as the `"label"` attribute)
#' @seealso `select_nac_vars`, `homologate_vars_defun_fetal`
#' @export
select_defun_fetal_vars <- function(defun_fetal_homo, ddi_defun_fetal_var_labels) {
  defun_fetal <- defun_fetal_homo |>
    mutate(
      sav_data = purrr::map(sav_data, \(x) {
        select(
          x,
          any_of(c(
            "rowid", "ano", "mes", "tipo_defun",
            "cod_dpto", "cod_munic", "codmunre", "codpres", "codptore",
            "cod_inst", "codocur", "codmunoc", "codpaisnacmad", "regsocialmadre",
            "area_res", "a_defun", "sit_defun",
            "edad_madre",
            "peso_nac", "t_ges",
            "n_hijosv", "n_hijosm",
            "idclasadmi", "idadmisalud",
            "sexo",
            "asis_med", "cons_exp", "mu_parto", "t_parto", "idprofcer",
            "man_muer", "pman_muer", "p_pman_iris",
            "hora", "minutos",
            "c_muerte", "c_muerteb", "c_muertec", "c_muerted", "c_muertee",
            "c_muertef", "c_muerteg",
            "c_bas1", "c_ant1", "c_ant2", "c_ant3", "c_ant12", "c_ant22",
            "c_ant32", "c_dir1", "c_dir12", "c_mcm1", "c_pat1", "c_pat2",
            "cau_homol", "causa_666", "causa_667", "causa_mult",
            "nom_inst", "otrsitiode",
            "ultcurmad", "t_ges_agru_cie"
          )),
          starts_with("est_civm"),
          starts_with("niv_edum"),
          starts_with("seg_social"),
          starts_with("tipo_emb")
        )
      })
    ) |>
    tidyr::unnest(sav_data) |>
    mutate(across(everything(), \(x) {
      attr(x, "label") <- ddi_defun_fetal_var_labels[cur_column()]
      x
    }))

  defun_fetal
}
