#' Select final Defunciones No Fetales variables and flatten the nested data
#'
#' Mirrors `select_nac_vars()`/`select_defun_fetal_vars()`: takes the
#' homologated Defunciones No Fetales data (still nested by year in the
#' `sav_data` column), keeps the variables handled by
#' `homologate_vars_defun()`/`homologate_vars_defun_nofetal()`, unnests
#' into a single flat data frame, and re-attaches variable labels (dropped
#' by `tidyr::unnest()`). Most variables are wrapped in `any_of()` rather
#' than referenced bare, since several only exist in a subset of years.
#'
#' @param defun_nofetal_homo a data frame with homologated defunciones no
#' fetales data, as returned by `homologate_vars_defun_nofetal`
#' @param ddi_defun_nofetal_var_labels a named character vector of variable
#' labels, keyed by variable name, as returned by `get_var_labels_only`
#' @return a flat (not nested) data frame with the selected variables,
#' each one carrying its corresponding label (as the `"label"` attribute)
#' @seealso `select_nac_vars`, `select_defun_fetal_vars`,
#' `homologate_vars_defun_nofetal`
#' @export
select_defun_nofetal_vars <- function(defun_nofetal_homo, ddi_defun_nofetal_var_labels) {
  defun_nofetal <- defun_nofetal_homo |>
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
            "ultcurmad", "t_ges_agru_cie",
            # nofetal-specific (deceased's own demographics, pregnancy flags)
            "idpertet", "muerteporo", "simuertepo",
            "emb_fal", "emb_mes", "emb_sem",
            "tipoformulario", "gru_ed2",
            "ultcurfal", "ocupacion", "gru_ed1", "codpaisnacfal"
          )),
          starts_with("est_civm"),
          starts_with("niv_edum"),
          starts_with("seg_social"),
          starts_with("tipo_emb"),
          starts_with("est_civil"),
          starts_with("nivel_edu")
        )
      })
    ) |>
    tidyr::unnest(sav_data) |>
    mutate(across(everything(), \(x) {
      attr(x, "label") <- ddi_defun_nofetal_var_labels[cur_column()]
      x
    }))

  defun_nofetal
}
