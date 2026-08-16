#' Homologate variables of interest for Defunciones Fetales
#'
#' Fetal deaths carry the same set of variables as `homologate_vars_defun()`
#' already handles (no variable is specific to this dataset that isn't also
#' present in Defunciones No Fetales), so this is a thin wrapper around it.
#' See `homologate_vars_defun()` for the actual per-variable logic and its
#' caveats (notably, cause-of-death and manner-of-death coding are left as
#' raw pass-throughs, not homologated).
#'
#' @inheritParams homologate_vars_defun
#' @param defun_fetal_raw a data frame with defunciones fetales data, as
#' returned by `ingest_eevv_defun_fetal`
#' @param ddi_defun_fetal_vars DDI metadata for defunciones fetales, as
#' returned by `ingest_eevv_ddi("fetal")`
#' @return a data frame with a similar structure as `defun_fetal_raw`
#' @seealso `homologate_vars_defun`, `homologate_vars_defun_nofetal`
#' @export
homologate_vars_defun_fetal <- function(defun_fetal_raw, ddi_defun_fetal_vars, ddi_nac_vars) {
  homologate_vars_defun(defun_fetal_raw, ddi_defun_fetal_vars, ddi_nac_vars)
}
