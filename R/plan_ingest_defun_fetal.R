#' {targets} plan: ingest, homologate, and compile Defunciones Fetales
#'
#' Defines the sequence of `targets` for the Defunciones Fetales (fetal
#' deaths) dataset: ingest the raw yearly `.sav` files
#' (`ingest_eevv_defun_fetal()`), ingest the corresponding DDI variable
#' metadata (`ingest_eevv_ddi("fetal")`), summarize each variable's coding
#' across years (`check_vars()`, rendered via the
#' `inspect_vars_info_defun_fetal.Rmd`/`inspect_ddi_vars_defun_fetal.Rmd`
#' notebooks), homologate the variables of interest
#' (`homologate_vars_defun_fetal()`, using `ddi_nac_vars` as a fallback for
#' the handful of variables undocumented in this dataset's own DDI), and
#' finally select/unnest the homologated variables into the final
#' `defun_fetal` table (`select_defun_fetal_vars()`), written out as
#' parquet.
#'
#' Called from `_targets.R`; mirrors `plan_ingest_nac()` and
#' `plan_ingest_defun_nofetal()`.
#'
#' @return a list of `targets` target objects, as built by
#' `tarchetypes::tar_plan()`
#' @seealso `plan_ingest_nac`, `plan_ingest_defun_nofetal`
#' @noRd
plan_ingest_defun_fetal <- function() {
  tarchetypes::tar_plan(
    # ingest raw data as qs, because ingest_eevv_defun_fetal() returns data
    # with nested data frames, that neither fst nor arrow/parquet/feather
    # can handle. Mirrors plan_ingest_eevv()
    tar_qs(defun_fetal_raw, ingest_eevv_defun_fetal()),

    # ingest DDI metadata for the variables of interest
    tar_target(ddi_defun_fetal_vars, ingest_eevv_ddi("fetal")),
    tar_target(ddi_defun_fetal_var_labels, get_var_labels_only(ddi_defun_fetal_vars)),

    # there have been a few changes in the variables and the categories of some
    # of them. Thus, we cannot simply row-bind the data from all years. We would
    # rather take a look at the differences and then make a decision on how to
    # homologate variables of interest, when their coding has changed over time
    tar_target(vars_info_defun_fetal, check_vars(defun_fetal_raw)),
    tar_render(inspect_vars_info_defun_fetal, "notebooks/inspect_vars_info_defun_fetal.Rmd"),
    tar_render(inspect_ddi_vars_defun_fetal, "notebooks/inspect_ddi_vars_defun_fetal.Rmd"),

    # homologate variables of interest (ddi_nac_vars used as a fallback for
    # the handful of variables undocumented in this dataset's own DDI)
    tar_qs(
      defun_fetal_homo,
      homologate_vars_defun_fetal(defun_fetal_raw, ddi_defun_fetal_vars, ddi_nac_vars)
    ),

    # select final variables, unnest and row bind the data
    tar_parquet(
      defun_fetal,
      select_defun_fetal_vars(defun_fetal_homo, ddi_defun_fetal_var_labels)
    )
  )
}
