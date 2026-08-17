#' {targets} plan: ingest, homologate, and compile Nacimientos
#'
#' Defines the sequence of `targets` for the Nacimientos (births) dataset:
#' ingest the raw yearly `.sav` files (`ingest_eevv_nac()`), ingest the
#' corresponding DDI variable metadata (`ingest_eevv_ddi()`), summarize
#' each variable's coding across years (`check_vars()`, rendered via the
#' `inspect_vars_info.Rmd`/`inspect_ddi_vars.Rmd` notebooks) so that
#' coding changes can be reviewed before homologating, homologate the
#' variables of interest (`homologate_vars()`), and finally select/unnest
#' the homologated variables into the final `nac` table
#' (`select_nac_vars()`), written out as parquet.
#'
#' Called from `_targets.R`; mirrors `plan_ingest_defun_fetal()` and
#' `plan_ingest_defun_nofetal()` for the Defunciones datasets.
#'
#' @return a list of `targets` target objects, as built by
#' `tarchetypes::tar_plan()`
#' @seealso `plan_ingest_defun_fetal`, `plan_ingest_defun_nofetal`
#' @noRd
plan_ingest_nac <- function() {
  tarchetypes::tar_plan(
    # breathe

    # ingest raw data as qs, because ingest_eevv_nac() returns data with
    # nested data frames, that neither fst nor arrow/parquet/feather can handle
    tar_qs(nac_raw, ingest_eevv_nac()),

    # ingest DDI metadata for the variables of interest
    tar_target(ddi_nac_vars, ingest_eevv_ddi()),
    tar_target(ddi_nac_var_labels, get_var_labels_only(ddi_nac_vars)),

    # there have been a few changes in the variables and the categories of some
    # of them. Thus, we cannot simply row-bind the data from all years. We would
    # rather take a look at the differences and then make a decision on how to
    # homologate variables of interest, when their coding has changed over time
    tar_target(vars_info, check_vars(nac_raw)),
    tar_render(inspect_vars_info, "notebooks/inspect_vars_info.Rmd"),
    tar_render(inspect_ddi_vars, "notebooks/inspect_ddi_vars.Rmd"),

    # homologate variables of interest
    tar_qs(nac_homo, homologate_vars(nac_raw, ddi_nac_vars, inspect_vars_info)),

    # select final variables for the NAC dataset, unnest and row bind the data
    tar_parquet(nac, select_nac_vars(nac_homo, ddi_nac_var_labels)),
    # tar_qs(nac_qs, nac),
    # tar_fst(nac_fst, nac),
  )
}
