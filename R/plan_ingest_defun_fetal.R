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
    tar_render(inspect_ddi_vars_defun_fetal, "notebooks/inspect_ddi_vars_defun_fetal.Rmd")

    # TODO once the notebooks above have been reviewed: homologate_vars_defun_fetal()
    # and select_defun_fetal_vars(), mirroring homologate_vars()/select_nac_vars()
  )
}
