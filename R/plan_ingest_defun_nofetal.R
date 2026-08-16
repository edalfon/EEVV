plan_ingest_defun_nofetal <- function() {
  tarchetypes::tar_plan(
    # ingest raw data as qs, because ingest_eevv_defun_nofetal() returns data
    # with nested data frames, that neither fst nor arrow/parquet/feather
    # can handle. Mirrors plan_ingest_eevv()
    tar_qs(defun_nofetal_raw, ingest_eevv_defun_nofetal()),

    # ingest DDI metadata for the variables of interest
    tar_target(ddi_defun_nofetal_vars, ingest_eevv_ddi("nofetal")),
    tar_target(ddi_defun_nofetal_var_labels, get_var_labels_only(ddi_defun_nofetal_vars)),

    # there have been a few changes in the variables and the categories of some
    # of them. Thus, we cannot simply row-bind the data from all years. We would
    # rather take a look at the differences and then make a decision on how to
    # homologate variables of interest, when their coding has changed over time
    tar_target(vars_info_defun_nofetal, check_vars(defun_nofetal_raw)),
    tar_render(inspect_vars_info_defun_nofetal, "notebooks/inspect_vars_info_defun_nofetal.Rmd"),
    tar_render(inspect_ddi_vars_defun_nofetal, "notebooks/inspect_ddi_vars_defun_nofetal.Rmd"),

    # homologate variables of interest (ddi_nac_vars used as a fallback for
    # the handful of variables undocumented in this dataset's own DDI)
    tar_qs(
      defun_nofetal_homo,
      homologate_vars_defun_nofetal(defun_nofetal_raw, ddi_defun_nofetal_vars, ddi_nac_vars)
    ),

    # select final variables, unnest and row bind the data
    tar_parquet(
      defun_nofetal,
      select_defun_nofetal_vars(defun_nofetal_homo, ddi_defun_nofetal_var_labels)
    )
  )
}
