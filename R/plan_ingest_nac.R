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
