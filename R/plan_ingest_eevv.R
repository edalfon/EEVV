plan_ingest_eevv <- function() {
  tarchetypes::tar_plan(
    # breathe

    # ingest raw data as qs, because ingest_eevv_nac() returns data with
    # nested data frames, that neither fst nor arrow/parquet/feather can handle
    tar_qs(nac_raw, ingest_eevv_nac()),

    tar_target(ddi_nac_vars, ingest_eevv_ddi()),
    tar_target(ddi_nac_var_labels, get_var_labels_only(ddi_nac_vars)),

    # there have been a few changes in the variables and the categories of some
    # of them. Thus, we cannot simply row-bind the data from all years. We would
    # rather take a look at the differences and then make a decision on how to
    # homologate variables of interest, when their coding has changed over time
    tar_target(vars_info, check_vars(nac_raw)),
    tar_render(inspect_vars_info, "notebooks/inspect_vars_info.Rmd"),
    tar_qs(nac_homo, homologate_vars(nac_raw, ddi_nac_vars, inspect_vars_info)),
    # tar_parquet(nac, homologate_vars(nac_raw, ddi_nac_vars, ddi_nac_var_labels, inspect_vars_info)),
    tar_parquet(nac, {
      # nac_homo |> print(n = 50)

      # ddi_nac_var_labels["apgar1_old"] <- ddi_nac_var_labels["apgar1"]
      # ddi_nac_var_labels["apgar2_old"] <- ddi_nac_var_labels["apgar2"]

      # # after homologating, all vars within nac_raw$sav_data should be row-bindable
      # nac <- nac_nested |>
      #   mutate(sav_data = purrr::map(sav_data, \(x) {
      #     select(x, rowid, ano, starts_with("apgar"), area_res, areanac)
      #     #|> slice_sample(n = 100)
      #   })) |>
      #   tidyr::unnest(sav_data) |>
      #   mutate(across(everything(), \(x) {
      #     attr(x, "label") <- ddi_nac_var_labels[cur_column()]
      #     x
      #   }))

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
            #|> slice_sample(n = 100)
          })
        ) |>
        tidyr::unnest(sav_data) |>
        mutate(across(everything(), \(x) {
          attr(x, "label") <- ddi_nac_var_labels[cur_column()]
          x
        }))

      nac
    }),
    # tar_qs(nac_qs, nac),
    # tar_fst(nac_fst, nac),

    # playground
    tar_target(playground, cue = tar_cue("never"), {
      tar_cancel()
    })
  )
}
