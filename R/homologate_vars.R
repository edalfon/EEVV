#' Homologate variables across years
#'
#' This will be a monster function!, Sorry about that!
#' Basically it contains a -hopefully short- code snippet for every
#' variable in the nacimientos dataset. The goal is to make sure
#' the same variable is comparable across years. This requires
#' sometimes recoding levels/values/labels because variables have
#' changed over time (e.g. new categories introduced, new variables,
#' or removed variables). So for each variable we will decide what to do
#' individually, using the data summary in a previous target inspect_vars_info
#'
#' @param nac_raw a dataframe with nacimientos data, including
#' columns year and sav_data, that containes nested data frame for each year
#' as returned by `ingest_eevv_nac`
#' @param ddi_nac_vars A data frame containing information about variables from
#' Nacimientos in DDI files as returned by `ingest_eevv_ddi`
#' @return a data frame with a similar structure as the input nac_raw
#' (nested data frames in the sav_data variable), but one that makes sure
#' the variables are all comparable so that they can be row-binded
#' @seealso `ingest_eevv_nac`, `ingest_eevv_ddi`
#' @export
homologate_vars <- function(nac_raw, ddi_nac_vars, ...) {
  nac_homo <- nac_raw |>
    mutate(sav_data = purrr::map(sav_data, function(nac_i) {
      ano_i <- na.omit(unique(nac_i$ano))

      # ano #################################################
      nac_i$ano <- as.character(nac_i$ano)

      # apgar #################################################
      if (ano_i <= 2007) {
        nac_i <- nac_i |>
          mutate(across(starts_with("apgar"), as.numeric)) |>
          mutate(across(starts_with("apgar"), \(x) na_if(x, 9))) |>
          mutate(apgar1_old = apgar1, apgar1 = NA) |>
          mutate(apgar2_old = apgar2, apgar2 = NA)
      } else {
        nac_i <- nac_i |>
          # need to do as.character first, because there are haven_labelled
          # in there and directly to numeric would error
          mutate(across(starts_with("apgar"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(99)
          })) |>
          mutate(across(starts_with("apgar"), .names = "{.col}_old", \(x) {
            case_when(x < 5 ~ 1, x %in% 5:6 ~ 2, x %in% 7:10 ~ 3)
          }))
      }

      # area_res, areanac #################################################
      nac_i <- mutate(nac_i, across(c(areanac, area_res), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("areanac", 2014, ddi_nac_vars)
      }))

      # aten_par #################################################
      nac_i <- mutate(nac_i, across(c(aten_par), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("aten_par", 2014, ddi_nac_vars)
      }))

      # codes #################################################
      nac_i <- mutate(nac_i, across(c(
        cod_dpto, cod_munic, codmunre, codpres, codptore
      ), \(x){
        as.character(x)
      }))
      if (ano_i <= 2017) {
        nac_i <- mutate(nac_i, across(c(cod_inst), \(x){
          as.character(x)
        }))
      }

      # edad_madre #################################################
      nac_i <- mutate(nac_i, across(c(edad_madre), \(x){
        as.numeric(as.character(x)) |>
          na_if(99) |>
          encode_ddi_factor("edad_madre", 2014, ddi_nac_vars)
      }))

      # edad_padre #################################################
      nac_i <- mutate(nac_i, across(c(edad_padre), \(x){
        as.numeric(as.character(x)) |>
          na_if(99)
      }))

      # est_civm #################################################
      if (ano_i >= 2008) {
        nac_i <- nac_i |>
          mutate(est_civm_old = na_if(as.numeric(as.character(est_civm)), 9)) |>
          mutate(across(c(est_civm_old), \(x){
            case_when(
              x == 1 ~ 4,
              x == 2 ~ 1,
              x == 3 ~ 5,
              x == 4 ~ 3,
              x == 5 ~ 1,
              x == 6 ~ 2,
              .default = NA
            ) |> encode_ddi_factor("est_civm", 2007, ddi_nac_vars)
          })) |>
          mutate(across(c(est_civm), \(x){
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("est_civm", 2014, ddi_nac_vars)
          }))
      } else {
        nac_i <- rename(nac_i, est_civm_old = est_civm) |>
          mutate(across(c(est_civm_old), \(x){
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("est_civm", 2007, ddi_nac_vars)
          }))
      }

      # gru_san #################################################
      nac_i <- mutate(nac_i, across(starts_with("gru_san"), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("gru_san", 2007, ddi_nac_vars)
      }))

      # idclasadmi #################################################
      nac_i <- mutate(nac_i, across(starts_with("idclasadmi"), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("idclasadmi", 2008, ddi_nac_vars)
      }))

      # idfactorrh #################################################
      nac_i <- mutate(nac_i, across(starts_with("idfactorrh"), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("idfactorrh", 2008, ddi_nac_vars)
      }))

      # idhemoclas #################################################
      nac_i <- mutate(nac_i, across(starts_with("idhemoclas"), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("idhemoclas", 2008, ddi_nac_vars)
      }))

      # _homo for: factorrh, homoclas, gru_san #################################
      if (ano_i >= 2008) {
        nac_i <- nac_i |>
          mutate(factorrh_homo = idfactorrh) |>
          mutate(hemoclas_homo = idhemoclas) |>
          mutate(gru_san_homo = paste(hemoclas_homo, factorrh_homo))
      } else {
        nac_i <- nac_i |>
          mutate(
            gru_san_homo = gru_san |>
              str_extract("\\((.*?)\\)") |>
              str_replace_all("\\(", "") |>
              str_replace_all("\\)", "")
          ) |>
          tidyr::separate(
            gru_san_homo,
            into = c("hemoclas_homo", "factorrh_homo"), sep = " "
          )
      }

      # idpertet #################################################
      nac_i <- mutate(nac_i, across(starts_with("idpertet"), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("idpertet", 2008, ddi_nac_vars)
      })) |>
        # done using across + .names to make it work when idpertet doesn't exist
        mutate(across(starts_with("idpertet"), .names = "pertet_homo", \(x) x))

      # idpuebloin #################################################
      nac_i <- mutate(nac_i, across(starts_with("idpuebloin"), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("idpuebloin", 2017, ddi_nac_vars)
      })) |>
        mutate(across(starts_with("idpertet"), .names = "pertet_homo", \(x) x))

      # mes #################################################
      nac_i <- mutate(nac_i, across(mes, \(x){
        as.numeric(as.character(x))
      }))

      # mul_parto #################################################
      nac_i <- mutate(nac_i, across(any_of(c("mul_parto")), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("mul_parto", 2014, ddi_nac_vars)
      }))

      # n_emb #################################################
      nac_i <- mutate(nac_i, across(any_of(c("n_emb", "n_hijosv")), \(x){
        as.numeric(as.character(x)) |>
          na_if(99)
      }))

      # niv_edum, niv_edup #################################################
      if (ano_i <= 2007) {
        nac_i <- rename(nac_i, niv_edum_old = niv_edum, niv_edup_old = niv_edup) |>
          mutate(across(starts_with("niv_edu"), \(x){
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("niv_edum", 2007, ddi_nac_vars)
          }))
      } else {
        nac_i <- rename(nac_i, niv_edum_new = niv_edum, niv_edup_new = niv_edup) |>
          mutate(across(starts_with("niv_edu"), \(x){
            as.numeric(as.character(x)) |>
              na_if(99) |>
              encode_ddi_factor("niv_edum", 2008, ddi_nac_vars)
          }))
      }

      # "nom_inst", "nomclasad", "otro_sit", "otrparatx" #############
      nac_i <- mutate(nac_i, across(any_of(c(
        "nom_inst", "nomclasad", "otro_sit", "otrparatx"
      )), \(x){
        as.character(x)
      }))

      # numconsul #################################################
      nac_i <- mutate(nac_i, across(any_of(c("numconsul")), \(x){
        as.numeric(as.character(x)) |>
          na_if(99)
      }))

      # peso_nac #################################################
      nac_i <- mutate(nac_i, across(any_of(c("peso_nac")), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("peso_nac", 2014, ddi_nac_vars)
      }))

      # profesion #################################################
      if (ano_i <= 2007) {
        nac_i <- rename(nac_i, profesion_old = profesion) |>
          mutate(across(starts_with("profesion"), \(x){
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("profesion", 2007, ddi_nac_vars)
          }))
      } else {
        nac_i <- rename(nac_i, profesion_new = profesion) |>
          mutate(across(starts_with("profesion"), \(x){
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("profesion", 2008, ddi_nac_vars)
          }))
      }

      # seg_social #################################################
      if (ano_i <= 2007) {
        nac_i <- rename(nac_i, seg_social_old = seg_social) |>
          mutate(across(starts_with("seg_social"), \(x){
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("seg_social", 2007, ddi_nac_vars)
          }))
      } else {
        nac_i <- rename(nac_i, seg_social_new = seg_social) |>
          mutate(across(starts_with("seg_social"), \(x){
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("seg_social", 2008, ddi_nac_vars)
          }))
      }

      # sexo #################################################
      nac_i <- mutate(nac_i, across(any_of(c("sexo")), \(x){
        as.numeric(as.character(x)) |>
          encode_ddi_factor("sexo", 2014, ddi_nac_vars)
      }))

      # sit_parto #################################################
      nac_i <- mutate(nac_i, across(any_of(c("sit_parto")), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("sit_parto", 2014, ddi_nac_vars)
      }))

      # t_ges #################################################
      nac_i <- mutate(nac_i, across(any_of(c("t_ges")), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("t_ges", 2014, ddi_nac_vars)
      }))

      # talla_nac #################################################
      nac_i <- mutate(nac_i, across(any_of(c("talla_nac")), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("talla_nac", 2014, ddi_nac_vars)
      }))

      # tipo_parto #################################################
      nac_i <- mutate(nac_i, across(any_of(c("tipo_parto")), \(x){
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("tipo_parto", 2014, ddi_nac_vars)
      }))

      # ultcurmad, ultcurpad #################################################
      nac_i <- mutate(nac_i, across(any_of(c("ultcurmad", "ultcurpad")), \(x){
        as.numeric(as.character(x)) |>
          na_if(99)
      }))

      # do not forget you are running within a function, so return nac_i
      nac_i
    }))

  nac_homo
}
