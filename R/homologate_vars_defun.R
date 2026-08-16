#' Homologate variables shared by both Defunciones datasets
#'
#' Defunciones Fetales and Defunciones No Fetales share the large majority
#' of their variables (same EEVV death-certificate form), so this function
#' holds the homologation logic common to both, mirroring the variable-by
#' -variable approach of `homologate_vars()` (for Nacimientos). It is meant
#' to be called from `homologate_vars_defun_fetal()` and
#' `homologate_vars_defun_nofetal()`, not directly.
#'
#' As with `homologate_vars()`, this reflects a first pass grounded in the
#' DDI codebooks and `inspect_vars_info_defun_*.Rmd` /
#' `inspect_ddi_vars_defun_*.Rmd`, not an exhaustive manual review of every
#' variable the way Nacimientos got (that took a long iterative process).
#' In particular, the cause-of-death coding (ICD codes: `c_bas1`, `c_ant*`,
#' `c_dir*`, `c_pat*`, `c_mcm1`, `cau_homol`, `causa_666`/`causa_667`,
#' `causa_mult`) and manner-of-death coding (`man_muer`/`pman_muer` vs. the
#' post-2019 `p_pman_iris`, which use different classification schemes) are
#' deliberately left as raw/cast-only pass-throughs rather than homologated:
#' doing that properly needs the actual DANE/OPS/WHO ICD-10 and "Lista
#' 6/67"/"Lista 105"/IRIS documentation to cross-reference codes across
#' periods, which is a research task on its own.
#'
#' @param defun_i a data frame with nacimientos data, including
#' columns year and sav_data, that contains nested data frame for each year
#' as returned by `ingest_eevv_defun_fetal`/`ingest_eevv_defun_nofetal`
#' @param ddi_defun_vars A data frame containing information about
#' variables from the corresponding Defunciones dataset in DDI files, as
#' returned by `ingest_eevv_ddi("fetal")`/`ingest_eevv_ddi("nofetal")`
#' @param ddi_nac_vars A data frame containing information about variables
#' from Nacimientos in DDI files as returned by `ingest_eevv_ddi()`. Used
#' as a fallback for variables whose categories are, for whatever reason,
#' undocumented in the Defunciones DDI (currently just `idclasadmi`) but
#' documented in the Nacimientos one for the same underlying EEVV coding
#' system.
#' @return a data frame with a similar structure as the input `defun_i`
#' (nested data frames in the `sav_data` variable)
#' @seealso `homologate_vars`, `homologate_vars_defun_fetal`,
#' `homologate_vars_defun_nofetal`
#' @export
homologate_vars_defun <- function(defun_i, ddi_defun_vars, ddi_nac_vars) {
  # renames `old` to `new` if `old` is present, no-ops otherwise -- unlike
  # nac_raw, several variables here aren't guaranteed present every year,
  # so a bare rename(new = old) would error on years missing that column
  rename_if_present <- function(df, old, new) {
    if (old %in% names(df)) names(df)[names(df) == old] <- new
    df
  }

  defun_homo <- defun_i |>
    mutate(sav_data = purrr::map(sav_data, function(x_i) {
      ano_i <- na.omit(unique(x_i$ano))

      # ano, mes, tipo_defun #################################################
      x_i$ano <- as.character(x_i$ano)
      x_i <- mutate(x_i, across(any_of(c("mes")), \(x) as.numeric(as.character(x))))
      x_i <- mutate(x_i, across(any_of(c("tipo_defun")), as.character))

      # geographic codes, just cast to character, same as nac_raw ############
      x_i <- mutate(x_i, across(any_of(c(
        "cod_dpto", "cod_munic", "codmunre", "codpres", "codptore", "cod_inst",
        "codocur", "codmunoc", "codpaisnacmad", "regsocialmadre"
      )), as.character))

      # area_res, a_defun, sit_defun (same 4/7-level structure, stable) #########
      x_i <- mutate(x_i, across(any_of(c("area_res")), \(x) {
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("area_res", 2014, ddi_defun_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("a_defun")), \(x) {
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("a_defun", 2024, ddi_defun_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("sit_defun")), \(x) {
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("sit_defun", 2024, ddi_defun_vars)
      }))

      # edad_madre #############################################################
      x_i <- mutate(x_i, across(any_of(c("edad_madre")), \(x) {
        as.numeric(as.character(x)) |>
          encode_ddi_factor("edad_madre", 2014, ddi_defun_vars)
      }))

      # est_civm (mother) #######################################################
      # unlike nac_raw, `est_civm` is not guaranteed present every year in
      # these datasets, so rename via rename_with(any_of()), which is a
      # no-op instead of erroring when the column is missing that year
      if (ano_i >= 2008) {
        x_i <- mutate(x_i, across(any_of("est_civm"), \(x) {
          as.numeric(as.character(x)) |>
            na_if(9) |>
            encode_ddi_factor("est_civm", 2014, ddi_defun_vars)
        }))
      } else {
        x_i <- rename_if_present(x_i, "est_civm", "est_civm_old") |>
          mutate(across(any_of("est_civm_old"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("est_civm", 2007, ddi_defun_vars)
          }))
      }

      # niv_edum (mother) #######################################################
      if (ano_i <= 2007) {
        x_i <- rename_if_present(x_i, "niv_edum", "niv_edum_old") |>
          mutate(across(any_of("niv_edum_old"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("niv_edum", 2007, ddi_defun_vars)
          }))
      } else {
        x_i <- rename_if_present(x_i, "niv_edum", "niv_edum_new") |>
          mutate(across(any_of("niv_edum_new"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(99) |>
              encode_ddi_factor("niv_edum", 2008, ddi_defun_vars)
          }))
      }

      # seg_social (deceased/mother) ############################################
      # codes 3/4/5 mean different things before/after 2008 (e.g. old 3 =
      # "Vinculado", new 3 = "Excepción"), so keep them apart rather than
      # attempt a value-level remap, mirroring homologate_vars()
      if (ano_i <= 2007) {
        x_i <- rename_if_present(x_i, "seg_social", "seg_social_old") |>
          mutate(across(any_of("seg_social_old"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("seg_social", 2007, ddi_defun_vars)
          }))
      } else {
        x_i <- rename_if_present(x_i, "seg_social", "seg_social_new") |>
          mutate(across(any_of("seg_social_new"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("seg_social", 2008, ddi_defun_vars)
          }))
      }

      # peso_nac, t_ges (of the fetus/child, when applicable) ###################
      x_i <- mutate(x_i, across(any_of(c("peso_nac", "t_ges")), \(x) {
        as.numeric(as.character(x))
      }))
      x_i <- mutate(x_i, across(any_of(c("peso_nac")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("peso_nac", 2014, ddi_defun_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("t_ges")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("t_ges", 2014, ddi_defun_vars)
      }))

      # n_hijosv, n_hijosm ######################################################
      x_i <- mutate(x_i, across(any_of(c("n_hijosv", "n_hijosm")), \(x) {
        as.numeric(as.character(x)) |> na_if(99)
      }))

      # idclasadmi: this dataset's own DDI never documents its categories
      # (0 categories in every year), so fall back to ddi_nac_vars, which
      # documents the exact same coding system
      x_i <- mutate(x_i, across(any_of(c("idclasadmi")), \(x) {
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("idclasadmi", 2008, ddi_nac_vars)
      }))

      # idadmisalu/idadmisalud: same variable, renamed from 2018 on (same
      # codes/labels both sides), unify under the newer name
      if ("idadmisalu" %in% names(x_i)) {
        x_i <- rename(x_i, idadmisalud = idadmisalu)
      }
      x_i <- mutate(x_i, across(any_of(c("idadmisalud")), \(x) {
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("idadmisalud", 2023, ddi_defun_vars)
      }))

      # ultcurmad: no DDI categories documented (just "99 sin información"),
      # keep numeric, matches homologate_vars()'s own ultcurmad treatment
      x_i <- mutate(x_i, across(any_of(c("ultcurmad")), \(x) {
        as.numeric(as.character(x)) |> na_if(99)
      }))

      # t_ges_agru_cie: gestation time re-grouped per CIE, only 2019 on
      x_i <- mutate(x_i, across(any_of(c("t_ges_agru_cie")), \(x) {
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("t_ges_agru_cie", 2024, ddi_defun_vars)
      }))

      # sexo (of the deceased) ##################################################
      x_i <- mutate(x_i, across(any_of(c("sexo")), \(x) {
        as.numeric(as.character(x)) |> encode_ddi_factor("sexo", 2014, ddi_defun_vars)
      }))

      # death circumstance vars, stable categories across years, single
      # reference year (2024, the most complete/recent) is enough ##############
      x_i <- mutate(x_i, across(any_of(c(
        "asis_med", "cons_exp", "mu_parto", "t_parto", "idprofcer"
      )), \(x) as.numeric(as.character(x))))
      x_i <- mutate(x_i, across(any_of(c("asis_med")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("asis_med", 2024, ddi_defun_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("cons_exp")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("cons_exp", 2024, ddi_defun_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("mu_parto")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("mu_parto", 2024, ddi_defun_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("t_parto")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("t_parto", 2024, ddi_defun_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("idprofcer")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("idprofcer", 2024, ddi_defun_vars)
      }))

      # tipo_emb: category granularity changed (old: simple/múltiple only;
      # new, >=2008: simple/doble/triple/cuádruple+), so an old/new split is
      # needed here too (a single reference year would mislabel old
      # "múltiple" records as, specifically, "doble")
      if (ano_i <= 2007) {
        x_i <- rename_if_present(x_i, "tipo_emb", "tipo_emb_old") |>
          mutate(across(any_of("tipo_emb_old"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("tipo_emb", 2000, ddi_defun_vars)
          }))
      } else {
        x_i <- rename_if_present(x_i, "tipo_emb", "tipo_emb_new") |>
          mutate(across(any_of("tipo_emb_new"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("tipo_emb", 2024, ddi_defun_vars)
          }))
      }

      # manner of death: pre-2019 (man_muer/pman_muer) and 2019+
      # (p_pman_iris) use genuinely different classification schemes (the
      # latter follows WHO's IRIS manner-of-death coding), so each is kept
      # as its own variable rather than forced into one homologated column.
      # TODO: build a proper crosswalk once/if the DANE/IRIS mapping is
      # available; until then these three are independent
      x_i <- mutate(x_i, across(any_of(c("man_muer", "pman_muer", "p_pman_iris")), \(x) {
        as.numeric(as.character(x))
      }))
      x_i <- mutate(x_i, across(any_of(c("man_muer")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("man_muer", 2018, ddi_defun_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("pman_muer")), \(x) {
        encode_ddi_factor(x, "pman_muer", 2018, ddi_defun_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("p_pman_iris")), \(x) {
        encode_ddi_factor(x, "p_pman_iris", 2024, ddi_defun_vars)
      }))

      # hora, minutos: come in as inconsistent types across years (numeric,
      # character, or hms/difftime, see check_vars.R fix) -- normalize to a
      # plain numeric hour-of-day / minute value rather than attempt to
      # factor-encode 24-60 raw levels
      x_i <- mutate(x_i, across(any_of(c("hora", "minutos")), \(x) {
        if (inherits(x, "difftime")) {
          as.numeric(x, units = "secs") / ifelse(cur_column() == "hora", 3600, 60)
        } else {
          suppressWarnings(as.numeric(as.character(x)))
        }
      }))

      # c_muerte, c_muerteb..g: how the cause of death was established;
      # the DDI only documents a single "1" category per field (each field
      # behaves like an independent yes/blank checkbox), so keep as
      # character rather than force a two-level factor
      x_i <- mutate(x_i, across(any_of(c(
        "c_muerte", "c_muerteb", "c_muertec", "c_muerted",
        "c_muertee", "c_muertef", "c_muerteg"
      )), as.character))

      # cause-of-death raw codes: TODO proper ICD-10 / "Lista 6-67" /
      # "Lista 105" homologation across periods needs the DANE/OPS
      # documentation to cross-reference codes; for now just keep them as
      # character so nothing is silently lost
      x_i <- mutate(x_i, across(any_of(c(
        "c_bas1", "c_ant1", "c_ant2", "c_ant3", "c_ant12", "c_ant22", "c_ant32",
        "c_dir1", "c_dir12", "c_mcm1", "c_pat1", "c_pat2",
        "cau_homol", "causa_666", "causa_667", "causa_mult"
      )), as.character))

      # free-text / large-cardinality fields, just cast to character,
      # same treatment as nac_raw's nom_inst/otro_sit ###########################
      x_i <- mutate(x_i, across(any_of(c("nom_inst", "otrsitiode")), as.character))

      x_i
    }))

  defun_homo
}
