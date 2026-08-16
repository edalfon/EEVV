#' Homologate variables of interest for Defunciones No Fetales
#'
#' Defunciones No Fetales carries every variable `homologate_vars_defun()`
#' already handles, plus a handful specific to this dataset (deceased's own
#' demographics -- since the deceased is not a fetus/newborn here -- and a
#' few pregnancy-context flags). This function applies the common
#' homologation first and then handles those extra variables. See
#' `homologate_vars_defun()` for the shared logic and its caveats.
#'
#' @inheritParams homologate_vars_defun
#' @param defun_nofetal_raw a data frame with defunciones no fetales data,
#' as returned by `ingest_eevv_defun_nofetal`
#' @param ddi_defun_nofetal_vars DDI metadata for defunciones no fetales, as
#' returned by `ingest_eevv_ddi("nofetal")`
#' @return a data frame with a similar structure as `defun_nofetal_raw`
#' @seealso `homologate_vars_defun`, `homologate_vars_defun_fetal`
#' @export
homologate_vars_defun_nofetal <- function(defun_nofetal_raw, ddi_defun_nofetal_vars, ddi_nac_vars) {
  # see homologate_vars_defun()'s own copy of this helper for why it's
  # needed: not every variable is guaranteed present every year here
  rename_if_present <- function(df, old, new) {
    if (old %in% names(df)) names(df)[names(df) == old] <- new
    df
  }

  defun_homo <- homologate_vars_defun(defun_nofetal_raw, ddi_defun_nofetal_vars, ddi_nac_vars)

  defun_homo |>
    mutate(sav_data = purrr::map(sav_data, function(x_i) {
      ano_i <- na.omit(unique(x_i$ano)) |> as.numeric()

      # est_civil (of the deceased): same old(<=2007)/new(>=2008) split as
      # est_civm, own DDI confirms the same category-scheme change
      if (ano_i >= 2008) {
        x_i <- mutate(x_i, across(any_of(c("est_civil")), \(x) {
          as.numeric(as.character(x)) |>
            na_if(9) |>
            encode_ddi_factor("est_civil", 2024, ddi_defun_nofetal_vars)
        }))
      } else {
        x_i <- rename_if_present(x_i, "est_civil", "est_civil_old") |>
          mutate(across(any_of("est_civil_old"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("est_civil", 2000, ddi_defun_nofetal_vars)
          }))
      }

      # nivel_edu (of the deceased): same old/new split as niv_edum
      if (ano_i <= 2007) {
        x_i <- rename_if_present(x_i, "nivel_edu", "nivel_edu_old") |>
          mutate(across(any_of("nivel_edu_old"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(9) |>
              encode_ddi_factor("nivel_edu", 2000, ddi_defun_nofetal_vars)
          }))
      } else {
        x_i <- rename_if_present(x_i, "nivel_edu", "nivel_edu_new") |>
          mutate(across(any_of("nivel_edu_new"), \(x) {
            as.numeric(as.character(x)) |>
              na_if(99) |>
              encode_ddi_factor("nivel_edu", 2024, ddi_defun_nofetal_vars)
          }))
      }

      # idpertet (ethnic self-recognition of the deceased): stable
      # categories throughout, mirrors idpertet in homologate_vars()
      x_i <- mutate(x_i, across(any_of(c("idpertet")), \(x) {
        as.numeric(as.character(x)) |>
          na_if(9) |>
          encode_ddi_factor("idpertet", 2024, ddi_defun_nofetal_vars)
      }))

      # occupation-related death flags, pregnancy-context flags: stable
      # Sí/No/Sin información style schemes
      x_i <- mutate(x_i, across(any_of(c(
        "muerteporo", "simuertepo", "emb_fal", "emb_mes", "emb_sem",
        "tipoformulario", "gru_ed2"
      )), \(x) as.numeric(as.character(x))))
      x_i <- mutate(x_i, across(any_of(c("muerteporo")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("muerteporo", 2024, ddi_defun_nofetal_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("simuertepo")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("simuertepo", 2024, ddi_defun_nofetal_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("emb_fal")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("emb_fal", 2024, ddi_defun_nofetal_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("emb_mes")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("emb_mes", 2024, ddi_defun_nofetal_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("emb_sem")), \(x) {
        na_if(x, 9) |> encode_ddi_factor("emb_sem", 2024, ddi_defun_nofetal_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("tipoformulario")), \(x) {
        encode_ddi_factor(x, "tipoformulario", 2024, ddi_defun_nofetal_vars)
      }))
      x_i <- mutate(x_i, across(any_of(c("gru_ed2")), \(x) {
        encode_ddi_factor(x, "gru_ed2", 2024, ddi_defun_nofetal_vars)
      }))

      # ultcurfal: no DDI categories, mirrors ultcurmad
      x_i <- mutate(x_i, across(any_of(c("ultcurfal")), \(x) {
        as.numeric(as.character(x)) |> na_if(99)
      }))

      # too large to homologate as categories (hundreds of raw codes), same
      # treatment as cod_munic/nom_inst: just cast to character
      x_i <- mutate(x_i, across(any_of(c(
        "ocupacion", "gru_ed1", "codpaisnacfal"
      )), as.character))

      x_i
    }))
}
