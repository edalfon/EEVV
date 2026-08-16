#' Ingest EEVV Defunciones No Fetales data
#'
#' This function ingests "Defunciones no fetales" (non-fetal, i.e. general
#' mortality) data from DANE's EEVV. Mirrors `ingest_eevv_nac()`, see that
#' function for details on the general approach (data location,
#' `daner::ingest_zipped_sav`, basic cleaning).
#'
#' @return A data frame containing the non-fetal deaths data, with the same
#' structure as returned by `ingest_eevv_nac`: one row per source file/year,
#' with a nested `sav_data` column holding the actual observations.
#'
#' @seealso `ingest_eevv_nac`, `ingest_eevv_defun_fetal`, `daner::ingest_zipped_sav`
#'
#' @export
ingest_eevv_defun_nofetal <- function() {
  fs::dir_ls("data/EEVV", recurse = TRUE, regexp = ".*defun.*.zip|.*nofetal.*.zip", ignore.case = TRUE)

  # Rather manually make a decision which files should we include
  defun_nofetal_zip_files <- c(
    `1998` = "data/EEVV/1998-2007/Defunciones 1998.zip",
    `1999` = "data/EEVV/1998-2007/Defunciones 1999.zip",
    `2000` = "data/EEVV/1998-2007/Defunciones  2000.zip",
    `2001` = "data/EEVV/1998-2007/Defunciones 2001.zip",
    `2002` = "data/EEVV/1998-2007/Defunciones 2002.zip",
    `2003` = "data/EEVV/1998-2007/Defunciones  2003.zip",
    `2004` = "data/EEVV/1998-2007/Defunciones  2004.zip",
    `2005` = "data/EEVV/1998-2007/Defunciones 2005.zip",
    `2006` = "data/EEVV/1998-2007/Defunciones  2006.zip",
    `2007` = "data/EEVV/1998-2007/Defunciones 2007.zip",
    `2008-2011` = "data/EEVV/2008-2011/Defun_2008_2011_SPSS.zip",
    `2012-2013` = "data/EEVV/2012-2013/Defunciones.zip",
    `2014` = "data/EEVV/2014/Defun_2014.zip",
    `2015` = "data/EEVV/2015/Defun_2015.zip",
    # 2016: only .txt/.csv/.dta available for this dataset, no .sav file
    # TODO: get hold of a .sav (or extend daner::ingest_zipped_sav to read .dta)
    `2017` = "data/EEVV/2017-2018/nofetal2017.zip",
    `2018` = "data/EEVV/2017-2018/nofetal2018.zip",
    `2019` = "data/EEVV/2019/nofetal2019.zip", # manually unzipped zip of zip
    `2020` = "data/EEVV/2020/nofetal2020.zip",
    `2021` = "data/EEVV/2021/nofetal2021.zip",
    `2022` = "data/EEVV/2022/nofetal2022.zip",
    `2023` = "data/EEVV/2023/BD-EEVV-Defuncionesnofetales-2023.zip",
    `2024` = "data/EEVV/2024/BD-EEVV-Defuncionesnofetales-2024.zip"
  )

  defun_nofetal_raw <-
    purrr::map_dfr(defun_nofetal_zip_files, daner::ingest_zipped_sav, .id = "bundle") |>
    dplyr::mutate(sav_data = purrr::map(sav_data, janitor::clean_names)) |>
    # we want to add a rowid, to make it easier to debug / unittest things
    dplyr::mutate(
      sav_data = purrr::map(sav_data, \(x) mutate(x, rowid = row_number()))
    ) |>
    dplyr::mutate(
      year = sav_file |>
        str_replace_all("\\D", "") |>
        str_sub(-4)
    )

  defun_nofetal_raw
}
