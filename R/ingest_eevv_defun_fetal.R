#' Ingest EEVV Defunciones Fetales data
#'
#' This function ingests "Defunciones fetales" (fetal deaths) data from
#' DANE's EEVV. Mirrors `ingest_eevv_nac()`, see that function for details
#' on the general approach (data location, `daner::ingest_zipped_sav`, basic
#' cleaning).
#'
#' @return A data frame containing the fetal deaths data, with the same
#' structure as returned by `ingest_eevv_nac`: one row per source file/year,
#' with a nested `sav_data` column holding the actual observations.
#'
#' @seealso `ingest_eevv_nac`, `daner::ingest_zipped_sav`
#'
#' @export
ingest_eevv_defun_fetal <- function() {
  fs::dir_ls("data/EEVV", recurse = TRUE, regexp = ".*fetal.*.zip", ignore.case = TRUE)

  # Rather manually make a decision which files should we include
  defun_fetal_zip_files <- c(
    `1998` = "data/EEVV/1998-2007/fetal1998.zip",
    `1999` = "data/EEVV/1998-2007/fetal1999.zip",
    `2000` = "data/EEVV/1998-2007/fetal2000.zip",
    `2001` = "data/EEVV/1998-2007/fetal2001.zip",
    `2002` = "data/EEVV/1998-2007/fetal2002.zip",
    `2003` = "data/EEVV/1998-2007/fetal2003.zip",
    `2004` = "data/EEVV/1998-2007/fetal2004.zip",
    `2005` = "data/EEVV/1998-2007/fetal2005.zip",
    `2006` = "data/EEVV/1998-2007/fetal2006.zip",
    `2007` = "data/EEVV/1998-2007/fetal2007.zip",
    `2008` = "data/EEVV/2008-2011/fetal2008.zip",
    `2009` = "data/EEVV/2008-2011/fetal2009.zip",
    `2010` = "data/EEVV/2008-2011/fetal2010.zip",
    `2011` = "data/EEVV/2008-2011/fetal2011.zip",
    `2012` = "data/EEVV/2012-2013/fetal2012.zip",
    `2013` = "data/EEVV/2012-2013/fetal2013.zip",
    `2014` = "data/EEVV/2014/fetal_2014.zip",
    `2015` = "data/EEVV/2015/Fetal_2015.zip",
    # 2016: only .txt/.csv/.dta available for this dataset, no .sav file
    # TODO: get hold of a .sav (or extend daner::ingest_zipped_sav to read .dta)
    `2017` = "data/EEVV/2017-2018/fetal2017.zip",
    `2018` = "data/EEVV/2017-2018/fetal2018.zip",
    `2019` = "data/EEVV/2019/fetal2019.zip", # manually unzipped zip of zip
    `2020` = "data/EEVV/2020/fetal2020.zip",
    `2021` = "data/EEVV/2021/fetal2021.zip",
    `2022` = "data/EEVV/2022/fetal2022.zip",
    `2023` = "data/EEVV/2023/BD-EEVV-Defuncionesfetales-2023.zip",
    `2024` = "data/EEVV/2024/BD-EEVV-Defuncionesfetales-2024.zip"
  )

  defun_fetal_raw <-
    purrr::map_dfr(defun_fetal_zip_files, daner::ingest_zipped_sav, .id = "bundle") |>
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

  defun_fetal_raw
}
