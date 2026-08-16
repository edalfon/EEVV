#' Ingest EEVV Nacimientos data
#'
#' This function ingests "Nacimientos" (births) data from DANE's EEVV
#' Data files have been downloaded and stored in the data/EEVV folder.
#' Yet, DANE does not allow you to make the data available to others
#' in a public repository. So you probably have to download it yourself.
#' It identifies relevant files, extracts data using `ingest_zipped_sav`,
#' and performs basic cleaning and processing.
#'
#' @return A data frame containing the births data. The data frame includes:
#'
#' * `bundle`: A character string identifying the file/year the data originated from.
#' * `sav_file`: Name of the sav file read (set daner::ingest_zipped_sav).
#' * `size`: Size of the sav file read (set daner::ingest_zipped_sav).
#' * `date`: Date of the sav file read (set daner::ingest_zipped_sav).
#' * `year`: Numeric year extracted from the file name.
#' * `sav_data`: Nested data frame, each representing data from one year.
#' Each data frame includes the data as is, meaning, it is just the
#' result of reading the data file via daner::ingest_zipped_sav, which
#' ultimately reads that using haven::read_sav. So, no further processing
#' is donde here, except for i) cleaning variable names via janitor::clean_names
#' and ii) adding a rowid within each dataframe, to make it easier to identify
#' individual observations and spot/debug errors down the line
#'
#' @seealso `daner::ingest_zipped_sav`
#'
#' @export
ingest_eevv_nac <- function() {
  # just to  take a look at all files. Do not read them all, though
  fs::dir_ls("data/EEVV", recurse = TRUE, regexp = ".*nac.*.zip", ignore.case = TRUE)

  # Rather manually make a decision which files should we include
  nac_zip_files <- c(
    `1999` = "data/EEVV/1998-2007/Nacimientos 1999.zip",
    `1998` = "data/EEVV/1998-2007/Nacimientos 1998.zip",
    `2000` = "data/EEVV/1998-2007/Nacimientos 2000.zip",
    `2001` = "data/EEVV/1998-2007/Nacimientos 2001.zip",
    `2002` = "data/EEVV/1998-2007/Nacimientos 2002.zip",
    `2003` = "data/EEVV/1998-2007/Nacimientos 2003.zip",
    `2004` = "data/EEVV/1998-2007/Nacimientos 2004.zip",
    `2005` = "data/EEVV/1998-2007/Nacimientos 2005.zip",
    `2006` = "data/EEVV/1998-2007/Nacimientos_2006.zip",
    `2007` = "data/EEVV/1998-2007/Nacimientos_2007.zip",
    `2008-2011` = "data/EEVV/2008-2011/Nac_2008_2011_SPSS.zip",
    `2012-2013` = "data/EEVV/2012-2013/Nacidos vivos.zip",
    `2014` = "data/EEVV/2014/Nac_2014.zip",
    `2015` = "data/EEVV/2015/Nac_2015.zip",
    `2016` = "data/EEVV/2016/Nacidos.zip",
    `2017` = "data/EEVV/2017-2018/nac2017.zip",
    `2018` = "data/EEVV/2017-2018/nac2018.zip",
    `2019` = "data/EEVV/2019/nac2019.zip", # manually unzipped zip of zip
    `2020` = "data/EEVV/2020/nac2020.zip",
    `2021` = "data/EEVV/2021/nacimientos2021.zip",
    `2022` = "data/EEVV/2022/nac2022.zip"
  )

  nac_raw <-
    purrr::map_dfr(nac_zip_files, daner::ingest_zipped_sav, .id = "bundle") |>
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

  nac_raw
}
