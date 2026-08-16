#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author edalfon
#' @export
plan_ingest_nac <- function() {
  #

  # just to  take a look at all files. Do not read them all, though
  fs::dir_ls(
    "data/EEVV",
    recurse = TRUE,
    regexp = ".*nac.*.zip",
    ignore.case = TRUE
  )

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

  files_df <- tibble::enframe(nac_zip_files, name = "name", value = "path")

  tar_map(
    values = files_df,
    names = "name",
    tar_target(file, path, format = "file"),
    tar_qs(data, daner::ingest_zipped_sav(file))
  )
}
