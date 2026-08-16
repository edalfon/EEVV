#' Ingest EEVV Nacimientos DDI information
#'
#' This function retrieves information about variables related to
#' births ("Nacimientos") from DANE's EEVV, using the DDI XML files.
#' Heavy lifting via `daner::list_ddi_files` and `daner::get_ddi_vars`
#'
#' But why doing this?, well, DANE's data do not always come with all
#' the variables encoding information. Although some of the .sav or .dta
#' files do have labels and values/categories, in several cases they do not
#' have that. Thus you would have to add them manually, or use some of the
#' metadata from other years. But you can also extract that metadata
#' from the DDI. In particular, we want the variable labels and the
#' categories encoding (value/label pairs).
#'
#' @return A data frame containing information about variables from
#' Nacimientos in DDI files. The data frame includes:
#'
#' * `xml`: Character vector indicating the source XML file for each variable.
#' * `id`: Character vector containing the variable ID extracted from the DDI document.
#' * `files`: Character vector with the data file ID associated in the DDI.
#' * `uri`: Character vector containing the URI from the DDI document
#'          (potentially useful for further exploration).
#' * `year`: Numeric year extracted from the URI or variable name
#' * `name`: Character vector containing the variable name in lowercase
#'           name and year are unique and hence the key to locate the metadata
#' * `intrvl`: some sort of data type
#' * `labl`: Character vector with the label associated to the variable
#' * `catgry`: Nested dataframe with the variable's categories.
#'             the dataframe has columns catValu and labl
#'
#' @seealso `daner::list_ddi_files`, `daner::get_ddi_vars`
#'
#' @export
ingest_eevv_ddi <- function() {
  # again, this only to take a look at the files, but let's
  # keep the list of xml files manually curated
  fs::dir_ls("data/EEVV", recurse = TRUE, regexp = "*.xml", ignore.case = TRUE)
  # daner::list_ddi_files("data/EEVV/1998-2007/DANE-DCD-EEVV-1998-2007.xml")
  # daner::get_ddi_vars("data/EEVV/1998-2007/DANE-DCD-EEVV-1998-2007.xml")

  xml_urls <- c(
    "data/EEVV/1998-2007/DANE-DCD-EEVV-1998-2007.xml",
    "data/EEVV/2008-2011/DANE-DCD-EEVV-2008-2011.xml",
    "data/EEVV/2012-2013/DANE-DCD-EEVV-2012-2013.xml",
    "data/EEVV/2014/DANE-DCD-EEVV-2014.xml",
    "data/EEVV/2015/DANE-DCD-EEVV-2015.xml",
    "data/EEVV/2016/DANE-DCD-EEVV-2016.xml",
    "data/EEVV/2017-2018/DANE-DCD-EEVV-2017-2018.xml",
    "data/EEVV/2019/DANE-DCD-EEVV-2019.xml",
    "data/EEVV/2020/DANE-DCD-EEVV-2020.xml",
    "data/EEVV/2021/COL-DANE-EEVV-2021.xml",
    "data/EEVV/2022/COL-DANE-EEVV-2022.xml",
    "data/EEVV/2023/COL-DANE-EEVV-2023.xml",
    "data/EEVV/2024/COL-DANE-EEVV-2024.xml"
  )
  xml_urls <- rlang::set_names(xml_urls, fs::path_file(xml_urls))

  ddi_nac_files <-
    purrr::map_dfr(xml_urls, daner::list_ddi_files, .id = "xml") |>
    # for the moment, we only want to keep the files related to nacimientos
    # and not those for mortalidad fetal o nofetal
    filter(str_detect(uri, regex("Name=nac", ignore_case = TRUE))) |>
    mutate(year = str_sub(uri, start = -4))

  testthat::expect_true(all(count(ddi_nac_files, id, xml)$n == 1))

  ddi_nac_vars <- purrr::map_dfr(xml_urls, daner::get_ddi_vars, .id = "xml") |>
    left_join(ddi_nac_files, by = c("files" = "id", "xml" = "xml")) |>
    # and we also want to keep for the moment, only the vars from nacimientos
    filter(!is.na(year)) |>
    mutate(name = str_to_lower(name))

  # variable name and year should be a unique key, and so will be used
  testthat::expect_true(all(count(ddi_nac_vars, name, year)$n == 1))

  ddi_nac_vars
}
