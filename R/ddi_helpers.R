#' Set Variable Label
#'
#' This function sets the label attribute of a variable, retriving
#' the labels from the `ddi_nac_vars` using `var_name` and `year`
#'
#' @param x The variable to set the label for.
#' @param var_name The name of the variable.
#' @param year The year associated with the variable.
#' @param ddi_nac_vars A data frame containing DDI metadata as returned by
#' `ingest_eevv_ddi`.
#' @return The variable with the label attribute set.
#' @seealso `ingest_eevv_ddi`
#' @export
set_var_label <- function(x, var_name, year, ddi_nac_vars) {
  labl <- ddi_nac_vars |>
    filter(name == var_name, year == year) |>
    pull(labl) |>
    first()

  attr(x, "label") <- labl
  x
}

#' Encode a factor variable using DDI category values and labels
#'
#' This function takes a vector of values and encodes them as a factor using the
#' category values and labels from DDI metadata. It retrives the labels from
#' `ddi_nac_vars` for the respective `var_name` and `year`
#'
#' @param x Vector of values to be encoded as a factor
#' @param var_name Name of the variable to be encoded
#' @param year Year of the DDI metadata to use
#' @param ddi_nac_vars A data frame containing DDI metadata as returned by
#' `ingest_eevv_ddi`.
#' @return A factor vector with the values encoded using the DDI metadata
#' @export
encode_ddi_factor <- function(x, var_name, year, ddi_nac_vars) {
  catgry <- ddi_nac_vars |>
    filter(name == var_name, year == year) |>
    pull(catgry) |>
    first()

  # TODO: checks
  factor(
    x = x,
    levels = catgry$catValu,
    labels = catgry$labl
  )
}


#' Encode Factor from Haven Labelled Variable
#'
#' An old function using to encode factor, but using the labels
#' available in another variable/dataset encoded as a haven_labelled.
#' We used this, to encode a non-haven_labelled variable (e.g. from other year)
#' into a factor using the labels from a haven_labelled variable
#'
#' @param var_to_encode The variable to encode as a factor.
#' @param haven_labelled_var A Haven labelled variable from which to extract
#' levels and labels.
#'
#' @return A factor variable encoded from the Haven labelled variable.
encode_factor_from_haven_labelled_var <- function(var_to_encode, haven_labelled_var) {
  factor(
    x = var_to_encode,
    levels = attributes(haven_labelled_var)$labels,
    labels = str_trim(names(attributes(haven_labelled_var)$labels))
  )
}
