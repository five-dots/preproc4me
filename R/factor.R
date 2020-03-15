
#' Convert to dummy variables
#'
#' @param df A dafa.frame
#' @param formula A formula
#' @param one_hot A logical scalar
#' @param ordered A logical scalar
#' @param logical A logical scalar
#' @export
as_dummy <- function(df = NULL, formula = NULL, one_hot = FALSE,
                     ordered = FALSE, logical = FALSE) {

  a4me::a_class(df, "data.frame")
  a4me::a_class(formula, "formula")
  a4me::a_flag(one_hot)
  a4me::a_flag(ordered)
  a4me::a_flag(logical)

  if (logical) df <- dplyr::mutate_if(df, is.logical, as.factor)

  if (!ordered) {
    ord_df <- dplyr::select_if(df, is.ordered)
    ord_cols <- colnames(ord_df)
    rec <- recipes::recipe(x = df, formula = formula) %>%
      recipes::step_dummy(recipes::all_nominal(),
                          -recipes::all_outcomes(),
                          -ord_cols,
                          one_hot = one_hot)
  } else {
    rec <- recipes::recipe(x = df, formula = formula) %>%
      recipes::step_dummy(recipes::all_nominal(),
                          -recipes::all_outcomes(),
                          one_hot = one_hot)
  }

  rec %>%
    recipes::prep(strings_as_factors = FALSE) %>%
    recipes::juice()

}

#' Convert to ordinalscore
#'
#' @inheritParams as_dummy
#' @param convert A function
#' @export
as_ordinalscore <- function(df = NULL, formula = NULL, convert = as.numeric) {

  a4me::a_class(df, "data.frame")
  a4me::a_class(formula, "formula")
  a4me::a_class(convert, "function")

  ord_df <- dplyr::select_if(df, is.ordered)
  ord_cols <- colnames(ord_df)

  rec <- recipes::recipe(x = df, formula = formula) %>%
    recipes::step_ordinalscore(ord_cols,
                               -recipes::all_outcomes(),
                               convert = convert)

  rec %>%
    recipes::prep(strings_as_factors = FALSE) %>%
    recipes::juice()

}
