
#' Convert to normalized
#'
#' @inheritParams as_dummy
#' @export
as_normalized <- function(df = NULL, formula = NULL) {

  rec <- recipes::recipe(x = df, formula = formula) %>%
    recipes::step_normalize(recipes::all_numeric(),
                            -recipes::all_outcomes())

  rec %>%
    recipes::prep(strings_as_factors = FALSE) %>%
    recipes::juice()

}
