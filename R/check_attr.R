check_attr <- function(x, ...){
  UseMethod("check_attr", x)
}


#' @export
check_attr.m_index <- function(x, period = NULL, unit = NULL){
  period_x <- period_attr(x)
  unit_x <- unit_attr(x)

  if (!is.null(period) && period_x != period) {
    cli::cli_abort(c(
      "Invalid object",
      "!" = "This operation requires an {.cls m_index} with {.field period} = {period}."
    ))
  }
  if (!is.null(unit) && unit_x != unit) {
    cli::cli_abort(c(
      "Invalid object",
      "!" = "This operation requires an {.cls m_index} with {.field unit} = {unit}."
    ))
  }

  invisible(x)
}


#' @export
check_attr.m_xtibble <- function(x, period = NULL, unit = NULL){
  idx_name <- idx_name_attr(x)
  check_attr(x[[idx_name]])

  invisible(x)
}
