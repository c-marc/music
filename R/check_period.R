check_period <- function(x){
  UseMethod("check_period", x)
}


#' @export
check_period.default <- function(x){
  valid_period <- c(31, 19, 12)
  if (!x %in% valid_period) {
    cli::cli_abort(c(
      "Invalid period",
      "!" = "Valid {.arg period} are {.val valid_p}."
    ))
  }
}


#' @export
check_period.m_index <- function(x){
  period <- period_attr(x)
  check_period(period)
}


#' @export
check_period.m_xtibble <- function(x){
  idx_name <- idx_name_attr(x)
  check_period(x[[idx_name]])
}
