#' Helper to get the interval of the fifth in different ETT
#' @noRd
fifth <- function(period){
  check_scalar_integerish(period)
  check_period(period)

  switch(as.character(period),
         "7" = 4L,
         "12" = 7L,
         "19" = 11L,
         "31" = 18L)
}


#' Check if an index is ordered as fifths
#' @noRd
check_fifths <- function(x){
  UseMethod("check_fifths", x)
}


#' @export
check_fifths.m_index <- function(x){
  period <- period_attr(x)
  unit <- unit_attr(x)

  if (unit == fifth(period)) {
    return(invisible(x))
  }

  cli::cli_abort(c(
    "Invalid attributes",
    "!" = "{.cls m_index} must be ordered as fifths.",
    "!" = "`x` has {.field period} {.val {period}} and {.field unit} {.val {unit}}.",
    "i" = "Expected {.field unit} is {.val {fifth(period)}}."
  ))
}


#' @export
check_fifths.m_xtibble <- function(x){
  idx_name <- idx_name_attr(x)
  check_fifths(x[[idx_name]])
}
