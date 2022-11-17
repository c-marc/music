#' @noRd
#' @examples
#' try( check_gcd(12, 4) )
check_gcd <- function(period, unit){
  if (gcd(period, unit) > 1) {
    units <- 1:(period - 1)
    gcds <- purrr::map_int(units, ~gcd(period, .x))
    allowed <- units[gcds == 1]
    cli::cli_abort(c(
      "Invalid index",
      "!" = "{.val {unit}} is not a valid unit for {.arg period} {.val {period}}.",
      "i" = "Allowed units are {.val {allowed}}."
    ))
  }
}
