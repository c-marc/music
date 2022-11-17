#' Get roles
#'
#' @description
#' Get roles from internal data and transform them:
#' - [index()] with `period = 31`, `unit = 18` (fifths);
#' - centered on `P1`.
#' @export
#' @return An [xtibble()] with 25 roles.
#' @examples
#' get_roles()
get_roles <- function(){
  tbl <- get_data("roles")

  tbl |>
    dplyr::select("idx", role = "name_abbr") |>
    dplyr::filter(.data$idx < 31L & !is.na(.data$role)) |>
    set_lock(to = FALSE, silently = TRUE) |>
    set_unit(to = 18L) |>
    periodic_slice(-15:15) |>
    set_lock(to = TRUE, silently = TRUE)
}
