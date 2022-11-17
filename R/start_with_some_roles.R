#' Start with a set of chosen roles
#'
#' Create an [xtibble()] object from _roles_.
#' @param roles A vector of strings. The set of roles.
#' @param idx_name A string, the column name for the index (default "idx").
#' @return An [xtibble()] with columns `idx` (period 31, unit 18) and `role`.
#' @export
#' @family start_with_
#' @examples
#' start_with_some_roles(c("M3", "m7"))
start_with_some_roles <- function(roles, idx_name = "idx"){
  xtbl <- get_roles()

  purrr::walk(roles, ~rlang::arg_match(.x, xtbl[["role"]]))

  xtbl |>
    dplyr::filter(.data$role %in% roles) |>
    dplyr::rename("{idx_name}" := "idx")
}
