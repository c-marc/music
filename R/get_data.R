#' Get internally stored data
#'
#' Proxy function to access preotected data of the package.
#' @param name, A string, the name of the table.
#' If `NULL` (default), `get_data()` lists available names.
#' @returns
#' - If name is `NULL` or invalid, the names of available data.
#' - The required data.
#' @export
#' @examples
#' get_data()
#' get_data(get_data()[1])
get_data <- function(name = NULL) {
  xtbl_internal <- c(
    "roles",
    "modes_7",
    "modes_5",
    "melodic_minor",
    "harmonic_minor"
  )

  if (is.null(name)) {
    xtbl_internal
  } else {
    rlang::arg_match(name, values = xtbl_internal)
    stringr::str_glue("xtbl_{name}") |>
      rlang::parse_expr() |>
      rlang::eval_tidy()
  }
}
