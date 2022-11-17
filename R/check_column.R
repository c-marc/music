check_column <- function(x, name) {
  if (!rlang::is_scalar_character(name)) {
    cli::cli_abort(c(
      "Invalid column",
      "!" = "The column name should be provided as a 'string'."
    ))
  }

  if (!name %in% rlang::names2(x)) {
    cli::cli_abort(c(
      "Invalid column",
      "i" = "The tibble has no column {.val {name}}."
    ))
  }
}
