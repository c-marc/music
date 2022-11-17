check_index <- function(x, arg_name = "It") {
  if (!is_index(x)) {
    cli::cli_abort(c(
      "Invalid object",
      "!" = "{.arg {arg_name}} must be an object of class {.cls m_index}."
    ))
  }
}


check_xtibble <- function(x, arg_name = "It") {
  if (!is_xtibble(x)) {
    cli::cli_abort(c(
      "Invalid object",
      "!" = "{.arg {arg_name}} must be an object of class {.cls m_xtibble}."
    ))
  }
}
