#' @noRd
#' @examples
#' funky <- function(a){check_integerish(a)}
#' try( funky("foo") )
#' is.integer( funky(1.0) )
check_integerish <- function(x){
  arg <- rlang::ensym(x)
  if (!rlang::is_integerish(x)) {
    cli::cli_abort(c(
            "Invalid type",
      "!" = "{.arg {arg}} must be an integer."
    ))
  }
  invisible(as.integer(x))
}


#' @noRd
#' @examples
#' funky <- function(a){check_scalar_integerish(a)}
#' try( funky("foo") )
#' is.integer( funky(1.0) )
check_scalar_integerish <- function(x){
  arg <- rlang::ensym(x)
  if (!rlang::is_scalar_integerish(x)) {
    cli::cli_abort(c(
      "Invalid type",
      "!" = "{.arg {arg}} must be a scalar integer."
    ))
  }
  invisible(as.integer(x))
}
