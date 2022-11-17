#' Test if the `m_xtibble` is locked
#' @param x An oject of class `m_xtibble`.
#' @return A logical
#' @export
is_locked <- function(x){
  check_xtibble(x)
  attr(x, "lock")
}


check_lock <- function(x){
  if (!is_locked(x)) {
    return(invisible(x))
  }

  cli::cli_abort(c(
    "Forbidden call",
    "!" = "Object {.cls m_xtibble} is locked.",
    "i" = "Unlock with {.code set_lock(x, to = FALSE)}."
  ))
}
