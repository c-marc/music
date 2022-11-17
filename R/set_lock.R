#' Lock or unlock an object of class `m_xtibble`
#' @param x An object of class `m_xtibble`.
#' @param to A logical to set the lock to (`TRUE` for _locked_).
#' @param silently A logical to use the fonction silently in non interactive context.
#' @return Modified `x`.
#' @export
#' @examples
#' example("xtibble") |> set_lock(to = TRUE) |> set_lock(to = FALSE)
set_lock <- function(x, to, silently = FALSE){
  check_xtibble(x)
  rlang::check_required(to)
  stopifnot(rlang::is_logical(to))

  if (is_locked(x) && to == FALSE && silently == FALSE) {
    cli::cli_inform(c(
      "!" = "{.cls m_xtibble} is now unlocked. Stay focused!"
    ))
  }
  if (!is_locked(x) && to == TRUE && silently == FALSE) {
    cli::cli_inform(c(
      "i" = "{.cls m_xtibble} is now locked."
    ))
  }

  attr(x, "lock") <- to
  x
}
