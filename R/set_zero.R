#' Set zero on a value of the index itself or from an alternative source
#'
#' Reset zero of the index on a specified value.
#' @param x An object.
#' @param ... further arguments passed to or from other methods.
#' @return The modified `x` object.
#' @export
set_zero <- function(x, ...){
  UseMethod("set_zero", x)
}


#' @rdname set_zero
#' @param on A scalar integer.
#' @export
#' @examples
#' example("index") |> set_zero(on = 1L)
set_zero.m_index <- function(x, on, ...){
  on <- check_scalar_integerish(on)
  x - on
}


#' @rdname set_zero
#' @description
#' The value `on` refers to:
#' - the index itself if `from` is `NULL` (default);
#' - a value in the column designated by `from` if `from` is supplied.
#' @param on
#' - An scalar integer if `from` is `NULL`.
#' - A scalar of the same type as `from`.
#' @param from A string, a column name.
#' @param direction, A string, either "forward" (default) or "backward".
#'   If `from` is supplied, direction used to search `on` if it is not unique.
#' @export
#' @examples
#' example("xtibble") |> set_zero(on = 2)
#' example("xtibble") |> set_zero(on = "x2", from = "name")
set_zero.m_xtibble <- function(x, on, from = NULL, direction = "forward", ...){
  check_lock(x)
  rlang::check_required(on)

  idx_name <- idx_name_attr(x)

  if (is.null(from)) on <- check_scalar_integerish(on)

  if (!is.null(from)) {
    check_column(x, name = from)

    # Cannot use typeof() because of unexpected integer/double
    c1 <- rlang::is_scalar_integerish(on) != rlang::is_integerish(x[[from]])
    c2 <- rlang::is_scalar_character(on) != rlang::is_character(x[[from]])
    if (c1 || c2) {
      cli::cli_abort(c(
        "Invalid type",
        "!" = "{.arg on} must have a compatible type with column {.arg from}.",
        "i" = "{.val {on}} has type {typeof(on)}.",
        "i" = "Column {.val {from}} has type {typeof(x[[from]])}."
      ))
    }

    pos <- purrr::detect_index(x[[from]], ~.x == on, .dir = direction)
    if (pos == 0) {
      cli::cli_abort(
        "Invalid value",
        "!" = "{.arg on} must be an existing value of {.arg from}.",
        "i" = "{.val {on}} cannot be found."
      )
    }
    on <- x[[idx_name]][pos]
  }

  x[[idx_name]] <- x[[idx_name]] - as.integer(on)
  x
}
