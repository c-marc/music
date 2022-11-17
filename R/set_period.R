#' Set the period to a new value
#'
#' @description
#' Change the period of an index.
#' The index must be ordered as fifths (as specified jointly by `period` and `unit`).
#'
#' If the new period is lower, there is a possibility of colliding values.
#' By default they will not be merged and the operation will abort.
#' In such a case, this usually means the data (notes and/or roles) need some attention.
#' Merging can be asked but will strip out any other column.
#'
#' If columns `note` and/or `role` are present, these richer values will be inherited by the new index.
#' This is a chosen feature that allows subtle roles definitions even when switching to 12-EDO.
#' This can be overwritten by calling [add_notes()] or [add_roles()] again: both will provide new labels.
#' @export
#' @param x An object.
#' @param to An integer. The period to set the index to (31, 19 or 12).
#' @param merge A logical. Default is FALSE.
#' @param ... Further arguments passed to or from other methods.
#' @returns A modified object.
#' @examples
#' index(0:1, period = 31L, unit = 18L) |> set_period(to = 12L)
#' start_with_some_roles(c("P1", "d7", "A5")) |> set_period(to = 12L)
#' start_with_some_roles(c("P1", "d7", "M6")) |> set_period(to = 12, merge = TRUE)
set_period <- function(x, ...){
  UseMethod("set_period", x)
}


#' @rdname set_period
#' @export
set_period.m_index <- function(x, to, merge = FALSE, ...){
  check_period(x)
  check_fifths(x)
  rlang::check_required(to)
  check_period(to)

  period_from <- period_attr(x)
  if (period_from <= to) {
    return(index(x, period = to, unit = fifth(to)))
  }

  x_new <- vec_data(x) %% to

  x_unique <- unique(x_new)
  n <- purrr::map_dbl(x_unique, ~sum(x_new == .x))
  if (any(n > 1)) {
    dup <- purrr::map(x_unique[which(n > 1)], ~vec_data(x)[x_new == .x])
    purrr::walk(dup, ~cli::cli_alert_warning("Duplicates: {.val {.x}}"))

    if (!merge) {
      cli::cli_abort(c(
        "Invalid index",
        "!" = "Cannot change {.field period} without merging.",
        "i" = "Values are probably unusual and should be checked."
      ))
    }
    x_new <- x_unique
  }

  index(x_new, period = to, unit = fifth(to))
}


#' @rdname set_period
#' @export
set_period.m_xtibble <- function(x, to, merge = FALSE, ...){
  rlang::check_required(to)

  idx_name <- idx_name_attr(x)

  idx_new <- set_period(x[[idx_name]], to = to, merge = merge)

  # Merge happened
  if (length(idx_new) < nrow(x)) {
    tbl <- tibble::tibble("{idx_name}" := idx_new)
    return(xtibble(tbl, idx_name = idx_name))
  }

  x[[idx_name]] <- idx_new

  sel <- switch(as.character(to),
                "19" = -9:9,
                "12" = -5:6) #do not duplicate here

  periodic_slice(x, sel)
}
