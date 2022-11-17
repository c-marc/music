#' Break the index at a chosen value
#'
#' Choose a breaking point for a _circular_ index (when the range of values does not exceed the period).
#' Force numeration to a chosen pattern, by specifying the maximum positive value (next is negative).
#' @param x An object
#' @param ... further arguments passed to or from other methods.
#' @param at An integer. The breaking point.
#' @export
break_point <- function(x, ...){
  UseMethod("break_point", x)
}


#' @rdname break_point
#' @export
#' @examples
#' index(0:11, period = 12) |> break_point(at = 5L)
break_point.m_index <- function(x, at, ...){
  check_circular(x)

  rlang::check_required(at)
  at <- check_scalar_integerish(at)

  x_new <- vec_data(x)
  x_new <- x_new %% period_attr(x)
  x_new[x_new > at] <- x_new[x_new > at] - period_attr(x)

  new_index(x_new,
            period = period_attr(x),
            unit = unit_attr(x))
}


#' @rdname break_point
#' @export
#' @examples
#' example("xtibble") |> break_point(at = 1L)
break_point.m_xtibble <- function(x, at, ...){
  check_lock(x)
  rlang::check_required(at)

  idx_name <- idx_name_attr(x)

  x[[idx_name]] <- break_point(x[[idx_name]], at = at)
  x
}
