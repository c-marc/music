#' Compute the parameter `k` for the transformation of  the index
#'
#' This is used by [set_unit()] to perform the appropriate projection.
#' @noRd
#' @example
#' music:::next_param(19, 11, 1)
next_param <- function(period, unit_from = 1L, unit_to = 1L){
  check_gcd(period = period, unit = unit_from)

  k <- 1:(period - 1)
  #which((k * unit_to) %% period == unit_from)
  purrr::detect(k, ~(.x * unit_to) %% period == unit_from)
}


#' Set the `unit` of an index to a new value
#'
#' Transform the [index()] to a new unit.
#' @export
#' @param x
#' - A vector of integer.
#' - An object.
#' @param period An integer, the period of the index.
#' @param from An integer, the unit of the current index.
#' @param to An integer, the unit to the set the index to.
#' @param ... Further arguments passed to or from other methods.
#' @returns
#' - A vector of integer, the new values of the index.
#' - An object, with the new index.
set_unit <- function(x, ...){
  UseMethod("set_unit", x)
}


#' @rdname set_unit
#' @export
set_unit.default <- function(x, period, from, to, ...){
  k <- next_param(period = period, unit_from = from, unit_to = to)
  (x * k) %% period
}


#' @rdname set_unit
#' @export
set_unit.m_index <- function(x, to, ...){
  period <- period_attr(x)
  unit_from <- unit_attr(x)

  x_new <- set_unit(vec_data(x), period = period, from = unit_from, to = to)
  index(x_new, period = period, unit = to)
}


#' @rdname set_unit
#' @export
set_unit.m_xtibble <- function(x, to, ...){
  check_lock(x)

  idx_name <- idx_name_attr(x)

  x[[idx_name]] <- set_unit(x[[idx_name]], to = to)
  x
}
