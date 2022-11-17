#' @export
#' @method vec_arith m_index
vec_arith.m_index <- function(op, x, y, ...) {
  UseMethod("vec_arith.m_index", y)
}


#' @export
#' @method vec_arith.m_index default
vec_arith.m_index.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}


#' @export
#' @method vec_arith.m_index integer
vec_arith.m_index.integer <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_index(vec_arith_base(op, x, y),
                    period = period_attr(x),
                    unit = unit_attr(x)),
    stop_incompatible_op(op, x, y)
  )
}


#' @export
#' @method vec_arith.numeric m_index
vec_arith.numeric.m_index <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_index(vec_arith_base(op, x, y),
                    period = period_attr(x),
                    unit = unit_attr(x)),
    stop_incompatible_op(op, x, y)
  )
}


#' @export
#' @method vec_arith.m_index m_index
vec_arith.m_index.m_index <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_index(vec_arith_base(op, x, y),
                    period = period_attr(x),
                    unit = unit_attr(x)),
    stop_incompatible_op(op, x, y)
  )
}
